//! This is an internal module for interfacing with the egglog library.

use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
};

use egglog::{
    ast::{
        Expr, GenericAction, GenericActions, GenericCommand, GenericFact, GenericRewrite,
        GenericRule, GenericRunConfig, GenericSchedule, Variant,
    },
    call, lit,
    prelude::{add_relation, add_ruleset, RustSpan, Span},
    span, var, CommandOutput, EGraph,
};
use intern::InternStore;

use crate::language::{
    constant::BubbleConstant,
    rule::Rewrite,
    term::{PredicateTerm, Term},
    CVec, Language, OpTrait, PVec,
};

mod intern;

// A bunch of variables for storing names of relations/datatypes used in egglog programs.
// In accordance with the style of other egglog code, datatypes are PascalCased and
// relations/rulesets are kebab-cased.
pub(crate) mod bubbler_defns {
    // Rulesets
    pub const REWRITE_TERMS_RULESET: &str = "term-rewrites";
    pub const PROPAGATE_ANALYSIS_RULESET: &str = "analysis-propagation-ruleset";

    // Relations
    pub const HAS_CVEC_RELATION: &str = "has-cvec";
    pub const HAS_PVEC_RELATION: &str = "has-pvec";
    pub const COND_EQUAL_RELATION: &str = "cond-equal";

    // NOTE(@ninehusky): I'm commenting these out for now. We need these for rewrites
    // like "for all terms in the universe, do X", but I can't really think of
    // a reason why you'd want to do such a thing in general, let alone in
    // an e-graph. See #21 for more discussion.
    // pub const UNIVERSE_TERM_RELATION: &str = "universe-term";
    // pub const UNIVERSE_META_RELATION: &str = "universe-meta";

    // Datatypes
    pub const PREDICATE_DATATYPE: &str = "Predicate";

    // Other
    // This is the enum type for representing Base terms in the
    // meta-language's datatype. (See `[EgglogBackend::setup_egraph]`.)
    pub const BASE_TERM: &str = "BaseTerm";
}

type CVecStore<L> = InternStore<CVec<L>>;
type PVecStore = InternStore<PVec>;

pub struct EgglogBackend<L: Language> {
    egraph: EGraph,
    cvec_store: CVecStore<L>,
    pvec_store: PVecStore,
    _marker: std::marker::PhantomData<L>,
}

impl<L: Language> EgglogBackend<L> {
    pub fn new() -> Self {
        let egraph = Self::setup_egraph();
        Self {
            egraph,
            cvec_store: CVecStore::<L>::new(),
            pvec_store: PVecStore::new(),
            _marker: std::marker::PhantomData,
        }
    }

    pub fn setup_egraph() -> EGraph {
        let mut egraph = EGraph::default();
        // 1. Register the base language's syntax as a datatype.
        let mut variants: Vec<_> = L::ops()
            .iter()
            .map(|op| Variant {
                span: span!(),
                name: op.name().to_string(),
                // TODO: is it `op.arity() + 1`?
                types: { vec![L::name().to_string(); op.arity()] },
                cost: None,
                unextractable: false,
            })
            .collect();

        // const and var.
        let language_constant_id: String = {
            let bubble_constant: BubbleConstant = L::constant_to_bubble(
                &L::interesting_constants()
                    .into_iter()
                    .next()
                    .expect("Language must have at least one interesting constant."),
            );

            match bubble_constant {
                BubbleConstant::Int(_) => "i64".to_string(),
                BubbleConstant::Bool(_) => "bool".to_string(),
                BubbleConstant::String(_) => "String".to_string(),
            }
        };

        variants.push(Variant {
            span: span!(),
            name: "Const".to_string(),
            types: { vec![language_constant_id] },
            cost: None,
            unextractable: false,
        });

        variants.push(Variant {
            span: span!(),
            name: "Var".to_string(),
            types: { vec!["String".to_string()] },
            cost: None,
            unextractable: false,
        });

        egraph
            .run_program(vec![GenericCommand::Datatype {
                span: span!(),
                name: L::name().to_string(),
                variants,
            }])
            .map_err(|_| "Failed to register base language datatype.".to_string())
            .unwrap();

        // 2. Register the metalanguage's syntax as a datatype.
        // Here, the metalanguage can only be a `(Predicate e)` wrapper around
        // some term `e` in the base language. We can think about how to
        // augment this to be an arbitrary AST, but it seems unnecessary for now.
        let mut variants = vec![];

        variants.push(Variant {
            span: span!(),
            name: bubbler_defns::BASE_TERM.to_string(),
            types: vec![L::name().to_string()],
            cost: None,
            unextractable: false,
        });

        egraph
            .run_program(vec![GenericCommand::Datatype {
                span: span!(),
                name: bubbler_defns::PREDICATE_DATATYPE.to_string(),
                variants,
            }])
            .map_err(|e| format!("Failed to register metalanguage datatype: {:?}", e))
            .unwrap();

        // 3. Add rulesets and relations.
        add_ruleset(&mut egraph, bubbler_defns::REWRITE_TERMS_RULESET)
            .ok()
            .unwrap();

        add_relation(
            &mut egraph,
            bubbler_defns::HAS_CVEC_RELATION,
            vec![
                // (has-cvec term cvec-hash)
                L::name().to_string(),
                "String".to_string(),
            ],
        )
        .unwrap();

        add_relation(
            &mut egraph,
            bubbler_defns::HAS_PVEC_RELATION,
            vec![
                // (has-pvec predterm pvec)
                bubbler_defns::PREDICATE_DATATYPE.to_string(),
                "String".to_string(),
            ],
        )
        .unwrap();

        add_relation(
            &mut egraph,
            bubbler_defns::COND_EQUAL_RELATION,
            vec![
                // (cond-equal cond lhs rhs)
                bubbler_defns::PREDICATE_DATATYPE.to_string(),
                L::name().to_string(),
                L::name().to_string(),
            ],
        )
        .ok()
        .unwrap();

        egraph
    }

    /// Returns a mapping from CVecs to terms in the egraph with those CVecs.
    fn get_cvec_map(&mut self) -> HashMap<CVec<L>, Vec<Term<L>>> {
        let result = self
            .egraph
            .run_program(vec![GenericCommand::PrintFunction(
                span!(),
                bubbler_defns::HAS_CVEC_RELATION.to_string(),
                None,
                None,
                egglog::ast::PrintFunctionMode::Default,
            )])
            .unwrap();

        let CommandOutput::PrintFunction(_, termdag, terms_and_outputs, _) = &result[0] else {
            panic!("Expected PrintFunctionOutput.");
        };

        let mut res: HashMap<CVec<L>, Vec<Term<L>>> = HashMap::new();

        for (term, _) in terms_and_outputs {
            let expr: egglog::ast::Expr = termdag.term_to_expr(term, span!());
            match expr {
                Expr::Call(_, ref op, ref args) if op == &bubbler_defns::HAS_CVEC_RELATION => {
                    assert_eq!(args.len(), 2);
                    let term_expr: Term<L> = args[0].clone().into();
                    let cvec_hash: u64 =
                        if let Expr::Lit(_, egglog::ast::Literal::String(s)) = &args[1] {
                            s.parse::<u64>().unwrap()
                        } else {
                            panic!("Expected string literal for cvec hash.");
                        };
                    let Some(cvec) = self.cvec_store.get(cvec_hash) else {
                        panic!("CVec hash not found in store.");
                    };

                    res.entry(cvec.clone())
                        .or_insert_with(Vec::new)
                        .push(term_expr);
                }
                _ => panic!("Expected has-cvec call."),
            };
        }

        res
    }

    /// Returns a mapping from PVecs to terms in the egraph with those PVecs.
    fn get_pvec_map(&mut self) -> HashMap<PVec, Vec<PredicateTerm<L>>> {
        let result = self
            .egraph
            .run_program(vec![GenericCommand::PrintFunction(
                span!(),
                bubbler_defns::HAS_PVEC_RELATION.to_string(),
                None,
                None,
                egglog::ast::PrintFunctionMode::Default,
            )])
            .unwrap();

        let CommandOutput::PrintFunction(_, termdag, terms_and_outputs, _) = &result[0] else {
            panic!("Expected PrintFunctionOutput.");
        };

        let mut res: HashMap<PVec, Vec<PredicateTerm<L>>> = HashMap::new();

        for (term, _) in terms_and_outputs {
            let expr: egglog::ast::Expr = termdag.term_to_expr(term, span!());
            match expr {
                Expr::Call(_, ref op, ref args) if op == &bubbler_defns::HAS_PVEC_RELATION => {
                    assert_eq!(args.len(), 2);
                    let term: Term<L> = {
                        if let Expr::Call(_, base_term_op, base_term_args) = &args[0] {
                            assert_eq!(base_term_op, &bubbler_defns::BASE_TERM);
                            assert_eq!(base_term_args.len(), 1);
                            base_term_args[0].clone().into()
                        } else {
                            panic!("Expected BaseTerm call for predicate term.");
                        }
                    };

                    let pvec_hash: u64 =
                        if let Expr::Lit(_, egglog::ast::Literal::String(s)) = &args[1] {
                            s.parse::<u64>().unwrap()
                        } else {
                            panic!("Expected string literal for cvec hash.");
                        };
                    let Some(pvec) = self.pvec_store.get(pvec_hash) else {
                        panic!("PVec hash not found in store.");
                    };

                    res.entry(pvec.clone())
                        .or_insert_with(Vec::new)
                        .push(PredicateTerm::from_term(term));
                }
                _ => panic!("Expected has-pvec call."),
            };
        }

        res
    }

    // Adds the given unconditional rewrite rule to the Bubbler's set of rewrite rules.
    fn register_unconditional_rewrite(&mut self, rule: &Rewrite<L>) -> Result<(), String> {
        let Rewrite::Unconditional { lhs, rhs } = rule else {
            return Err("Expected unconditional rewrite.".to_string());
        };

        self.egraph
            .run_program(vec![GenericCommand::Rule {
                rule: GenericRule {
                    span: span!(),
                    // union the LHS and RHS when...
                    head: GenericActions(vec![GenericAction::Union(
                        span!(),
                        lhs.clone().into(),
                        rhs.clone().into(),
                    )]),
                    // ...we can see that the LHS is in the egraph.
                    body: vec![GenericFact::Fact(lhs.clone().into())],
                    name: format!("{}", rule),
                    ruleset: bubbler_defns::REWRITE_TERMS_RULESET.to_string(),
                },
            }])
            .map_err(|e| format!("Failed to register rewrite: {:?}", e).to_string())?;

        Ok(())
    }

    fn register_conditional_rewrite(&mut self, rule: &Rewrite<L>) -> Result<(), String> {
        let Rewrite::Conditional { cond, lhs, rhs } = rule else {
            return Err("Expected conditional rewrite.".to_string());
        };

        self.egraph
            .run_program(vec![GenericCommand::Rule {
                rule: GenericRule {
                    span: span!(),
                    head: GenericActions(vec![GenericAction::Expr(
                        span!(),
                        // mark the LHS and RHS as conditionally equal under cond if...
                        call!(
                            bubbler_defns::COND_EQUAL_RELATION.to_string(),
                            vec![
                                call!(
                                    bubbler_defns::BASE_TERM.to_string(),
                                    vec![cond.clone().into()]
                                ),
                                lhs.clone().into(),
                                rhs.clone().into()
                            ]
                        ),
                    )]),
                    // ...we can see that the LHS is in the egraph.
                    body: vec![GenericFact::Fact(lhs.clone().into())],
                    name: format!("{}", rule),
                    ruleset: bubbler_defns::REWRITE_TERMS_RULESET.to_string(),
                },
            }])
            .map_err(|e| format!("Failed to register rewrite: {:?}", e).to_string())?;

        Ok(())
    }

    pub fn register(&mut self, rule: &Rewrite<L>) -> Result<(), String> {
        // TODO: we may want to have register_set which takes a `RewriteSet<L>` and batch-adds them.
        // I really, really doubt that this will be a performance bottleneck, though. Also, such a
        // function is at odds with the one-by-one registration that's described in the Ruler/Chompy sphere of papers.
        match rule {
            Rewrite::Unconditional { .. } => self.register_unconditional_rewrite(rule),
            Rewrite::Conditional { .. } => self.register_conditional_rewrite(rule),
        }
    }

    pub fn add_term(&mut self, term: Term<L>, cvec: Option<CVec<L>>) -> Result<(), String> {
        let mut commands = vec![];

        commands.push(GenericCommand::Action(GenericAction::Expr(
            span!(),
            term.clone().into(),
        )));

        if let Some(cvec) = cvec {
            // 1. store the cvec in the cvec store.
            let hash = self.cvec_store.intern(cvec);

            // 2. add a fact to the egraph associating the term with its cvec hash.
            commands.push(GenericCommand::Action(GenericAction::Expr(
                span!(),
                call!(
                    bubbler_defns::HAS_CVEC_RELATION.to_string(),
                    // it's dumb, but if we have some u64 hash that isn't representable as
                    // an `i64`, egglog will complain (because u64s aren't supported literals in egglog).
                    // So we just store it as a string.
                    vec![term.clone().into(), lit!(hash.to_string())]
                ),
            )))
        }

        self.egraph
            .run_program(commands)
            .map_err(|e| format!("Failed to add term: {:?}", e))?;

        Ok(())
    }

    pub fn add_predicate(
        &mut self,
        predicate: PredicateTerm<L>,
        pvec: Option<PVec>,
    ) -> Result<(), String> {
        let mut commands = vec![];

        commands.push(GenericCommand::Action(GenericAction::Expr(
            span!(),
            call!(
                bubbler_defns::BASE_TERM.to_string(),
                vec![predicate.term.clone().into()]
            ),
        )));

        if let Some(pvec) = pvec {
            // 1. store the pvec in the pvec store.
            let hash = self.pvec_store.intern(pvec);

            // 2. add a fact to the egraph associating the term with its pvec hash.
            commands.push(GenericCommand::Action(GenericAction::Expr(
                span!(),
                call!(
                    bubbler_defns::HAS_PVEC_RELATION.to_string(),
                    // it's dumb, but if we have some u64 hash that isn't representable as
                    // an `i64`, egglog will complain (because u64s aren't supported literals in egglog).
                    // So we just store it as a string.
                    vec![
                        call!(
                            bubbler_defns::BASE_TERM.to_string(),
                            vec![predicate.term.clone().into()]
                        ),
                        lit!(hash.to_string())
                    ]
                ),
            )))
        }

        self.egraph
            .run_program(commands)
            .map_err(|e| format!("Failed to add predicate: {:?}", e))?;

        Ok(())
    }

    pub fn run_rewrites(&mut self) -> Result<(), String> {
        self.egraph
            .run_program(vec![GenericCommand::RunSchedule(GenericSchedule::Run(
                span!(),
                GenericRunConfig {
                    ruleset: bubbler_defns::REWRITE_TERMS_RULESET.to_string(),
                    until: None,
                },
            ))])
            .map_err(|e| format!("Failed to run rewrites: {:?}", e))?;
        Ok(())
    }
}

impl<L: Language> From<egglog::ast::Expr> for Term<L> {
    fn from(value: egglog::ast::Expr) -> Self {
        match value {
            egglog::ast::GenericExpr::Call(_, op, args) => match op.as_str() {
                "Var" => {
                    assert_eq!(args.len(), 1);
                    if let egglog::ast::GenericExpr::Lit(_, lit) = &args[0] {
                        if let egglog::ast::Literal::String(name) = lit {
                            if name.starts_with('?') {
                                Term::Hole(name.clone())
                            } else {
                                Term::Var(name.clone())
                            }
                        } else {
                            panic!("Expected string literal for Var name.");
                        }
                    } else {
                        panic!("Expected literal for Var name.");
                    }
                }
                "Const" => {
                    assert_eq!(args.len(), 1);
                    if let egglog::ast::GenericExpr::Lit(_, lit) = &args[0] {
                        let constant: BubbleConstant = lit.clone().into();
                        Term::Const(L::constant_from_bubble(constant).into())
                    } else {
                        panic!("Expected literal for Const value.");
                    }
                }
                other => {
                    let op = other.parse::<L::Op>().unwrap_or_else(|_| {
                        panic!("Failed to parse operator {} in term.", other);
                    });

                    let children: Vec<Term<L>> = args.into_iter().map(|a| a.into()).collect();
                    Term::Call(op, children)
                }
            },
            _ => {
                panic!("Expected call expression for term.");
            }
        }
    }
}

impl<L: Language> From<Term<L>> for egglog::ast::Expr {
    fn from(term: Term<L>) -> Self {
        match term {
            Term::Hole(name) => {
                assert!(name.starts_with('?'), "A hole must start with '?'.");
                var!(name)
            }
            Term::Var(name) => {
                call!("Var".to_string(), vec![lit!(name)])
            }
            Term::Const(c) => {
                // match on the type of a constant.
                let bubble = L::constant_to_bubble(&c);
                let literal: egglog::ast::Literal = bubble.into();
                call!("Const".to_string(), vec![lit!(literal)])
            }
            Term::Call(op, children) => {
                let child_exprs: Vec<Expr> =
                    children.into_iter().map(|c| c.clone().into()).collect();
                call!(op.name().to_string(), child_exprs)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use egglog::CommandOutput;

    use super::*;
    use crate::test_langs::llvm::{LLVMLang, LLVMLangOp};

    #[test]
    fn add_term_ok() {
        let mut backend: EgglogBackend<LLVMLang> = EgglogBackend::new();
        // This only works if the egraph has been properly set up.
        backend
            .add_term(
                Term::Call(
                    LLVMLangOp::Add,
                    vec![Term::Const(1.into()), Term::Const(2.into())],
                ),
                None,
            )
            .unwrap();
        let result = backend
            .egraph
            .parse_and_run_program(None, "(print-size)")
            .unwrap();

        assert_eq!(result.len(), 1);
        let CommandOutput::PrintAllFunctionsSize(sizes) = &result[0] else {
            panic!("Expected PrintAllFunctionsSize output.");
        };

        for (func_name, size) in sizes {
            match func_name.as_str() {
                "Add" => assert_eq!(*size, 1),
                "Const" => assert_eq!(*size, 2),
                _ => (),
            }
        }
    }

    #[test]
    fn add_total_rewrite_ok() {
        let mut backend: EgglogBackend<LLVMLang> = EgglogBackend::new();
        // observe that the rewrite contains _holes_ for metavariables.
        let rewrite = Rewrite::Unconditional {
            lhs: Term::Call(
                LLVMLangOp::Add,
                vec![Term::Const(0.into()), Term::Hole("?x".into())],
            ),
            rhs: Term::Hole("?x".into()),
        };

        backend.register(&rewrite).unwrap();

        // Add a term that matches the LHS of the rewrite.
        backend
            .add_term(
                Term::Call(
                    LLVMLangOp::Add,
                    vec![Term::Const(0.into()), Term::Var("y".into())],
                ),
                None,
            )
            .unwrap();

        backend.run_rewrites().unwrap();

        // Check that the egraph contains the RHS term, and that it is unioned with the LHS term.
        backend
            .egraph
            .parse_and_run_program(None, "(check (= (Var \"y\") (Add (Const 0) (Var \"y\"))))")
            .unwrap();
    }

    #[test]
    // This test is similar to `add_total_rewrite_ok`, but it rewrites just to ametavariable.
    // The opposite direction won't work; we can't have `?a ~> ?a + 0`. But such rewrites don't
    // win you much, I think.
    fn add_total_rewrite_match_any() {
        let mut backend: EgglogBackend<LLVMLang> = EgglogBackend::new();

        // ?a + 0 ~> ?a
        let rewrite = Rewrite::Unconditional {
            lhs: Term::Call(
                LLVMLangOp::Add,
                vec![Term::Hole("?a".into()), Term::Const(0.into())],
            ),
            rhs: Term::Hole("?a".into()),
        };

        backend.register(&rewrite).unwrap();

        // Add a term that matches the LHS of the rewrite.
        backend.add_term(
            Term::Call(
                LLVMLangOp::Add,
                vec![Term::Var("a".into()), Term::Const(0.into())],
            ),
            None,
        );

        backend.run_rewrites().unwrap();

        // Check that the egraph contains the RHS term, and that it is unioned with the LHS term.
        backend
            .egraph
            .parse_and_run_program(None, "(check (= (Var \"a\") (Add (Var \"a\") (Const 0))) )")
            .unwrap();
    }

    #[test]
    fn add_conditional_rewrite_ok() {
        let mut backend: EgglogBackend<LLVMLang> = EgglogBackend::new();
        // y / y ~> 1 if y != 0
        let rewrite = Rewrite::Conditional {
            cond: Term::Call(
                LLVMLangOp::Neq,
                vec![Term::Hole("?y".into()), Term::Const(0.into())],
            ),
            lhs: Term::Call(
                LLVMLangOp::Div,
                vec![Term::Hole("?y".into()), Term::Hole("?y".into())],
            ),
            rhs: Term::Const(1.into()),
        };

        backend.register(&rewrite).unwrap();

        // Add a term that matches the LHS of the rewrite.
        backend
            .add_term(
                Term::Call(
                    LLVMLangOp::Div,
                    vec![Term::Var("a".into()), Term::Var("a".into())],
                ),
                None,
            )
            .unwrap();

        // should fail initially
        assert!(matches!(
            backend
                .egraph
                .parse_and_run_program(None, "(check (= (Const 1) (Div (Var \"a\") (Var \"a\"))))",)
                .unwrap_err(),
            egglog::Error::CheckError(..)
        ));

        // ...then we run the rules,
        backend.run_rewrites().unwrap();

        // ...and after it should show that 1== (a / a) under the condition that a != 0.
        backend
            .egraph
            .parse_and_run_program(
                None,
                format!(
                    "(check ({} (BaseTerm (Neq (Var \"a\") (Const 0)))  (Div (Var \"a\") (Var \"a\")) (Const 1)))",
                    bubbler_defns::COND_EQUAL_RELATION
                )
                .as_str(),
            )
            .unwrap();
    }

    #[test]
    fn get_cvec_map_ok() {
        let mut backend: EgglogBackend<LLVMLang> = EgglogBackend::new();
        // Add a term with a CVec.
        let cvec: CVec<LLVMLang> = vec![Some(1), Some(2), None];

        let one_plus_two: Term<LLVMLang> = Term::Call(
            LLVMLangOp::Add,
            vec![Term::Const(1.into()), Term::Const(2.into())],
        );

        let two_plus_one: Term<LLVMLang> = Term::Call(
            LLVMLangOp::Add,
            vec![Term::Const(2.into()), Term::Const(1.into())],
        );

        backend
            .add_term(one_plus_two.clone(), Some(cvec.clone()))
            .unwrap();

        backend
            .add_term(two_plus_one.clone(), Some(cvec.clone()))
            .unwrap();

        let cvec_map = backend.get_cvec_map();
        assert_eq!(cvec_map.len(), 1);
        let terms = cvec_map.get(&cvec).unwrap();
        assert_eq!(terms.len(), 2);
        assert!(terms.contains(&one_plus_two));
        assert!(terms.contains(&two_plus_one));
    }

    #[test]
    fn get_pvec_map_ok() {
        let mut backend: EgglogBackend<LLVMLang> = EgglogBackend::new();
        // Add a predicate with a PVec.
        let pvec: PVec = vec![true, false, true];

        let predicate_a: PredicateTerm<LLVMLang> = PredicateTerm::from_term(Term::Call(
            LLVMLangOp::Lt,
            vec![Term::Var("x".into()), Term::Const(10.into())],
        ));

        let predicate_b: PredicateTerm<LLVMLang> = PredicateTerm::from_term(Term::Call(
            LLVMLangOp::Gt,
            vec![Term::Var("y".into()), Term::Const(5.into())],
        ));

        backend
            .add_predicate(predicate_a.clone(), Some(pvec.clone()))
            .unwrap();

        backend
            .add_predicate(predicate_b.clone(), Some(pvec.clone()))
            .unwrap();

        let pvec_map = backend.get_pvec_map();
        assert_eq!(pvec_map.len(), 1);
        let terms = pvec_map.get(&pvec).unwrap();
        assert_eq!(terms.len(), 2);
        assert!(terms.contains(&predicate_a));
        assert!(terms.contains(&predicate_b));
    }
}
