//! This is an internal module for interfacing with the egglog library.

use egglog::{
    ast::{Expr, GenericCommand, GenericFact, GenericRewrite},
    call, lit,
    prelude::{add_ruleset, RustSpan, Span},
    span, var, EGraph,
};

use crate::language::{rule::Rewrite, term::Term, Language, OpTrait};

// A bunch of variables for storing names of relations/datatypes used in egglog programs.
pub(crate) const TERM_RULESET_NAME: &str = "TermRewrites";

pub struct EgglogBackend<L: Language> {
    egraph: EGraph,
    _lang: std::marker::PhantomData<L>,
}

impl<L: Language> EgglogBackend<L> {
    pub fn new() -> Self {
        let mut egraph = EGraph::default();
        add_ruleset(&mut egraph, TERM_RULESET_NAME).ok().unwrap();
        Self {
            egraph,
            _lang: std::marker::PhantomData,
        }
    }

    // Adds the given unconditional rewrite rule to the Bubbler's set of rewrite rules.
    fn register_unconditional_rewrite(&mut self, rule: &<L>) -> Result<(), String> {
        let lhs: Expr = rule.lhs.clone().into();
        let rhs: Expr = rule.rhs.clone().into();

        let rw_prog = format!(
            r#"
            (rule
                (({UNIVERSAL_TERM_RELATION} {lhs}))
                ((union {lhs} {rhs})
                 ({UNIVERSAL_TERM_RELATION} {rhs}))
                :ruleset {REWRITE_RULESET})
            "#
        );
        run_prog!(self.egraph, &rw_prog)?;
        Ok(())
    }

    /// Adds the given rule to the Bubbler's set of rewrite rules.
    /// Errors if the rule already exists.
    // TODO: we may want to have register_set which takes a `RewriteSet<L>` and batch-adds them.
    // I really, really doubt that this will be a performance bottleneck, though. Also, such a
    // function is at odds with the one-by-one registration that's described in the Ruler/Chompy sphere of papers.
    pub fn register(&mut self, rule: &Rewrite<L>) -> Result<(), String> {
        match rule.cond {
            Some(_) => {
                return Err(
                    "Conditional rewrites are not yet supported in the Egglog backend.".to_string(),
                )
            }
            None => {}
        }

        let lhs: Expr = rule.lhs.clone().into();
        let rhs: Expr = rule.rhs.clone().into();
        // if the rule is a None, then empty vector.
        // else then map it to a genericfact.
        let conditions: Vec<GenericFact<String, String>> = match rule.cond {
            Some(ref c) => {
                let c: Expr = c.clone().into();
                vec![GenericFact {
                    span: span!(),
                    head: format!("{UNIVERSAL_PREDICATE_RELATION}"),
                    args: vec![format!("PredTerm {}", c)],
                }]
            }
            None => vec![],
        };

        assert!(
            rule.cond.is_none(),
            "I'm not ready for conditional rewrites yet. Blurb."
        );

        let rw: GenericRewrite<String, String> = GenericRewrite {
            span: span!(),
            lhs,
            rhs,
        };

        self.egraph
            .run_program(vec![GenericCommand::Rewrite("blorb".to_string())]);

        let rw_prog = match rule.cond {
            Some(ref c) => {
                let c: Expr = c.clone().into();
                format!(
                    r#"
                (rule
                    (({UNIVERSAL_PREDICATE_RELATION} (PredTerm {c}))
                     ({UNIVERSAL_TERM_RELATION} {lhs}))
                    (({COND_EQUAL_FN} (PredTerm {c}) {lhs} {rhs})
                     ({UNIVERSAL_TERM_RELATION} {rhs}))
                    :ruleset {REWRITE_RULESET})
                "#
                )
            }
            None => {
                format!(
                    r#"
                (rule
                    (({UNIVERSAL_TERM_RELATION} {lhs}))
                    ((union {lhs} {rhs})
                     ({UNIVERSAL_TERM_RELATION} {rhs}))
                    :ruleset {REWRITE_RULESET})
                "#
                )
            }
        };
        run_prog!(self.egraph, &rw_prog)?;
        self.rules.push(rule.clone());
        Ok(())
    }
}

impl<L: Language> From<Term<L>> for egglog::ast::Expr {
    fn from(term: Term<L>) -> Self {
        match term {
            Term::Hole(name) => {
                assert!(name.starts_with('?'), "A hole must start with '?'.");
                var!(name)
            }
            Term::Var(name) => var!(name),
            Term::Const(c) => {
                // match on the type of a constant.
                let c: egglog::ast::Literal = c.into().into();
                lit!(c)
            }
            Term::Call(op, children) => {
                let child_exprs: Vec<Expr> =
                    children.into_iter().map(|c| c.clone().into()).collect();
                call!(op.name().to_string(), child_exprs)
            }
        }
    }
}
