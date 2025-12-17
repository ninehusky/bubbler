//! This is an internal module for interfacing with the egglog library.

use egglog::{
    ast::{
        Expr, GenericAction, GenericActions, GenericCommand, GenericFact, GenericRewrite,
        GenericRule, Variant,
    },
    call, lit,
    prelude::{add_ruleset, RustSpan, Span},
    span, var, EGraph,
};

use crate::language::{analysis::MetaLanguage, rule::Rewrite, term::Term, Language, OpTrait};

// A bunch of variables for storing names of relations/datatypes used in egglog programs.
pub(crate) mod bubbler_defns {
    // Rulesets
    pub const REWRITE_TERMS_RULESET: &str = "term-rewrites";
    pub const PROPAGATE_ANALYSIS_RULESET: &str = "analysis-propagation-ruleset";

    // Relations
    pub const COND_EQUAL_RELATION: &str = "cond-equal";
    pub const UNIVERSE_TERM_RELATION: &str = "universe-term";
    pub const UNIVERSE_META_RELATION: &str = "universe-meta";

    // Datatypes
    pub const PREDICATE_DATATYPE: &str = "Predicate";

    // Other
    // This is the enum type for representing Base terms in the
    // meta-language's datatype. (See `[EgglogBackend::setup_egraph]`.)
    pub const BASE_TERM: &str = "BaseTerm";
}

pub struct EgglogBackend<L: MetaLanguage> {
    egraph: EGraph,
    _lang: std::marker::PhantomData<L>,
}

impl<L: MetaLanguage> EgglogBackend<L> {
    pub fn new() -> Self {
        let mut egraph = EGraph::default();
        add_ruleset(&mut egraph, bubbler_defns::REWRITE_TERMS_RULESET)
            .ok()
            .unwrap();
        Self {
            egraph,
            _lang: std::marker::PhantomData,
        }
    }

    fn setup_egraph(&mut self) -> Result<(), String> {
        // 1. Register the base language's syntax as a datatype.
        let variants: Vec<_> = L::Base::ops()
            .iter()
            .map(|op| Variant {
                span: span!(),
                name: op.name().to_string(),
                // TODO: is it `op.arity() + 1`?
                types: { vec![L::Base::name().to_string(); op.arity()] },
                cost: None,
                unextractable: false,
            })
            .collect();

        self.egraph
            .run_program(vec![GenericCommand::Datatype {
                span: span!(),
                name: L::Base::name().to_string(),
                variants,
            }])
            .map_err(|_| "Failed to register base language datatype.".to_string())?;

        // 2. Register the metalanguage's syntax as a datatype.
        let mut variants: Vec<_> = L::meta_ops()
            .iter()
            .map(|op| Variant {
                span: span!(),
                name: op.name().to_string(),
                types: { vec![L::name().to_string(); op.arity()] },
                cost: None,
                unextractable: false,
            })
            .collect();

        // Here, we special-case the regular language's terms here too:
        // `(datatype MetaLanguage ... (BaseTerm L::name()))`
        variants.push(Variant {
            span: span!(),
            name: bubbler_defns::BASE_TERM.to_string(),
            types: vec![L::Base::name().to_string()],
            cost: None,
            unextractable: false,
        });

        self.egraph
            .run_program(vec![GenericCommand::Datatype {
                span: span!(),
                name: L::name().to_string(),
                variants,
            }])
            .map_err(|_| "Failed to register metalanguage datatype.".to_string())?;

        Ok(())
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
                    head: GenericActions(vec![GenericAction::Union(
                        span!(),
                        lhs.clone().into(),
                        rhs.clone().into(),
                    )]),
                    body: vec![GenericFact::Fact(lhs.clone().into())],
                    name: format!("{}", rule),
                    ruleset: bubbler_defns::REWRITE_TERMS_RULESET.to_string(),
                },
            }])
            .map_or(Err("Failed to register rewrite.".to_string()), |_| Ok(()))
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
                        call!(
                            bubbler_defns::COND_EQUAL_RELATION.to_string(),
                            vec![cond.clone().into(), lhs.clone().into(), rhs.clone().into()]
                        ),
                    )]),
                    body: vec![GenericFact::Fact(lhs.clone().into())],
                    name: format!("{}", rule),
                    ruleset: bubbler_defns::REWRITE_TERMS_RULESET.to_string(),
                },
            }])
            .map_or(Err("Failed to register rewrite.".to_string()), |_| Ok(()))
    }

    // TODO: we may want to have register_set which takes a `RewriteSet<L>` and batch-adds them.
    // I really, really doubt that this will be a performance bottleneck, though. Also, such a
    // function is at odds with the one-by-one registration that's described in the Ruler/Chompy sphere of papers.
    pub fn register(&mut self, rule: &Rewrite<L>) -> Result<(), String> {
        match rule {
            Rewrite::Unconditional { .. } => self.register_unconditional_rewrite(rule),
            Rewrite::Conditional { .. } => self.register_conditional_rewrite(rule),
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

#[cfg(test)]
mod tests {
    use super::*;
}
