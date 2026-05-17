use crate::{
    bubbler::{Condition, Implication},
    language::{Language, OpTrait, Rewrite, Term},
};

mod basic_minimize;

pub use basic_minimize::{BasicImplicationMinimize, BasicRewriteMinimize};

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct MinimizationConfig {}

pub type RewriteScoreFn<'a, L> = dyn Fn(&Rewrite<L>) -> i64 + Send + Sync + 'a;
pub type ImplicationScoreFn<'a, L> = dyn Fn(&Implication<L>) -> i64 + Send + Sync + 'a;

pub mod score_fns {

    use super::*;

    pub mod implication_score_fns {
        use super::*;

        /// Score by PVec-based subsumption order: weak antecedent + strong consequent first.
        /// Higher score = more general antecedent (many true slots) AND more informative
        /// consequent (few true slots) = should be registered first so it subsumes weaker
        /// variants. Falls back to AST size for implications without PVec counts.
        pub fn pvec_strength<'a, L: Language>() -> Box<ImplicationScoreFn<'a, L>> {
            Box::new(|imp: &Implication<L>| {
                let base = if imp.pvec_ante_count > 0 || imp.pvec_cons_count > 0 {
                    imp.pvec_ante_count as i64 - imp.pvec_cons_count as i64
                } else {
                    0
                };
                // Heavily penalize And-headed consequents (component-wise weakenings
                // like And(P,Q)→And(P',Q)). The and-intro axiom will derive these
                // once individual component implications are registered, so they
                // should be processed last — they'll be pruned from candidates by
                // the retain step before ever being popped into chosen.
                let and_penalty: i64 = match &imp.to.term {
                    Term::Call(op, _) if op.is_conjunction() => -1_000_000,
                    _ => 0,
                };
                base + and_penalty
            })
        }

        pub fn prioritize_vars<'a, L: Language>() -> Box<ImplicationScoreFn<'a, L>> {
            fn cost<L: Language>(term: &Term<L>) -> i64 {
                match term {
                    // penalty for constants
                    Term::Const { .. } => 2,
                    Term::Var { .. } => 1,
                    Term::Hole { .. } => 1,
                    Term::Call(_, children) => {
                        let child_costs: i64 = children.iter().map(|c| cost(c)).sum();
                        1 + child_costs
                    }
                }
            }
            Box::new(|imp: &Implication<L>| {
                let lhs_term = match &imp.from {
                    Condition::Predicate(p) => &p.term,
                    Condition::Term(t) => t,
                };
                cost(lhs_term) + cost(&imp.to.term)
            })
        }
    }

    pub mod rewrite_score_fns {
        use super::*;

        pub fn prioritize_vars<'a, L: Language>() -> Box<RewriteScoreFn<'a, L>> {
            fn cost<L: Language>(term: &Term<L>) -> i64 {
                match term {
                    // penalty for constants
                    Term::Const { .. } => 2,
                    Term::Var { .. } => 1,
                    Term::Hole { .. } => 1,
                    Term::Call(_, children) => {
                        let child_costs: i64 = children.iter().map(|c| cost(c)).sum();
                        1 + child_costs
                    }
                }
            }

            Box::new(|rw: &Rewrite<L>| {
                let cond_cost = match rw {
                    Rewrite::Conditional { cond, .. } => cost(&cond.term),
                    Rewrite::Unconditional { .. } => 0,
                };
                let lhs_cost = cost(&rw.lhs_concrete());
                let rhs_cost = cost(&rw.rhs_concrete());
                cond_cost + lhs_cost + rhs_cost
            })
        }

        #[allow(dead_code)]
        pub fn ast_size<'a, L: Language>() -> Box<RewriteScoreFn<'a, L>> {
            Box::new(|rw: &Rewrite<L>| {
                let cond_size = match rw {
                    Rewrite::Conditional { cond, .. } => cond.term.size(),
                    Rewrite::Unconditional { .. } => 0,
                };
                let lhs_size = rw.lhs_concrete().size();
                let rhs_size = rw.rhs_concrete().size();
                (cond_size + lhs_size + rhs_size) as i64
            })
        }
    }
}
