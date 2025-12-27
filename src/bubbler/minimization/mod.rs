use crate::{
    colors::Implication,
    language::{Language, Rewrite},
};

mod basic_minimize;

pub use basic_minimize::{BasicImplicationMinimize, BasicRewriteMinimize};

#[derive(Clone, Debug)]
pub struct MinimizationConfig {}

pub type RewriteScoreFn<'a, L> = dyn Fn(&Rewrite<L>) -> i64 + Send + Sync + 'a;
pub type ImplicationScoreFn<'a, L> = dyn Fn(&Implication<L>) -> i64 + Send + Sync + 'a;

pub mod score_fns {
    use crate::{colors::Implication, language::Term};

    use super::*;

    pub mod implication_score_fns {
        use super::*;
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
                    crate::colors::Condition::Predicate(p) => &p.term,
                    crate::colors::Condition::Term(t) => t,
                };
                let lhs_cost = cost(lhs_term);
                let rhs_cost = cost(&imp.to.term);
                lhs_cost + rhs_cost
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
