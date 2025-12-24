use crate::{
    colors::Implication,
    language::{Language, Rewrite},
};

mod basic_minimize;

#[derive(Clone, Debug)]
pub struct MinimizationConfig {}

pub type RewriteScoreFn<L> = dyn Fn(&Rewrite<L>) -> i64 + Send + Sync;
pub type ImplicationScoreFn<L> = dyn Fn(&Implication<L>) -> i64 + Send + Sync;

pub mod score_fns {
    use crate::{colors::Implication, language::Term};

    use super::*;

    pub mod implication_score_fns {
        use super::*;
        pub fn prioritize_vars<L: Language>() -> Box<ImplicationScoreFn<L>> {
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
                let lhs_cost = cost(&imp.lhs_concrete().term);
                let rhs_cost = cost(&imp.rhs_concrete().term);
                lhs_cost + rhs_cost
            })
        }
    }

    pub mod rewrite_score_fns {
        use super::*;

        pub fn prioritize_vars<L: Language>() -> Box<RewriteScoreFn<L>> {
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
                    Rewrite::Conditional { cond, .. } => cost(cond),
                    Rewrite::Unconditional { .. } => 0,
                };
                let lhs_cost = cost(&rw.lhs_concrete());
                let rhs_cost = cost(&rw.rhs_concrete());
                cond_cost + lhs_cost + rhs_cost
            })
        }

        pub fn ast_size<L: Language>() -> Box<RewriteScoreFn<L>> {
            Box::new(|rw: &Rewrite<L>| {
                let cond_size = match rw {
                    Rewrite::Conditional { cond, .. } => cond.size(),
                    Rewrite::Unconditional { .. } => 0,
                };
                let lhs_size = rw.lhs_concrete().size();
                let rhs_size = rw.rhs_concrete().size();
                (cond_size + lhs_size + rhs_size) as i64
            })
        }
    }
}
