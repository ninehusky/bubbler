use crate::language::{Language, Rewrite};

mod basic_minimize;

/// Describes the kind fact that is being identified.
#[derive(Clone, Debug)]
pub struct MinimizationConfig {}

pub mod score_fns {
    use crate::language::Term;

    use super::*;

    pub type ScoreFn<L> = dyn Fn(&Rewrite<L>) -> i64 + Send + Sync;

    pub fn prioritize_vars<L: Language>() -> Box<ScoreFn<L>> {
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

    pub fn ast_size<L: Language>() -> Box<ScoreFn<L>> {
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
