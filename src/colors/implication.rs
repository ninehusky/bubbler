//! Implications are connections between two propositions, where one proposition
//! implies another.

use egglog::ast::Expr;
use std::collections::HashMap;

use crate::language::{term::PredicateTerm, Language};
use std::hash::Hash;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Implication<L: Language> {
    pub from: PredicateTerm<L>,
    pub to: PredicateTerm<L>,
}

impl<L: Language> Implication<L> {
    pub fn new(from: PredicateTerm<L>, to: PredicateTerm<L>) -> Result<Self, String> {
        let mut map = HashMap::new();
        let from_generalized = from
            .term
            .generalize(&mut map)
            .map_err(|e| format!("Failed to generalize LHS of implication: {}", e))?;
        let to_generalized = to
            .term
            .generalize(&mut map)
            .map_err(|e| format!("Failed to generalize RHS of implication: {}", e))?;
        Ok(Self {
            from: PredicateTerm::from_term(from_generalized),
            to: PredicateTerm::from_term(to_generalized),
        })
    }

    pub fn lhs_concrete(&self) -> PredicateTerm<L> {
        let lhs = self.from.term.concretize().unwrap();
        PredicateTerm { term: lhs }
    }

    pub fn rhs_concrete(&self) -> PredicateTerm<L> {
        let rhs = self.to.term.concretize().unwrap();
        PredicateTerm { term: rhs }
    }
}

impl<L: Language> std::fmt::Display for Implication<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} --> {}",
            self.from.term.to_sexp(),
            self.to.term.to_sexp()
        )
    }
}
