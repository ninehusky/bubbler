//! Implications are connections between two propositions, where one proposition
//! implies another.

use egglog::ast::Expr;
use std::collections::HashMap;

use crate::language::{Language, Term, term::PredicateTerm};
use std::hash::Hash;

/// The left-hand side of an [`Implication`] can be either a predicate
/// or a term.
/// For example, to describe the fact that
/// `(abs x) -> (x >= 0)`,
/// you would have a `Condition::Term` for `(abs x)`.
/// To describe the fact that
/// `(x > 0) -> (abs x)`,
/// you would have a `Condition::Predicate` for `(x > 0)`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Condition<L: Language> {
    Predicate(PredicateTerm<L>),
    Term(Term<L>),
}

impl<L: Language> From<PredicateTerm<L>> for Condition<L> {
    fn from(p: PredicateTerm<L>) -> Self {
        Condition::Predicate(p)
    }
}

impl<L: Language> From<Term<L>> for Condition<L> {
    fn from(t: Term<L>) -> Self {
        Condition::Term(t)
    }
}

impl<L: Language> From<Condition<L>> for Expr {
    fn from(cond: Condition<L>) -> Self {
        match cond {
            Condition::Predicate(p) => p.into(),
            Condition::Term(t) => t.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Implication<L: Language> {
    pub from: Condition<L>,
    pub to: PredicateTerm<L>,
}

impl<L: Language> Implication<L> {
    pub fn new(from: Condition<L>, to: PredicateTerm<L>) -> Result<Self, String> {
        let mut map = HashMap::new();
        let from_generalized = match from.clone() {
            Condition::Predicate(p) => p
                .term
                .generalize(&mut map)
                .map_err(|e| format!("Failed to generalize LHS of implication: {}", e))?,
            Condition::Term(ref t) => t
                .generalize(&mut map)
                .map_err(|e| format!("Failed to generalize LHS of implication: {}", e))?,
        };
        let to_generalized = to
            .term
            .generalize(&mut map)
            .map_err(|e| format!("Failed to generalize RHS of implication: {}", e))?;
        Ok(Self {
            from: match from.clone() {
                Condition::Predicate(_) => {
                    Condition::Predicate(PredicateTerm::from_term(from_generalized))
                }
                Condition::Term(_) => Condition::Term(from_generalized),
            },
            to: PredicateTerm::from_term(to_generalized),
        })
    }

    pub fn lhs_concrete(&self) -> PredicateTerm<L> {
        match &self.from {
            Condition::Predicate(p) => {
                let lhs = p.term.concretize().unwrap();
                PredicateTerm { term: lhs }
            }
            Condition::Term(t) => {
                let lhs = t.concretize().unwrap();
                PredicateTerm { term: lhs }
            }
        }
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
            match &self.from {
                Condition::Predicate(p) => p.term.to_sexp(),
                Condition::Term(t) => t.to_sexp(),
            },
            self.to.term.to_sexp()
        )
    }
}
