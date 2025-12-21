//! Implications are connections between two propositions, where one proposition
//! implies another.

use crate::language::{Language, term::PredicateTerm};

#[derive(Clone, PartialEq, Eq)]
pub struct Implication<L: Language> {
    pub from: PredicateTerm<L>,
    pub to: PredicateTerm<L>,
}
