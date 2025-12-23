//! Implications are connections between two propositions, where one proposition
//! implies another.

use crate::language::{term::PredicateTerm, Language};
use std::hash::Hash;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Implication<L: Language> {
    pub from: PredicateTerm<L>,
    pub to: PredicateTerm<L>,
}
