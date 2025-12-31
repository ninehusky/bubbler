//! A cvec (characteristic vector) is a vector that represents
//! the evaluation of a term over a set of environments.
//! It is used to identify potential rewrite opportunities.

use std::hash::Hash;
use std::ops::Deref;

use crate::language::BubbleConstant;
use crate::language::Language;

use crate::language::vectors::PVec;
use crate::language::vectors::SemVec;

/// A characteristic vector: observed values of a term
/// under the fuzzing environment.
///
/// Semantics:
/// - `Some(c)` means the term evaluated successfully to `c`
/// - `None` means evaluation failed / was undefined
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CVec<L: Language>(pub SemVec<Option<L::Constant>>);

impl<L: Language> Hash for CVec<L> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<L: Language> Deref for CVec<L> {
    type Target = SemVec<Option<L::Constant>>;
    fn deref(&self) -> &SemVec<Option<L::Constant>> {
        &self.0
    }
}

impl<'a, L: Language> IntoIterator for &'a CVec<L> {
    type Item = &'a Option<L::Constant>;
    type IntoIter = std::slice::Iter<'a, Option<L::Constant>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<L: Language> FromIterator<Option<L::Constant>> for CVec<L> {
    fn from_iter<I: IntoIterator<Item = Option<L::Constant>>>(iter: I) -> Self {
        CVec(iter.into_iter().collect())
    }
}

impl<L: Language> CVec<L> {
    /// Interpret this CVec as a predicate vector by
    /// mapping constants to booleans.
    ///
    /// Semantics:
    /// - `Some(c)` => `c.to_bool()`
    /// - `None`    => `false`
    pub fn to_pvec(&self) -> PVec {
        self.0
            .iter()
            .map(|opt| {
                opt.as_ref().is_some_and(|c| {
                    let bc: BubbleConstant = L::constant_to_bubble(c);
                    bc.to_bool()
                })
            })
            .collect()
    }
}
