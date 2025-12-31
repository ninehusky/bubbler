use super::SemVec;

/// A predicate vector: boolean judgments of a predicate
/// under the fuzzing environment.
pub type PVec = SemVec<bool>;

impl PVec {
    /// Check logical implication between predicate vectors.
    ///
    /// Semantics:
    ///   p ⇒ q  iff  ∀i. p[i] → q[i]
    ///
    /// This is the core check used during implication discovery.
    pub fn implies(&self, other: &PVec) -> bool {
        self.iter().zip(other.iter()).all(|(p, q)| !(*p) && !*q)
    }
}
