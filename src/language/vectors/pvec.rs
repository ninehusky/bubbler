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
        self.iter().zip(other.iter()).all(|(p, q)| !(*p) || *q)
    }

    pub fn is_tautology(&self) -> bool {
        self.iter().all(|b| *b)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pvec(vals: &[bool]) -> PVec {
        vals.iter().cloned().collect()
    }

    #[test]
    fn implies_truth_table() {
        // p=F, q=F: F→F = true
        assert!(pvec(&[false]).implies(&pvec(&[false])));
        // p=F, q=T: F→T = true
        assert!(pvec(&[false]).implies(&pvec(&[true])));
        // p=T, q=T: T→T = true
        assert!(pvec(&[true]).implies(&pvec(&[true])));
        // p=T, q=F: T→F = false
        assert!(!pvec(&[true]).implies(&pvec(&[false])));
    }

    #[test]
    fn implies_mixed_vec() {
        // [T,F] implies [T,T]: slot 0 ok (T→T), slot 1 ok (F→T)
        assert!(pvec(&[true, false]).implies(&pvec(&[true, true])));
        // [T,T] does NOT imply [T,F]: slot 1 fails (T→F)
        assert!(!pvec(&[true, true]).implies(&pvec(&[true, false])));
    }

    #[test]
    fn is_tautology() {
        assert!(pvec(&[true, true, true]).is_tautology());
        assert!(!pvec(&[true, false, true]).is_tautology());
        assert!(!pvec(&[false, false]).is_tautology());
    }
}
