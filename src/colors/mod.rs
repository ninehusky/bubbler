//! Defines an API for colored e-graph operations in egglog.
//! These are probably not going to be implemented nearly as efficiently
//! as they would be in Egg, but for prototyping this should reveal where
//! the bottlenecks are.
//!
//! In particular, we define operations for colored merge and find.
//! Maybe there are more later. Meh!

use crate::language::{Language, Term};

/// A condition under which new equalities may hold.
pub struct Condition<L: Language> {
    pub term: Term<L>,
}

/// A directed implication from one condition to another.
/// How this relates to equalities: for two conditions `p`, `q`
/// such that `p -> q`, any terms equal in `q` are also equal in `p`.
/// Example:
/// ```text
/// p: x < y
/// q: x != y
/// Observe that p -> q; if x < y, then certainly x != y.
/// Any equality under `q` also holds under `p`.
/// The opposite is not true; for example, even though given `p`,
/// `min(x, y) == x`, this does not necessarily hold under the assumption `q`.
/// ```
/// Bi-directional implications always represent equivalences.
/// If you have a pair of implications `p -> q` and `q -> p`,
/// you should consider merging `p` and `q` into the same condition.
pub struct Implication<L: Language> {
    pub from: Condition<L>,
    pub to: Condition<L>,
}

/// A colored DAG structure for managing conditional equivalences.
/// The nodes in the graph are conditions (colors), and the edges are
/// `[Implication]`s from one condition to another.
/// Importantly, the "edges" are directed in the _opposite_ direction
/// of the logical implication. That is, if `p -> q`, then there is an edge
/// from `q` to `p`.
pub struct Lattice<L: Language> {
    pub root: LatticeNode<L>,
}

pub struct LatticeNode<L: Language> {
    condition: Condition<L>,
    children: Vec<Implication<L>>,
}
