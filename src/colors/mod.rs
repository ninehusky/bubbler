//! Defines an API for colored e-graph operations in egglog.
//! These are probably not going to be implemented nearly as efficiently
//! as they would be in Egg, but for prototyping this should reveal where
//! the bottlenecks are.
//!
//! In particular, we define operations for colored merge and find.
//! Maybe there are more later. Meh!

mod graph;

use std::collections::HashMap;
use std::hash::Hash;

use crate::language::{Language, Term};
use graph::{Graph, NodeId};

/// A condition under which new equalities may hold.
/// TODO: Conditions should take terms in a metalanguage that can express arbitrary
/// abstract conditions, not just terms in the object language.
#[derive(Clone, PartialEq, Eq)]
pub struct Condition<L: Language> {
    pub term: Term<L>,
}

impl<L: Language> Hash for Condition<L>
where
    L: Language,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.term.hash(state);
    }
}

/// A directed implication from one condition to another.
/// How this relates to equalities: for two conditions `p`, `q`
/// such that `p -> q`, any terms equal in `q` are also equal in `p`.
/// Example:
/// ```text
/// p: x < 0
/// q: x < 1
/// Observe that p -> q; if x < 0, then x < 1 also holds.
/// Therefore, any equality that holds under the assumption `q` also holds
/// under the assumption `p`, e.g., `max(x, 2) == 2` in the world where we assume `x < 1`,
/// therefore it also holds in the world where we assume `x < 0`.
/// ```
/// Bi-directional implications always represent equivalences.
/// If you have a pair of implications `p -> q` and `q -> p`,
/// that's a great opportunity to merge that into an equality.
/// But don't let me boss you around.
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
/// The Lattice, at minimum, always has at least two nodes:
/// the north-most node representing the condition `bottom` (false; an error has occurred),
/// and the south-most node representing the condition `top` (true; no assumptions).
/// It's confusing, I know.
pub struct Lattice<L: Language> {
    graph: Graph<LatticeNode<L>>,
    top: NodeId,
    bottom: NodeId,
    facts: HashMap<Condition<L>, NodeId>,
}

pub enum LatticeNode<L: Language> {
    Top,
    Bottom,
    Fact(Condition<L>),
}

impl<L: Language> Lattice<L> {
    fn fact_node(&mut self, cond: Condition<L>) -> NodeId {
        if let Some(&id) = self.facts.get(&cond) {
            id
        } else {
            let id = self.graph.add_node(LatticeNode::Fact(cond.clone()));
            self.facts.insert(cond, id);
            id
        }
    }

    pub fn add_implication(&mut self, imp: Implication<L>) {
        let to_node = self.fact_node(imp.to);
        let from_node = self.fact_node(imp.from);
        self.graph.add_edge(to_node, from_node);
    }

    /// Create a new lattice with just the top and bottom elements.
    pub fn new() -> Self {
        let mut graph = Graph::new();
        let facts = HashMap::new();
        let bottom = graph.add_node(LatticeNode::Bottom);
        let top = graph.add_node(LatticeNode::Top);
        graph.add_edge(top, bottom); // false -> true, so edge from top to bottom
        Lattice {
            graph,
            top,
            bottom,
            facts,
        }
    }
}
