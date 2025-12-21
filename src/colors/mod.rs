//! Defines an API for colored e-graph operations in egglog.
//! These are probably not going to be implemented nearly as efficiently
//! as they would be in Egg, but for prototyping this should reveal where
//! the bottlenecks are.
//!
//! In particular, we define operations for colored merge and find.
//! Maybe there are more later. Meh!

mod graph;
pub mod implication;

use std::collections::HashMap;

use crate::language::{
    term::PredicateTerm,
    Language,
};
use graph::{Graph, NodeId};
use implication::Implication;

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
#[allow(dead_code)]
pub struct Lattice<L: Language> {
    graph: Graph<LatticeNode<L>>,
    top: NodeId,
    bottom: NodeId,
    facts: HashMap<PredicateTerm<L>, NodeId>,
}

impl<L: Language> Default for Lattice<L> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum LatticeNode<L: Language> {
    Top,
    Bottom,
    Fact(PredicateTerm<L>),
}

impl<L: Language> Lattice<L> {
    fn fact_node(&mut self, cond: PredicateTerm<L>) -> NodeId {
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
