//! Defines an API for colored e-graph operations in egglog.
//! These are probably not going to be implemented nearly as efficiently
//! as they would be in Egg, but for prototyping this should reveal where
//! the bottlenecks are.
//!
//! In particular, we define operations for colored merge and find.
//! Maybe there are more later. Meh!

mod commands;
mod context;
mod graph;
pub mod implication;

use crate::{
    bubbler::backend::uf::{FakeUnionFind, UnionFind},
    language::{Language, term::PredicateTerm},
};
use graph::{Graph, NodeId};
pub use implication::{Condition, Implication};
use std::collections::HashMap;

use super::uf::UnionFindLike;

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
pub struct Lattice<'a, L: Language> {
    graph: Graph<LatticeNode<L>>,
    top: NodeId,
    bottom: NodeId,
    facts: HashMap<PredicateTerm<L>, NodeId>,
    ufs: HashMap<NodeId, Box<dyn UnionFindLike + 'a>>,
}

#[allow(dead_code)]
#[derive(PartialEq, Eq)]
pub enum LatticeNode<L: Language> {
    Top,
    Bottom,
    Fact(PredicateTerm<L>),
}

#[allow(dead_code)]
impl<'a, L: Language> Lattice<'a, L> {
    fn assert_invariants(&self) -> bool {
        // The graph size should be exactly the number of UFs plus 1 (bottom).
        assert_eq!(self.graph.size(), self.ufs.len() + 1);

        // There should be exactly one more UF than there are facts,
        // because of the top element.
        assert_eq!(self.facts.len() + 1, self.ufs.len());
        true
    }

    fn fact_node(&mut self, cond: PredicateTerm<L>) -> NodeId {
        if let Some(&id) = self.facts.get(&cond) {
            id
        } else {
            let id = self.graph.add_node(LatticeNode::Fact(cond.clone()));
            self.facts.insert(cond, id);
            self.ufs.insert(id, Box::new(UnionFind::new()));
            id
        }
    }

    pub fn top(&self) -> NodeId {
        self.top
    }

    pub fn size(&self) -> usize {
        self.graph.size()
    }

    pub fn add_implication(&mut self, imp: Implication<L>) {
        let Condition::Predicate(from) = &imp.from else {
            panic!("LHS of implication is not a term");
        };
        let to_node = self.fact_node(imp.to);
        let from_node = self.fact_node(from.clone());
        self.graph.add_edge(to_node, from_node);
        debug_assert!(self.assert_invariants());
    }

    /// Create a new lattice with just the top and bottom elements.
    pub fn new() -> Self {
        let mut graph = Graph::new();
        let facts = HashMap::new();
        let mut ufs = HashMap::new();
        let bottom = graph.add_node(LatticeNode::Bottom);
        let top = graph.add_node(LatticeNode::Top);
        graph.add_edge(top, bottom); // false -> true, so edge from top to bottom
        ufs.insert(
            top,
            Box::new(FakeUnionFind::new()) as Box<dyn UnionFindLike>,
        );
        Lattice {
            graph,
            top,
            bottom,
            facts,
            ufs,
        }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::{
        language::Term,
        test_langs::llvm::{LLVMLang, LLVMLangOp},
    };

    use super::*;

    #[test]
    fn empty_lattice() {
        let lattice: Lattice<LLVMLang> = Lattice::new();
        assert_eq!(lattice.graph.size(), 2);
    }

    #[test]
    fn add_implication_ok() {
        let mut lattice: Lattice<LLVMLang> = Lattice::new();
        let imp = Implication::new(
            Condition::Predicate(PredicateTerm::from_term(Term::Call(
                LLVMLangOp::Lt,
                vec![Term::Var("x".into()), Term::Const(0)],
            ))),
            PredicateTerm::from_term(Term::Call(
                LLVMLangOp::Neq,
                vec![Term::Var("x".into()), Term::Const(0)],
            )),
        )
        .unwrap();
        lattice.add_implication(imp);
        // top, bottom, and the two fact nodes
        assert_eq!(lattice.graph.size(), 2 + 2);
    }
}
