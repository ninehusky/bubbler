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
    bubbler::backend::{
        colors::context::{lattice_is_set, with_lattice},
        enodes::EClassId,
        uf::{FakeUnionFind, UFContext, UnionFind},
    },
    language::{Language, term::PredicateTerm},
};
use egglog::EGraph;
use graph::{Graph, NodeId};
pub use implication::{Condition, Implication};
use std::collections::HashMap;

use super::uf::UnionFindLike;

pub type ColorId = NodeId;

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
    top: ColorId,
    bottom: ColorId,
    facts: HashMap<PredicateTerm<L>, ColorId>,
    ufs: HashMap<ColorId, Box<dyn UnionFindLike + 'a>>,
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
        assert!(self.ufs.contains_key(&self.top));

        // The graph size should be exactly the number of UFs plus 1 (bottom).
        assert_eq!(self.graph.size(), self.ufs.len() + 1);

        // There should be exactly one more UF than there are facts,
        // because of the top element.
        assert_eq!(self.facts.len() + 1, self.ufs.len());
        true
    }

    /// Trim edges that are redundant in the lattice.
    fn trim_edges(&mut self) {
        todo!("MAXIM!!! Implement this now!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    }

    fn add_color(&mut self, predicate: PredicateTerm<L>) -> ColorId {
        let id = self.graph.add_node(LatticeNode::Fact(predicate.clone()));
        self.facts.insert(predicate, id);
        self.ufs.insert(id, Box::new(UnionFind::new()));
        // here, add an implication from true -> new color.
        self.graph.add_edge(self.top, id);

        self.assert_invariants();

        id
    }

    // This `egraph` parameter is potentially unsafe. I guess the invariant for a Bubbler overall is that
    // the `egraph` you pass into here _must_ be the same one that represents "top" in the lattice.
    fn colored_find(&mut self, egraph: &mut EGraph, color: ColorId, elem: EClassId) -> EClassId {
        assert_eq!(
            egraph.get_canonical_value(elem.0, egraph.get_sort_by_name(L::name()).unwrap()),
            elem.0,
            "Element passed to colored_find is not canonical!"
        );

        let uf = self
            .ufs
            .get_mut(&color)
            .expect("No UF for color in colored_find"); // do we wanna use `ensure_color` here?

        let uf_ctx = if self.top == color {
            UFContext::EGraph {
                egraph,
                sort: egraph.get_sort_by_name(L::name()).unwrap(),
            }
        } else {
            UFContext::None
        };

        let rep = uf.find(uf_ctx, elem.0);
        EClassId(rep)
    }

    fn colored_merge(&mut self, egraph: &mut EGraph, color: ColorId, a: EClassId, b: EClassId) {
        if color == self.top {
            panic!("Why are you merging under the top color? Just merge normally!");
        }
        assert_eq!(
            egraph.get_canonical_value(a.0, egraph.get_sort_by_name(L::name()).unwrap()),
            a.0,
            "Element passed to colored_find is not canonical!"
        );

        assert_eq!(
            egraph.get_canonical_value(b.0, egraph.get_sort_by_name(L::name()).unwrap()),
            b.0,
            "Element passed to colored_find is not canonical!"
        );

        let uf = self
            .ufs
            .get_mut(&color)
            .expect("No UF for color in colored_merge");

        let uf_ctx = UFContext::None;

        uf.union(uf_ctx, a.0, b.0);
    }

    fn fact_node(&mut self, cond: PredicateTerm<L>) -> ColorId {
        if let Some(&id) = self.facts.get(&cond) {
            id
        } else {
            println!("debug: adding new fact node for condition {:?}", cond);
            let id = self.graph.add_node(LatticeNode::Fact(cond.clone()));
            self.facts.insert(cond, id);
            self.graph.add_edge(self.top, id);
            self.ufs.insert(id, Box::new(UnionFind::new()));
            id
        }
    }

    pub fn top(&self) -> ColorId {
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
        if lattice_is_set() {
            panic!("Lattice is already set!");
        }
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
