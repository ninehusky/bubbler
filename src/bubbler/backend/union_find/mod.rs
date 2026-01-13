use egglog::Value;
use std::hash::Hash;

mod fake_union_find;
mod union_find;

pub use fake_union_find::FakeUnionFind;
pub use union_find::UnionFind;

#[derive(Clone)]
pub enum UFContext<'a> {
    EGraph {
        egraph: &'a egglog::EGraph,
        sort: &'a egglog::ArcSort,
    },
    None,
}

impl<'a> UFContext<'a> {
    /// Panics if this is a `None` context.
    pub fn expect(self) -> (&'a egglog::EGraph, &'a egglog::ArcSort) {
        match self {
            UFContext::EGraph { egraph, sort } => (egraph, sort),
            UFContext::None => panic!("Expected EGraph context, found None"),
        }
    }

    pub fn canonical_value(self, x: Value) -> Value {
        let (egraph, sort) = self.expect();
        egraph.get_canonical_value(x, sort)
    }
}

pub trait UnionFindLike: Send + Sync {
    fn peek(&self, ctx: UFContext<'_>, x: Value) -> Value;
    fn find(&mut self, ctx: UFContext<'_>, x: Value) -> Value;
    fn union(&mut self, ctx: UFContext<'_>, a: Value, b: Value);
    fn contains(&self, ctx: UFContext<'_>, x: &Value) -> bool;
}
