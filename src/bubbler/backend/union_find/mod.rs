use egglog::Value;
use std::hash::Hash;

mod fake_union_find;
mod union_find;

pub use fake_union_find::FakeUnionFind;
pub use union_find::UnionFind;

pub trait UnionFindLike: Send + Sync {
    fn peek(&self, x: Value) -> Value;
    fn find(&mut self, x: Value) -> Value;
    fn union(&mut self, a: Value, b: Value);
    fn contains(&self, x: &Value) -> bool;
}
