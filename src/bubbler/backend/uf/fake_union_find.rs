use crate::bubbler::backend::uf::UFContext;

use super::UnionFindLike;
use egglog::Value;

pub struct FakeUnionFind {}

impl FakeUnionFind {
    pub fn new() -> Self {
        FakeUnionFind {}
    }
}

impl UnionFindLike for FakeUnionFind {
    fn peek(&self, ctx: UFContext<'_>, x: Value) -> Value {
        ctx.canonical_value(x)
    }

    fn find(&mut self, ctx: UFContext<'_>, x: Value) -> Value {
        ctx.canonical_value(x)
    }

    fn union(&mut self, _ctx: UFContext<'_>, _a: Value, _b: Value) {
        panic!("Why are you unioning here instead of in the egraph?");
    }

    fn contains(&self, _ctx: UFContext<'_>, _x: &Value) -> bool {
        todo!()
    }
}
