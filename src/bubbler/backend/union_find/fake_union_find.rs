use super::UnionFindLike;
use egglog::ArcSort;
use egglog::Value;

pub struct FakeUnionFind<'a> {
    egraph: &'a egglog::EGraph,
    sort: &'a egglog::ArcSort,
}

impl<'a> FakeUnionFind<'a> {
    pub fn new(egraph: &'a egglog::EGraph, sort: &'a ArcSort) -> Self {
        Self { egraph, sort }
    }
}

impl<'a> UnionFindLike for FakeUnionFind<'a> {
    fn peek(&self, x: Value) -> Value {
        self.egraph.get_canonical_value(x, self.sort)
    }

    fn find(&mut self, x: Value) -> Value {
        self.egraph.get_canonical_value(x, self.sort)
    }

    fn union(&mut self, _a: Value, _b: Value) {
        panic!("Why are you unioning here instead of in the egraph?");
    }

    fn contains(&self, x: &Value) -> bool {
        let res = self.egraph.extract_value(&self.sort, *x);
        match res {
            Ok(_) => true,
            Err(egglog::Error::NotFoundError(_)) => false,
            _ => panic!("Unexpected error when checking contains in FakeUnionFind"),
        }
    }
}
