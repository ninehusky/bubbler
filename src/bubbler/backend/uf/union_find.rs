/// Andrew's implementation of union-find.
/// I haven't needed to do this since undergrad.
use std::collections::HashMap;

use crate::bubbler::backend::uf::UFContext;

use super::UnionFindLike;

use egglog::Value;

#[derive(Default)]
#[allow(dead_code)]
pub struct UnionFind {
    parent: HashMap<Value, Value>,
    rank: HashMap<Value, u32>,
}

impl UnionFind {
    pub fn new() -> Self {
        Self::default()
    }
}

impl UnionFindLike for UnionFind {
    /// Find the representative of `x` without path compression.
    /// I'm calling this `peek` because usually you want `find`.
    fn peek(&self, ctx: UFContext<'_>, x: Value) -> Value {
        assert!(
            matches!(ctx, UFContext::None),
            "peek with context is not supported"
        );
        let mut current = x;
        while let Some(p) = self.parent.get(&current) {
            current = *p;
        }
        current
    }

    /// Find the representative of `x`.
    /// If `x` is not present, it is its own representative.
    fn find(&mut self, ctx: UFContext<'_>, x: Value) -> Value {
        assert!(
            matches!(ctx, UFContext::None),
            "find with context is not supported"
        );
        if let Some(p) = self.parent.get(&x).cloned() {
            let root = self.find(ctx, p);
            self.parent.insert(x, root); // path compression
            root
        } else {
            x
        }
    }

    /// Union the sets containing `a` and `b`.
    fn union(&mut self, ctx: UFContext<'_>, a: Value, b: Value) {
        assert!(
            matches!(ctx, UFContext::None),
            "find with context is not supported"
        );
        let ra = self.find(ctx.clone(), a);
        let rb = self.find(ctx, b);

        if ra == rb {
            return;
        }

        let rank_a = *self.rank.get(&ra).unwrap_or(&0);
        let rank_b = *self.rank.get(&rb).unwrap_or(&0);

        if rank_a < rank_b {
            self.parent.insert(ra, rb);
        } else if rank_a > rank_b {
            self.parent.insert(rb, ra);
        } else {
            self.parent.insert(rb, ra);
            self.rank.insert(ra, rank_a + 1);
        }
    }

    /// Returns true if this UF has *any* information about `x`.
    fn contains(&self, ctx: UFContext<'_>, x: &Value) -> bool {
        assert!(
            matches!(ctx, UFContext::None),
            "contains with context is not supported"
        );
        self.parent.contains_key(x) || self.rank.contains_key(x)
    }
}
