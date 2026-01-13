/// Andrew's implementation of union-find.
/// I haven't needed to do this since undergrad.
use std::collections::HashMap;

use super::UnionFindLike;

use egglog::Value;

pub struct UnionFind {
    parent: HashMap<Value, Value>,
    rank: HashMap<Value, u32>,
}

impl Default for UnionFind {
    fn default() -> Self {
        Self {
            parent: HashMap::new(),
            rank: HashMap::new(),
        }
    }
}

impl UnionFind {
    pub fn new() -> Self {
        Self::default()
    }
}

impl UnionFindLike for UnionFind {
    /// Find the representative of `x` without path compression.
    /// I'm calling this `peek` because usually you want `find`.
    fn peek(&self, x: Value) -> Value {
        let mut current = x.clone();
        while let Some(p) = self.parent.get(&current) {
            current = p.clone();
        }
        current
    }

    /// Find the representative of `x`.
    /// If `x` is not present, it is its own representative.
    fn find(&mut self, x: Value) -> Value {
        if let Some(p) = self.parent.get(&x).cloned() {
            let root = self.find(p);
            self.parent.insert(x.clone(), root.clone()); // path compression
            root
        } else {
            x
        }
    }

    /// Union the sets containing `a` and `b`.
    fn union(&mut self, a: Value, b: Value) {
        let ra = self.find(a.clone());
        let rb = self.find(b.clone());

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
            self.parent.insert(rb, ra.clone());
            self.rank.insert(ra, rank_a + 1);
        }
    }

    /// Returns true if this UF has *any* information about `x`.
    fn contains(&self, x: &Value) -> bool {
        self.parent.contains_key(x) || self.rank.contains_key(x)
    }
}
