/// Andrew's implementation of union-find.
/// I haven't needed to do this since undergrad.
use std::collections::HashMap;
use std::hash::Hash;

pub struct UnionFind<K> {
    parent: HashMap<K, K>,
    rank: HashMap<K, u32>,
}

impl<K: Eq + Hash + Clone> Default for UnionFind<K> {
    fn default() -> Self {
        Self {
            parent: HashMap::new(),
            rank: HashMap::new(),
        }
    }
}

impl<K: Eq + Hash + Clone> UnionFind<K> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn find(&self, x: K) -> K {
        let mut current = x.clone();
        while let Some(p) = self.parent.get(&current) {
            current = p.clone();
        }
        current
    }

    /// Find the representative of `x`.
    /// If `x` is not present, it is its own representative.
    pub fn find_or_add(&mut self, x: K) -> K {
        if let Some(p) = self.parent.get(&x).cloned() {
            let root = self.find(p);
            self.parent.insert(x.clone(), root.clone()); // path compression
            root
        } else {
            x
        }
    }

    /// Union the sets containing `a` and `b`.
    pub fn union(&mut self, a: K, b: K) {
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
    pub fn contains(&self, x: &K) -> bool {
        self.parent.contains_key(x) || self.rank.contains_key(x)
    }
}
