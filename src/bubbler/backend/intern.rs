use std::{
    collections::HashMap,
    hash::{DefaultHasher, Hash, Hasher},
};

pub struct InternStore<T: Eq + Hash> {
    map: HashMap<u64, T>,
}

impl<T> InternStore<T>
where
    T: Eq + Hash,
{
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    /// Inserts a value into the intern store with the given hash.
    /// Panics if a different value with the same hash already exists,
    /// i.e., there is a hash collision.
    pub fn intern(&mut self, value: T) -> u64 {
        let mut hasher = DefaultHasher::new();
        value.hash(&mut hasher);
        let hash = hasher.finish();

        if let Some(existing) = self.map.get(&hash) {
            if existing != &value {
                panic!("Hash collision detected in InternStore",);
            }
        } else {
            self.map.insert(hash, value);
        }
        hash
    }

    pub fn get(&self, hash: u64) -> Option<&T> {
        self.map.get(&hash)
    }
}
