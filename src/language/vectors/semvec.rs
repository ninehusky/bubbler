use std::{hash::Hash, ops::Deref};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SemVec<T>(Vec<T>);

impl<T> Hash for SemVec<T>
where
    T: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for item in &self.0 {
            item.hash(state);
        }
    }
}

impl<T> Deref for SemVec<T> {
    type Target = Vec<T>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> FromIterator<T> for SemVec<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        SemVec(iter.into_iter().collect())
    }
}

impl<'a, T> IntoIterator for &'a SemVec<T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
