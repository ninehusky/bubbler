use super::Language;

pub struct Rewrite<L: Language> {
    pub cond: Option<L>,
    pub lhs: L,
    pub rhs: L,
}

pub struct Implication<L: Language> {
    pub pre: L,
    pub post: L,
}
