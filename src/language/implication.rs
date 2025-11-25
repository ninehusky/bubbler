use super::Language;

pub struct Implication<L: Language> {
    pub antecedent: L,
    pub consequent: L,
}
