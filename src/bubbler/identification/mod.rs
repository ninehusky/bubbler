mod basic_match;
mod bubbler_match;

pub use basic_match::{ConditionalCvecMatch, CvecMatch, PvecMatch};

/// Describes the kind fact that is being identified.
#[derive(Clone, Debug)]
pub struct IdentificationConfig {
    /// What is being identified?
    pub mode: IdentificationMode,
    /// Should we validate now or later?
    pub validate_now: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IdentificationMode {
    Implications,
    Rewrites,
}
