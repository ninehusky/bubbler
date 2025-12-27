mod basic_enumerate;

pub use basic_enumerate::BasicEnumerate;

/// Describes roughly what is being enumerated.
#[derive(Clone, Debug)]
pub struct EnumerationConfig {
    /// What is being enumerated?
    pub mode: EnumerationMode,
    /// Should we evaluate the enumerated terms/predicates
    /// before adding them?
    pub evaluate: bool,
}

#[derive(Clone, Debug)]
pub enum EnumerationMode {
    Terms,
    Predicates,
}
