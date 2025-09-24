use egglog::EGraph;

use crate::language::rule::{Implication, Rewrite};
use crate::language::Language;

pub struct BubblerConfig {
    pub vars: Vec<String>,
}

pub enum BubblerStep {
    FindRewrites,
    FindImplications,
}

/// One "step" of the Bubbler algorithm.
pub struct BubblerSchedule {
    pub steps: Vec<BubblerStep>,
}

/// The Bubbler struct, which manages a core Bubbler e-graph.
pub struct Bubbler<L: Language> {
    pub egraph: EGraph,
    pub rules: Vec<Rewrite<L>>,
    pub implications: Vec<Implication<L>>,
}

impl<L: Language> Bubbler<L> {
    pub fn new() -> Self {
        let mut egraph = EGraph::default();
        egraph
            .parse_and_run_program(None, L::to_egglog_src().as_str())
            .unwrap();
        Self {
            egraph,
            rules: vec![],
            implications: vec![],
        }
    }
}

mod tests {
    #[test]
    fn bubbler_egraph_ok() {
        use super::Bubbler;
        use crate::language::BubbleLang;

        let _bubbler: Bubbler<BubbleLang> = Bubbler::new();
    }
}
