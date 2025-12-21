//! This module defines a `BubblerAction`, which
//! is just one step of the overall theory exploration algorithm.
//! Conditional theory exploration involves two types of inference:
//! inference of program _analyses_, and then inference of rewrite
//! rules which use these analyses.
//!
//! Both types of inference follow a three step process of (1) enumeration,
//! (2) candidate identification, and (3) minimization.

use egglog::EGraph;

use crate::language::Language;

use ruler::enumo::Workload;

pub struct BubblerSchedule<L: Language> {
    pub actions: Vec<BubblerAction<L>>,
}

impl<L: Language> BubblerSchedule<L> {
    pub fn new(actions: Vec<BubblerAction<L>>) -> Self {
        Self { actions }
    }
}

pub enum BubblerAction<L: Language> {
    EnumerationAction(Box<dyn EnumerationAction<L>>),
    IdentificationAction(Box<dyn IdentificationAction<L>>),
    MinimizationAction(Box<dyn MinimizationAction<L>>),
}

/// Enumeration: Add the terms in some workload to an e-graph.
pub trait EnumerationAction<L: Language> {
    fn enumerate(&self, egraph: &mut EGraph, workload: Workload) -> Result<String, String>;
}

/// Identification: Analyze an e-graph for likely candidates
/// of rewrites/implications.
pub trait IdentificationAction<L: Language> {}

/// Minimization: Given a set of rewrites/implications,
/// select a subset of rules that subsumes
/// the proving power of the original set.
pub trait MinimizationAction<L: Language> {}
