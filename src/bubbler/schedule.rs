//! This module defines a `BubblerAction`, which
//! is just one step of the overall theory exploration algorithm.
//! Conditional theory exploration involves two types of inference:
//! inference of program _analyses_, and then inference of rewrite
//! rules which use these analyses.
//!
//! Both types of inference follow a three step process of (1) enumeration,
//! (2) candidate identification, and (3) minimization.

use egglog::EGraph;

use crate::bubbler::Bubbler;
use crate::language::{rewrite::Rewrite, Language};

use ruler::enumo::Workload;

use super::backend::EgglogBackend;
use super::InferredFacts;

pub struct BubblerSchedule<L: Language> {
    pub actions: Vec<BubblerAction<L>>,
}

impl<L: Language> BubblerSchedule<L> {
    pub fn new(actions: Vec<BubblerAction<L>>) -> Self {
        Self { actions }
    }
}

pub enum BubblerAction<L: Language> {
    Enumeration(Box<dyn Enumeration<L>>),
    Identification(Box<dyn Identification<L>>),
    Minimization(Box<dyn Minimization<L>>),
}

/// Enumeration: Add the terms in some workload to an e-graph.
pub trait Enumeration<L: Language> {
    fn enumerate_bubbler(
        &self,
        backend: &mut EgglogBackend<L>,
        workload: Workload,
    ) -> Result<(), String>;
}

/// Identification: Analyze an e-graph for likely candidates
/// of rewrites/implications.
pub trait Identification<L: Language> {
    fn identify(&self, backend: &mut EgglogBackend<L>) -> Result<InferredFacts<L>, String>;
}

/// Minimization: Given a set of rewrites/implications,
/// select a subset of rules that subsumes
/// the proving power of the original set.
pub trait Minimization<L: Language> {
    fn minimize(
        &self,
        backend: &mut EgglogBackend<L>,
        candidates: InferredFacts<L>,
    ) -> Result<InferredFacts<L>, String>;
}
