use crate::{
    bubbler::{backend::EgglogBackend, schedule::Minimization, Bubbler, InferredFacts},
    language::{rewrite::Rewrite, Language},
};

pub struct BasicMinimize<L: Language> {
    /// A scoring function for each rewrite. Low means better!
    score_fn: Box<dyn Fn(Rewrite<L>) -> i64>,
    existing: Vec<Rewrite<L>>,
    step_size: usize,
    _marker: std::marker::PhantomData<L>,
}

/// The basic minimization strategy pitched in the Ruler paper.
/// Until the `candidates` are empty:
/// 1. Pick a fact to add.
/// 2. Remove all facts in `candidates` that are now redundant.
impl<L: Language> BasicMinimize<L> {
    pub fn new(
        score_fn: Box<dyn Fn(Rewrite<L>) -> i64>,
        existing: Vec<Rewrite<L>>,
        step_size: usize,
    ) -> Self {
        Self {
            score_fn,
            existing,
            step_size,
            _marker: std::marker::PhantomData,
        }
    }
}

impl<L: Language> Minimization<L> for BasicMinimize<L> {
    fn minimize(
        &self,
        backend: &mut EgglogBackend<L>,
        candidates: InferredFacts<L>,
    ) -> Result<InferredFacts<L>, String> {
        // 1. Sort candidates by some scoring function.
        let InferredFacts::Rewrites(mut candidates) = candidates else {
            return Err("BasicMinimize only supports rewrite minimization.".into());
        };
        candidates.sort_by(|a, b| {
            let score_a = (self.score_fn)(a.clone());
            let score_b = (self.score_fn)(b.clone());
            score_a.cmp(&score_b)
        });

        // 2. Iteratively add rewrites and remove redundant ones.
        while !candidates.is_empty() {
            let chosen = candidates.pop().unwrap();
            // bubbler.rules.push(chosen.clone());

            // Re-run the e-graph to see what rewrites are now redundant.
        }

        Ok(InferredFacts::Rewrites(vec![]))
    }
}
