use crate::{
    bubbler::{backend::EgglogBackend, schedule::Minimization, Bubbler, InferredFacts},
    colors::implication::Implication,
    language::{rewrite::Rewrite, Language},
};

use super::ImplicationScoreFn;
use super::RewriteScoreFn;

pub struct BasicRewriteMinimize<L: Language> {
    /// A scoring function for each rewrite. Low means better!
    score_fn: Box<RewriteScoreFn<L>>,
    existing: Vec<Rewrite<L>>,
    step_size: usize,
    _marker: std::marker::PhantomData<L>,
}

/// The basic minimization strategy pitched in the Ruler paper.
/// Until the `candidates` are empty:
/// 1. Pick a fact to add.
/// 2. Remove all facts in `candidates` that are now redundant.
impl<L: Language> BasicRewriteMinimize<L> {
    pub fn new(
        score_fn: Box<RewriteScoreFn<L>>,
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

impl<L: Language> Minimization<L> for BasicRewriteMinimize<L> {
    fn minimize(
        &self,
        backend: &mut EgglogBackend<L>,
        candidates: InferredFacts<L>,
    ) -> Result<InferredFacts<L>, String> {
        // 1. Sort candidates by some scoring function.
        let InferredFacts::Rewrites(mut candidates) = candidates else {
            return Err("BasicRewriteMinimize only supports rewrite minimization.".into());
        };
        candidates.sort_by(|a, b| {
            let score_a = (&self.score_fn)(&a);
            let score_b = (&self.score_fn)(&b);
            score_b.cmp(&score_a)
        });

        let mut chosen: Vec<Rewrite<L>> = self.existing.clone();

        // 2. Add the lhs and rhs of each candidate to the backend.
        let mut initial = vec![];
        for rw in candidates.iter() {
            let lhs = rw.lhs_concrete();
            let rhs = rw.rhs_concrete();
            backend.add_term(lhs.clone(), None).unwrap();
            backend.add_term(rhs.clone(), None).unwrap();
            initial.push((rw, lhs, rhs));
        }

        // 3. Iteratively add rewrites and remove redundant ones.
        while let Some(selected) = candidates.pop() {
            chosen.push(selected.clone());
            backend.register(&selected).unwrap();

            backend.run_rewrites().unwrap();

            // Remove redundant candidates.
            candidates.retain(|rw| {
                let lhs = rw.lhs_concrete();
                let rhs = rw.rhs_concrete();

                !backend.is_equal(&lhs, &rhs).unwrap()
            });
        }

        Ok(InferredFacts::Rewrites(chosen))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        bubbler::minimization::score_fns,
        language::Term,
        test_langs::llvm::{LLVMLang, LLVMLangOp},
    };

    use super::*;
    #[test]
    fn basic_equality_test() {
        let mut backend = EgglogBackend::<LLVMLang>::new();
        let mut candidates: Vec<Rewrite<LLVMLang>> = vec![];

        // rules:
        // a + 0 ~> a
        // a + b - b ~> a
        // a + 0 ~> a + a - a <-- this should be subsumed by the other three.
        // a + b ~> b + a
        let r1 = Rewrite::new(
            None,
            Term::Call(LLVMLangOp::Add, vec![Term::Var("a".into()), Term::Const(0)]),
            Term::Var("a".into()),
        )
        .unwrap();

        candidates.push(r1.clone());

        let r2 = Rewrite::new(
            None,
            Term::Call(
                LLVMLangOp::Sub,
                vec![
                    Term::Call(
                        LLVMLangOp::Add,
                        vec![Term::Var("a".into()), Term::Var("a".into())],
                    ),
                    Term::Var("a".into()),
                ],
            ),
            Term::Var("a".into()),
        )
        .unwrap();

        candidates.push(r2.clone());

        let r3 = Rewrite::new(
            None,
            Term::Call(LLVMLangOp::Add, vec![Term::Var("a".into()), Term::Const(0)]),
            Term::Call(
                LLVMLangOp::Sub,
                vec![
                    Term::Call(
                        LLVMLangOp::Add,
                        vec![Term::Var("a".into()), Term::Var("a".into())],
                    ),
                    Term::Var("a".into()),
                ],
            ),
        )
        .unwrap();

        candidates.push(r3.clone());

        let r4 = Rewrite::new(
            None,
            Term::Call(
                LLVMLangOp::Add,
                vec![Term::Var("a".into()), Term::Var("b".into())],
            ),
            Term::Call(
                LLVMLangOp::Add,
                vec![Term::Var("b".into()), Term::Var("a".into())],
            ),
        )
        .unwrap();
        candidates.push(r4.clone());

        let minimizer = BasicRewriteMinimize::new(
            score_fns::rewrite_score_fns::ast_size::<LLVMLang>(),
            vec![],
            1,
        );
        let minimized = minimizer
            .minimize(&mut backend, InferredFacts::Rewrites(candidates))
            .unwrap();

        let InferredFacts::Rewrites(minimized) = &minimized else {
            panic!("Expected rewrites after minimization.");
        };

        assert_eq!(minimized.len(), 3);
        assert!(minimized.contains(&r1));
        assert!(minimized.contains(&r2));
        assert!(minimized.contains(&r4));
    }
}

pub struct BasicImplicationMinimize<L: Language> {
    /// A scoring function for each rewrite. Low means better!
    score_fn: Box<ImplicationScoreFn<L>>,
    existing: Vec<Implication<L>>,
    step_size: usize,
    _marker: std::marker::PhantomData<L>,
}

/// The basic minimization strategy pitched in the Ruler paper.
/// Until the `candidates` are empty:
/// 1. Pick a fact to add.
/// 2. Remove all facts in `candidates` that are now redundant.
impl<L: Language> BasicImplicationMinimize<L> {
    pub fn new(
        score_fn: Box<ImplicationScoreFn<L>>,
        existing: Vec<Implication<L>>,
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

impl<L: Language> Minimization<L> for BasicImplicationMinimize<L> {
    fn minimize(
        &self,
        backend: &mut EgglogBackend<L>,
        candidates: InferredFacts<L>,
    ) -> Result<InferredFacts<L>, String> {
        // 1. Sort candidates by some scoring function.
        let InferredFacts::Implications(mut candidates) = candidates else {
            return Err("BasicImplicationMinimize only supports implication minimization.".into());
        };
        candidates.sort_by(|a, b| {
            let score_a = (&self.score_fn)(&a);
            let score_b = (&self.score_fn)(&b);
            score_b.cmp(&score_a)
        });

        let mut chosen: Vec<Implication<L>> = self.existing.clone();

        // 2. Iteratively add implications and remove redundant ones.
        while let Some(selected) = candidates.pop() {
            chosen.push(selected.clone());
            backend.register_implication(&selected).unwrap();

            backend.run_rewrites().unwrap();

            // Remove redundant candidates.
            candidates.retain(|imp| {
                let pre = imp.lhs_concrete();
                let post = imp.rhs_concrete();

                !backend.is_implied(&pre, &post).unwrap()
            });
        }

        Ok(InferredFacts::Implications(chosen))
    }
}
