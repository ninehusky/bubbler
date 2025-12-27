use egglog::SerializeConfig;

use crate::{
    bubbler::{backend::EgglogBackend, schedule::Minimization, Bubbler, InferredFacts},
    colors::implication::Implication,
    language::{rewrite::Rewrite, Language, PredicateTerm},
};

use super::ImplicationScoreFn;
use super::RewriteScoreFn;

pub struct BasicRewriteMinimize<'a, L: Language> {
    /// A scoring function for each rewrite. Low means better!
    score_fn: Box<RewriteScoreFn<'a, L>>,
    existing: Vec<Rewrite<L>>,
    step_size: usize,
    _marker: std::marker::PhantomData<L>,
}

/// The basic minimization strategy pitched in the Ruler paper.
/// Until the `candidates` are empty:
/// 1. Pick a fact to add.
/// 2. Remove all facts in `candidates` that are now redundant.
impl<'a, L: Language> BasicRewriteMinimize<'a, L> {
    pub fn new(
        score_fn: Box<RewriteScoreFn<'a, L>>,
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

impl<'a, L: Language> Minimization<L> for BasicRewriteMinimize<'a, L> {
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
        for rw in candidates.iter() {
            let lhs = rw.lhs_concrete();
            let rhs = rw.rhs_concrete();
            backend.add_term(lhs.clone(), None).unwrap();
            backend.add_term(rhs.clone(), None).unwrap();
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

        // 4. Only return the new rules.
        let delta = chosen
            .into_iter()
            .filter(|rw| !self.existing.contains(rw))
            .collect();

        Ok(InferredFacts::Rewrites(delta))
    }
}

#[cfg(test)]
mod rw_tests {
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

pub struct BasicImplicationMinimize<'a, L: Language> {
    /// A scoring function for each rewrite. Low means better!
    score_fn: Box<ImplicationScoreFn<'a, L>>,
    existing: Vec<Implication<L>>,
    step_size: usize,
    _marker: std::marker::PhantomData<L>,
}

/// The basic minimization strategy pitched in the Ruler paper.
/// Until the `candidates` are empty:
/// 1. Pick a fact to add.
/// 2. Remove all facts in `candidates` that are now redundant.
impl<'a, L: Language> BasicImplicationMinimize<'a, L> {
    pub fn new(
        score_fn: Box<ImplicationScoreFn<'a, L>>,
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

impl<'a, L: Language> Minimization<L> for BasicImplicationMinimize<'a, L> {
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

        for imp in candidates.iter() {
            let pre = imp.lhs_concrete();
            let post = imp.rhs_concrete();
            backend.add_predicate(pre.clone(), None).unwrap();
            backend.add_predicate(post.clone(), None).unwrap();
        }

        // 2. Iteratively add implications and remove redundant ones.
        while let Some(selected) = candidates.pop() {
            chosen.push(selected.clone());
            backend.register_implication(&selected).unwrap();

            backend.run_implications().unwrap();

            // Remove redundant candidates.
            candidates.retain(|imp| {
                let pre = imp.lhs_concrete();
                let post = imp.rhs_concrete();

                !backend.is_implied(&pre, &post).unwrap()
            });
        }

        // 3. Only add the new rules.
        let delta = chosen
            .into_iter()
            .filter(|imp| !self.existing.contains(imp))
            .collect();

        Ok(InferredFacts::Implications(delta))
    }
}

#[cfg(test)]
mod imp_tests {
    use crate::{
        bubbler::minimization::score_fns::implication_score_fns,
        language::{PredicateTerm, Term},
        test_langs::llvm::{LLVMLang, LLVMLangOp},
    };

    use super::*;

    #[test]
    fn basic_implication_test() {
        let mut backend = EgglogBackend::<LLVMLang>::new();
        let a_lt_zero: PredicateTerm<LLVMLang> = PredicateTerm::from_term(Term::Call(
            LLVMLangOp::Lt,
            vec![Term::Var("a".into()), Term::Const(0)],
        ));

        let a_lt_neg_one: PredicateTerm<LLVMLang> = PredicateTerm::from_term(Term::Call(
            LLVMLangOp::Lt,
            vec![Term::Var("a".into()), Term::Const(-1)],
        ));

        let a_lt_neg_two: PredicateTerm<LLVMLang> = PredicateTerm::from_term(Term::Call(
            LLVMLangOp::Lt,
            vec![Term::Var("a".into()), Term::Const(-2)],
        ));

        let a_lt_neg_three: PredicateTerm<LLVMLang> = PredicateTerm::from_term(Term::Call(
            LLVMLangOp::Lt,
            vec![Term::Var("a".into()), Term::Const(-3)],
        ));

        let candidates = vec![
            // a < -2 --> a < 0
            Implication::new(a_lt_neg_two.clone().into(), a_lt_zero.clone()).unwrap(),
            // a < -1 --> a < 0
            Implication::new(a_lt_neg_one.clone().into(), a_lt_zero.clone()).unwrap(),
            // a < -3 --> a < 0
            Implication::new(a_lt_neg_three.clone().into(), a_lt_zero.clone()).unwrap(),
        ];

        let existing = vec![
            // a < -3 --> a < -2
            Implication::new(a_lt_neg_three.clone().into(), a_lt_neg_two.clone()).unwrap(),
            // a < -2 --> a < -1
            Implication::new(a_lt_neg_two.clone().into(), a_lt_neg_one.clone()).unwrap(),
            // a < -1 --> a < 0
            Implication::new(a_lt_neg_one.clone().into(), a_lt_zero.clone()).unwrap(),
        ];

        for imp in &existing {
            backend.register_implication(imp).unwrap();
        }

        let minimizer = BasicImplicationMinimize::new(
            implication_score_fns::prioritize_vars::<LLVMLang>(),
            existing.clone(),
            1,
        );

        let minimized = minimizer
            .minimize(&mut backend, InferredFacts::Implications(candidates))
            .unwrap();

        let InferredFacts::Implications(minimized) = &minimized else {
            panic!("Expected implications after minimization.");
        };

        // Why 3 + 1 and not 3?
        // When we minimize, we add one candidate to the backend first, and then
        // remove redundants. So even though all three candidates are redundant,
        // we will have added one of them before checking for redundancy.
        // In the real world, this won't happen because every candidate
        // is never derivable from existing implications alone.
        assert_eq!(minimized.len(), existing.len() + 1);
    }
}

pub struct ConditionalRewriteMinimize<'a, L: Language> {
    score_fn: Box<RewriteScoreFn<'a, L>>,
    existing_rws: Vec<Rewrite<L>>,
    existing_imps: Vec<Implication<L>>,
    step_size: usize,
    _marker: std::marker::PhantomData<L>,
}

impl<'a, L: Language> Minimization<L> for ConditionalRewriteMinimize<'a, L> {
    fn minimize(
        &self,
        backend: &mut EgglogBackend<L>,
        candidates: InferredFacts<L>,
    ) -> Result<InferredFacts<L>, String> {
        // 1. Sort candidates by some scoring function.
        let InferredFacts::Rewrites(mut candidates) = candidates else {
            return Err("ConditionalRewriteMinimize only supports rewrite minimization.".into());
        };

        candidates.sort_by(|a, b| {
            let score_a = (&self.score_fn)(&a);
            let score_b = (&self.score_fn)(&b);
            score_b.cmp(&score_a)
        });

        // 2. Add the predicates, lhs, and rhs of each candidate to the backend.
        for rw in candidates.iter() {
            if let Rewrite::Conditional { cond, .. } = rw {
                backend
                    .add_predicate(
                        PredicateTerm::from_term(cond.term.concretize().clone().unwrap()),
                        None,
                    )
                    .unwrap();
            }
            let lhs = rw.lhs_concrete();
            let rhs = rw.rhs_concrete();
            backend.add_term(lhs.clone(), None).unwrap();
            backend.add_term(rhs.clone(), None).unwrap();
        }

        let mut chosen: Vec<Rewrite<L>> = self.existing_rws.clone();

        // 3. While candidates is not empty:
        //   a. Pick the best candidate.
        //   b. Add it to the backend.
        //   c. Run rewrites and implications.
        while let Some(selected) = candidates.pop() {
            backend.register(&selected).unwrap();
            chosen.push(selected.clone());

            backend.run_rewrites().unwrap();
            backend.run_implications().unwrap();

            // Remove redundant candidates.
            candidates.retain(|rw| {
                let lhs = rw.lhs_concrete();
                let rhs = rw.rhs_concrete();
                let cond = rw.cond_concrete().unwrap();

                !backend.is_conditionally_equal(&cond, &lhs, &rhs).unwrap()
            });
        }

        // 4. Only add the new rules.
        let delta = chosen
            .into_iter()
            .filter(|rw| !self.existing_rws.contains(rw))
            .collect();
        Ok(InferredFacts::Rewrites(delta))
    }
}

#[cfg(test)]
pub mod cond_rw_tests {
    use crate::{
        bubbler::minimization::score_fns,
        colors::Condition,
        language::Term,
        test_langs::llvm::{LLVMLang, LLVMLangOp},
    };

    use super::*;

    #[test]
    pub fn conditional_rewrite_minimize() {
        let mut backend = EgglogBackend::<LLVMLang>::new();

        let candidates: Vec<Rewrite<LLVMLang>> = vec![
            Rewrite::new(
                Some(PredicateTerm::from_term(Term::Call(
                    LLVMLangOp::Gt,
                    vec![Term::Var("a".into()), Term::Const(0)],
                ))),
                Term::Call(
                    LLVMLangOp::Div,
                    vec![Term::Var("a".into()), Term::Var("a".into())],
                ),
                Term::Const(1),
            )
            .unwrap(),
            Rewrite::new(
                Some(PredicateTerm::from_term(Term::Call(
                    LLVMLangOp::Lt,
                    vec![Term::Var("a".into()), Term::Const(0)],
                ))),
                Term::Call(
                    LLVMLangOp::Div,
                    vec![Term::Var("a".into()), Term::Var("a".into())],
                ),
                Term::Const(1),
            )
            .unwrap(),
            Rewrite::new(
                // rig the election and get this one to be chosen first!
                // in real life, we'd probably choose the weakest condition
                // first, with pvecs as a proxy for weakness.
                Some(PredicateTerm::from_term(Term::Call(
                    LLVMLangOp::Neq,
                    vec![Term::Var("a".into()), Term::Const(0)],
                ))),
                Term::Call(
                    LLVMLangOp::Div,
                    vec![Term::Var("a".into()), Term::Var("a".into())],
                ),
                Term::Const(1),
            )
            .unwrap(),
        ];

        // note that Lt -> Neq is missing here. That's on purpose!
        let implications: Vec<Implication<LLVMLang>> = vec![Implication::new(
            PredicateTerm::from_term(Term::Call(
                LLVMLangOp::Gt,
                vec![Term::Var("a".into()), Term::Const(0)],
            ))
            .into(),
            PredicateTerm::from_term(Term::Call(
                LLVMLangOp::Neq,
                vec![Term::Var("a".into()), Term::Const(0)],
            )),
        )
        .unwrap()];

        for imp in &implications {
            backend.register_implication(imp).unwrap();
        }

        let minimizer: ConditionalRewriteMinimize<LLVMLang> = ConditionalRewriteMinimize {
            score_fn: score_fns::rewrite_score_fns::ast_size::<LLVMLang>(),
            existing_rws: vec![],
            existing_imps: implications.clone(),
            step_size: 1,
            _marker: std::marker::PhantomData,
        };

        let minimized = minimizer
            .minimize(&mut backend, InferredFacts::Rewrites(candidates))
            .unwrap();

        let InferredFacts::Rewrites(minimized) = &minimized else {
            panic!("Expected rewrites after minimization.");
        };

        assert_eq!(minimized.len(), 2);
        assert!(!minimized.contains(
            &Rewrite::new(
                Some(PredicateTerm::from_term(Term::Call(
                    LLVMLangOp::Gt,
                    vec![Term::Var("a".into()), Term::Const(0)],
                ))),
                Term::Call(
                    LLVMLangOp::Div,
                    vec![Term::Var("a".into()), Term::Var("a".into())],
                ),
                Term::Const(1),
            )
            .unwrap()
        ));
    }
}
