//! This file contains the matching strategies used in Chompy's implementation.

use crate::{
    bubbler::{InferredFacts, backend::EgglogBackend, schedule::Identification},
    colors::implication::Implication,
    language::{CVec, Language, PVec, rewrite::Rewrite},
};

use super::IdentificationConfig;

/// CvecMatch identifies rewrites by looking for terms which are
/// observationally equivalent in the e-graph. More or less a
/// 1:1 mapping from the Enumo repo's implementation.
pub struct CvecMatch<L: Language> {
    cfg: IdentificationConfig,
    _marker: std::marker::PhantomData<L>,
}

impl<L: Language> CvecMatch<L> {
    pub fn new(cfg: IdentificationConfig) -> CvecMatch<L> {
        Self {
            cfg,
            _marker: std::marker::PhantomData::<L>,
        }
    }
}

impl<L: Language> Identification<L> for CvecMatch<L> {
    fn identify(&self, backend: &mut EgglogBackend<L>) -> Result<InferredFacts<L>, String> {
        if self.cfg.mode != super::IdentificationMode::Rewrites {
            return Err("CvecMatch only supports rewrite identification.".into());
        }
        let cvec_map = backend.get_cvec_map();

        // Naively, a pairing of any two terms with identical
        // cvecs is a rewrite.
        let mut candidates = vec![];

        for (_cvec, terms) in cvec_map {
            if terms.len() < 2 {
                continue;
            }

            for i in 0..terms.len() {
                for j in (i + 1)..terms.len() {
                    let lhs = terms[i].clone();
                    let rhs = terms[j].clone();

                    if let Ok(rw) = Rewrite::new(None, lhs, rhs) {
                        candidates.push(rw.clone());
                    }
                }
            }
        }

        Ok(InferredFacts::Rewrites(candidates))
    }
}

/// PvecMatch finds likely candidates for _implications_ through pvec matching.
/// Formally, two predicates with pvecs `p, q` are a match if
/// `\forall i. p[i] => q[i]`. Like in the real world, implications in
/// the forward directions do not guarantee implications in the reverse direction.
///
// NOTE: pvec matching only works for predicates. If you want to discover
// predicates which hold for _any_ term, use a different matching strategy
// (which I'll implement soon!).
// TODO(@ninehusky): implement the above.
#[allow(dead_code)]
pub struct PvecMatch<L: Language> {
    cfg: IdentificationConfig,
    _marker: std::marker::PhantomData<L>,
}

impl<L: Language> PvecMatch<L> {
    pub fn new(cfg: IdentificationConfig) -> PvecMatch<L> {
        Self {
            cfg,
            _marker: std::marker::PhantomData::<L>,
        }
    }

    fn is_bad_pvec(pvec: PVec) -> bool {
        // A pvec which is all false is useless.
        if pvec.iter().all(|b| !*b) {
            return true;
        }

        // A pvec which is all true is useless.
        if pvec.iter().all(|b| *b) {
            return true;
        }
        false
    }
}

impl<L: Language> Identification<L> for PvecMatch<L> {
    fn identify(&self, backend: &mut EgglogBackend<L>) -> Result<InferredFacts<L>, String> {
        let pvec_map = backend.get_pvec_map();

        let pvecs = pvec_map.keys().cloned();

        let mut matching_pvecs: Vec<(PVec, PVec)> = vec![];

        for pvec1 in pvecs.clone() {
            if Self::is_bad_pvec(pvec1.clone()) {
                continue;
            }
            for pvec2 in pvecs.clone() {
                if Self::is_bad_pvec(pvec2.clone()) {
                    continue;
                }
                if pvec1 == pvec2 {
                    continue;
                }

                let mut implies = true;
                for (b1, b2) in pvec1.iter().zip(pvec2.iter()) {
                    if *b1 && !*b2 {
                        implies = false;
                        break;
                    }
                }

                if implies {
                    matching_pvecs.push((pvec1.clone(), pvec2.clone()));
                }
            }
        }

        let mut candidates = vec![];
        for (from, to) in matching_pvecs {
            let from_terms = pvec_map.get(&from).unwrap();
            let to_terms = pvec_map.get(&to).unwrap();

            for from_term in from_terms {
                for to_term in to_terms {
                    let implication =
                        Implication::new(from_term.clone().into(), to_term.clone()).unwrap();
                    candidates.push(implication);
                }
            }
        }

        Ok(InferredFacts::Implications(candidates))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        bubbler::{
            BubblerConfig,
            enumeration::{BasicEnumerate, EnumerationConfig, EnumerationMode},
            identification::{IdentificationConfig, IdentificationMode},
            schedule::{BubblerAction, Enumeration, Identification},
        },
        test_langs::llvm::LLVMLang,
    };

    use super::*;
    #[test]
    fn cvec_match_finds_add_mul_rules() {
        let cfg: BubblerConfig<LLVMLang> =
            BubblerConfig::new(vec!["x".into(), "y".into()], vec![1, 2]);
        let bubbler = Bubbler::new(cfg);

        let enumeration_cfg: EnumerationConfig = EnumerationConfig {
            mode: EnumerationMode::Terms,
            evaluate: true,
        };

        let enumerate_act =
            BasicEnumerate::<LLVMLang>::new(enumeration_cfg, bubbler.environment.clone());

        let mut backend = bubbler.new_backend();

        enumerate_act
            .enumerate_bubbler(
                &mut backend,
                ruler::enumo::Workload::new(vec![
                    "(Add x y)",
                    "(Mul x y)",
                    "(Add y x)",
                    "(Mul y x)",
                    "(Add 0 x)",
                    "x",
                ]),
            )
            .unwrap();

        let identification_cfg: IdentificationConfig = IdentificationConfig {
            mode: IdentificationMode::Rewrites,
            validate_now: false,
        };

        let identification_act = CvecMatch::<LLVMLang>::new(identification_cfg);
        let inferred = identification_act.identify(&mut backend).unwrap();
        let InferredFacts::Rewrites(inferred) = inferred else {
            panic!("Expected rewrites inferred.");
        };
        assert_eq!(inferred.len(), 3);
    }

    #[test]
    fn pvec_match_finds_implications() {
        let cfg: BubblerConfig<LLVMLang> =
            BubblerConfig::new(vec!["x".into(), "y".into()], vec![1, 2]);
        let mut bubbler = Bubbler::new(cfg);

        let enumeration_cfg: EnumerationConfig = EnumerationConfig {
            mode: EnumerationMode::Predicates,
            evaluate: true,
        };

        let mut backend: EgglogBackend<LLVMLang> = bubbler.new_backend();

        let enumerate_act =
            BasicEnumerate::<LLVMLang>::new(enumeration_cfg, bubbler.environment.clone());

        enumerate_act
            .enumerate_bubbler(
                &mut backend,
                ruler::enumo::Workload::new(vec!["(Lt x y)", "(Gt x y)", "(Neq x y)"]),
            )
            .unwrap();

        let identification_cfg: IdentificationConfig = IdentificationConfig {
            mode: IdentificationMode::Implications,
            validate_now: false,
        };

        let identification_act = PvecMatch::<LLVMLang>::new(identification_cfg);
        let inferred = identification_act.identify(&mut backend).unwrap();
        let InferredFacts::Implications(inferred) = inferred else {
            panic!("Expected implications inferred.");
        };
        assert_eq!(inferred.len(), 2);
    }
}

#[derive(Debug, Clone)]
pub struct ConditionalCvecMatch<L: Language> {
    cfg: IdentificationConfig,
    _marker: std::marker::PhantomData<L>,
}

impl<L: Language> ConditionalCvecMatch<L> {
    pub fn new(cfg: IdentificationConfig) -> ConditionalCvecMatch<L> {
        Self {
            cfg,
            _marker: std::marker::PhantomData::<L>,
        }
    }

    fn pvec_implies_cvecs_equal(pvec: &PVec, cvec1: &CVec<L>, cvec2: &CVec<L>) -> bool {
        assert_eq!(pvec.len(), cvec1.len());
        assert_eq!(cvec1.len(), cvec2.len());

        for i in 0..pvec.len() {
            let p = pvec[i];
            let c1 = &cvec1[i];
            let c2 = &cvec2[i];

            // p[i] -> c[i] == c'[i]
            if p && c1 != c2 {
                return false;
            }
        }

        true
    }
}

impl<L: Language> Identification<L> for ConditionalCvecMatch<L> {
    fn identify(&self, backend: &mut EgglogBackend<L>) -> Result<InferredFacts<L>, String> {
        if self.cfg.mode != super::IdentificationMode::Rewrites {
            return Err("ConditionalCvecMatch only supports rewrite identification.".into());
        }
        let cvec_map = backend.get_cvec_map();
        let pvec_map = backend.get_pvec_map();

        let mut vector_matches: Vec<(PVec, CVec<L>, CVec<L>)> = vec![];
        for (i, cvec1) in cvec_map.keys().cloned().enumerate() {
            for cvec2 in cvec_map.keys().skip(i + 1) {
                // find the pvecs which make cvec1 = cvec2.
                for pvec in pvec_map.keys() {
                    if Self::pvec_implies_cvecs_equal(pvec, &cvec1, cvec2) {
                        vector_matches.push((pvec.clone(), cvec1.clone(), cvec2.clone()));
                    }
                }
            }
        }

        let mut candidates: Vec<Rewrite<L>> = vec![];

        // Now, materialize the matches into candidates.
        for (pvec, cvec1, cvec2) in vector_matches {
            let cond = pvec_map.get(&pvec).unwrap();
            let terms1 = cvec_map.get(&cvec1).unwrap();
            let terms2 = cvec_map.get(&cvec2).unwrap();

            for cond_term in cond {
                for term1 in terms1 {
                    for term2 in terms2 {
                        if backend.is_conditionally_equal(cond_term, term1, term2)? {
                            continue;
                        }
                        let fwds =
                            Rewrite::new(Some(cond_term.clone()), term1.clone(), term2.clone());
                        if let Ok(rw) = fwds {
                            candidates.push(rw);
                        }
                        let bwds =
                            Rewrite::new(Some(cond_term.clone()), term2.clone(), term1.clone());
                        if let Ok(rw) = bwds {
                            candidates.push(rw);
                        }
                    }
                }
            }
        }

        Ok(InferredFacts::Rewrites(candidates))
    }
}

#[cfg(test)]
pub mod cond_cvec_match_test {
    use egglog::SerializeConfig;

    use crate::{
        bubbler::{BubblerConfig, identification::IdentificationMode},
        language::{PredicateTerm, Term},
        test_langs::llvm::{LLVMLang, LLVMLangOp},
    };

    use super::*;
    #[test]
    fn conditional_cvec_match_finds_conditional_rewrites() {
        let bubbler =
            Bubbler::<LLVMLang>::new(BubblerConfig::new(vec!["x".into()], vec![-1, 0, 1]));
        let mut backend = EgglogBackend::<LLVMLang>::new();
        backend
            .register(
                &Rewrite::new(
                    Some(PredicateTerm::from_term(Term::Call(
                        LLVMLangOp::Lt,
                        vec![Term::Var("x".into()), Term::Const(0)],
                    ))),
                    Term::Call(
                        LLVMLangOp::Div,
                        vec![Term::Var("x".into()), Term::Var("x".into())],
                    ),
                    Term::Const(1),
                )
                .unwrap(),
            )
            .unwrap();

        backend
            .register_implication(&Implication {
                from: PredicateTerm::from_term(Term::Call(
                    LLVMLangOp::Lt,
                    vec![Term::Var("x".into()), Term::Var("y".into())],
                ))
                .into(),
                to: PredicateTerm::from_term(Term::Call(
                    LLVMLangOp::Neq,
                    vec![Term::Var("x".into()), Term::Var("y".into())],
                )),
            })
            .unwrap();

        let x_div_x = Term::Call(
            LLVMLangOp::Div,
            vec![Term::Var("x".into()), Term::Var("x".into())],
        );

        backend
            .add_term(
                Term::Const(1),
                Some(Term::Const(1).evaluate(&bubbler.environment)),
            )
            .unwrap();

        backend
            .add_term(
                x_div_x.clone(),
                Some(x_div_x.evaluate(&bubbler.environment)),
            )
            .unwrap();

        let x_gt_0 = Term::Call(LLVMLangOp::Gt, vec![Term::Var("x".into()), Term::Const(0)]);
        backend
            .add_predicate(
                PredicateTerm::from_term(x_gt_0.clone()),
                Some(x_gt_0.evaluate(&bubbler.environment).to_pvec()),
            )
            .unwrap();

        let x_lt_0 = Term::Call(LLVMLangOp::Lt, vec![Term::Var("x".into()), Term::Const(0)]);
        backend
            .add_predicate(
                PredicateTerm::from_term(x_lt_0.clone()),
                Some(x_lt_0.evaluate(&bubbler.environment).to_pvec()),
            )
            .unwrap();

        let x_neq_0 = Term::Call(LLVMLangOp::Neq, vec![Term::Var("x".into()), Term::Const(0)]);
        backend
            .add_predicate(
                PredicateTerm::from_term(x_neq_0.clone()),
                Some(x_neq_0.evaluate(&bubbler.environment).to_pvec()),
            )
            .unwrap();

        // NOTE: the order here is pretty important. If you run implications first, then
        // rewrites, then the backend doesn't discover the `cond-equal` fact needed to prune
        // away redundant rewrites.
        backend.run_rewrites().unwrap();
        backend.run_implications().unwrap();

        let identifier = ConditionalCvecMatch::<LLVMLang>::new(IdentificationConfig {
            mode: IdentificationMode::Rewrites,
            validate_now: false,
        });

        let candidates = identifier.identify(&mut backend).unwrap();

        let InferredFacts::Rewrites(candidates) = candidates else {
            panic!("Expected rewrites inferred.");
        };

        assert_eq!(candidates.len(), 2);
        assert!(
            candidates.contains(
                &Rewrite::new(
                    Some(PredicateTerm::from_term(x_gt_0)),
                    x_div_x.clone(),
                    Term::Const(1),
                )
                .unwrap()
            )
        );

        assert!(
            candidates.contains(
                &Rewrite::new(
                    Some(PredicateTerm::from_term(x_neq_0)),
                    x_div_x.clone(),
                    Term::Const(1),
                )
                .unwrap()
            )
        );
    }
}
