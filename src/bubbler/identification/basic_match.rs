use crate::{
    bubbler::{backend::EgglogBackend, schedule::Identification, Bubbler, InferredFacts},
    colors::implication::Implication,
    language::{rewrite::Rewrite, Language, PVec},
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
}

impl<L: Language> Identification<L> for PvecMatch<L> {
    fn identify(&self, backend: &mut EgglogBackend<L>) -> Result<InferredFacts<L>, String> {
        let pvec_map = backend.get_pvec_map();

        let pvecs = pvec_map.keys().cloned();

        let mut matching_pvecs: Vec<(PVec, PVec)> = vec![];

        for pvec1 in pvecs.clone() {
            for pvec2 in pvecs.clone() {
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
                    let implication = Implication {
                        from: from_term.clone(),
                        to: to_term.clone(),
                    };
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
            enumeration::{basic_enumerate::BasicEnumerate, EnumerationConfig, EnumerationMode},
            identification::{IdentificationConfig, IdentificationMode},
            schedule::{BubblerAction, Enumeration, Identification},
            BubblerConfig,
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
