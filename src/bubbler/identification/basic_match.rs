use crate::{
    bubbler::{schedule::Identification, Bubbler, InferredFacts},
    language::{rewrite::Rewrite, Language},
};

use super::IdentificationConfig;

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
    fn identify(&self, bubbler: &mut Bubbler<L>) -> Result<InferredFacts<L>, String> {
        if self.cfg.mode != super::IdentificationMode::Rewrites {
            return Err("CvecMatch only supports rewrite identification.".into());
        }
        let cvec_map = bubbler.backend.get_cvec_map();
        // Naively, any pairing of two terms with identical
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
        let mut bubbler = Bubbler::new(cfg);

        let enumeration_cfg: EnumerationConfig = EnumerationConfig {
            mode: EnumerationMode::Terms,
            evaluate: true,
        };

        let enumerate_act = BasicEnumerate::<LLVMLang>::new(enumeration_cfg);

        enumerate_act
            .enumerate_bubbler(
                &mut bubbler,
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

        let cvec_map = bubbler.backend.get_cvec_map();

        println!("Cvec Map:");
        for (cvec, terms) in cvec_map {
            println!("Cvec: {:?} => Terms:", cvec);
            for term in terms {
                println!("  {}", term);
            }
        }

        let identification_cfg: IdentificationConfig = IdentificationConfig {
            mode: IdentificationMode::Rewrites,
            validate_now: false,
        };

        let identification_act = CvecMatch::<LLVMLang>::new(identification_cfg);

        let inferred = identification_act.identify(&mut bubbler).unwrap();

        let InferredFacts::Rewrites(inferred) = inferred else {
            panic!("Expected rewrites inferred.");
        };

        for i in inferred {
            println!("Inferred Rewrite: {}", i);
        }
    }
}
