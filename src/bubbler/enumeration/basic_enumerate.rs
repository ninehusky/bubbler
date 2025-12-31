
use crate::{
    bubbler::{backend::EgglogBackend, schedule::Enumeration},
    language::{
        term::{PredicateTerm, Term},
        Environment, Language,
    },
};

use super::{EnumerationConfig, EnumerationMode};

/// Adds all of the terms in the workload to the e-graph.
pub struct BasicEnumerate<L: Language> {
    pub cfg: EnumerationConfig,
    pub env: Environment<L>,
    _marker: std::marker::PhantomData<L>,
}

impl<L: Language> BasicEnumerate<L> {
    pub fn new(cfg: EnumerationConfig, env: Environment<L>) -> BasicEnumerate<L> {
        Self {
            cfg,
            env,
            _marker: std::marker::PhantomData::<L>,
        }
    }
}

impl<L: Language> Enumeration<L> for BasicEnumerate<L> {
    fn enumerate_bubbler(
        &self,
        backend: &mut EgglogBackend<L>,
        workload: ruler::enumo::Workload,
    ) -> Result<(), String> {
        let cfg = self.cfg.clone();
        for sexp in workload.force() {
            let term: Term<L> = Term::from_sexp(&sexp)?;
            let cvec = if cfg.evaluate {
                Some(term.evaluate(&self.env))
            } else {
                None
            };
            match cfg.mode {
                EnumerationMode::Terms => {
                    backend.add_term(term, cvec)?;
                }
                EnumerationMode::Predicates => {
                    let pvec = cvec.map(|cvec| cvec.to_pvec());
                    let predicate = PredicateTerm::from_term(term);
                    backend.add_predicate(predicate, pvec)?;
                }
                _ => todo!(),
            }
        }
        Ok(())
    }
}
