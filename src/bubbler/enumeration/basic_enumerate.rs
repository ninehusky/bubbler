use crate::{
    bubbler::{backend::EgglogBackend, schedule::Enumeration},
    language::{
        Language,
        term::{PredicateTerm, Term},
    },
};

use super::{EnumerationConfig, EnumerationMode};

/// Adds all of the terms in the workload to the e-graph.
pub struct BasicEnumerate<L: Language> {
    pub cfg: EnumerationConfig,
    _marker: std::marker::PhantomData<L>,
}

impl<L: Language> BasicEnumerate<L> {
    pub fn new(cfg: EnumerationConfig) -> BasicEnumerate<L> {
        Self {
            cfg,
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
            match cfg.mode {
                EnumerationMode::Terms => {
                    backend.add_term(term, cfg.evaluate)?;
                }
                EnumerationMode::Predicates => {
                    let predicate = PredicateTerm::from_term(term);
                    backend.add_predicate(predicate, cfg.evaluate)?;
                }
            }
        }
        Ok(())
    }
}
