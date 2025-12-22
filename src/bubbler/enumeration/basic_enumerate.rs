use ruler::enumo::Workload;

use crate::{
    bubbler::schedule::Enumeration,
    language::{
        term::{PredicateTerm, Term},
        Language,
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
        bubbler: &mut crate::bubbler::Bubbler<L>,
        workload: ruler::enumo::Workload,
    ) -> Result<(), String> {
        let cfg = self.cfg.clone();
        for sexp in workload.force() {
            let term: Term<L> = Term::from_sexp(&sexp)?;
            match cfg.mode {
                EnumerationMode::Terms => {
                    bubbler.add_term(&term, cfg.evaluate)?;
                }
                EnumerationMode::Predicates => {
                    let predicate = PredicateTerm::from_term(term);
                    bubbler.add_predicate(&predicate, cfg.evaluate)?;
                }
                _ => todo!(),
            }
        }
        Ok(())
    }
}
