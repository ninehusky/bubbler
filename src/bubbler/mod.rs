use backend::EgglogBackend;
use schedule::BubblerAction;

use crate::colors::implication::Implication;
use crate::language::constant::BubbleConstant;
use crate::language::rewrite::Rewrite;
use crate::language::term::PredicateTerm;
use crate::language::{term::Term, CVec, Environment, Language};

mod backend;
mod enumeration;
mod identification;
mod schedule;

pub struct BubblerConfig<L: Language> {
    pub vars: Vec<String>,
    pub vals: Vec<L::Constant>,
    _marker: std::marker::PhantomData<L>,
}

impl<L: Language> BubblerConfig<L> {
    pub fn new(vars: Vec<String>, vals: Vec<L::Constant>) -> Self {
        Self {
            vars,
            vals,
            _marker: std::marker::PhantomData,
        }
    }
}

/// The Bubbler struct, which manages a core Bubbler e-graph.
pub struct Bubbler<L: Language> {
    pub backend: EgglogBackend<L>,
    pub environment: Environment<L>,
    pub rules: Vec<Rewrite<L>>,
    pub implications: Vec<Implication<L>>,
}

impl<L: Language> Bubbler<L> {
    pub fn new(cfg: BubblerConfig<L>) -> Self {
        let environment = L::make_environment(&cfg.vars);

        Self {
            backend: EgglogBackend::<L>::new(),
            environment,
            rules: vec![],
            implications: vec![],
        }
    }

    pub fn run_implications(&mut self) {
        self.backend.run_implications().unwrap();
    }

    // pub fn run_action(&mut self, action: BubblerAction<L>) {
    //     match action {
    //         BubblerAction::Enumeration(act) => {
    //             act.enumerate(self, ruler::enumo::Workload::default())
    //                 .unwrap();
    //         }
    //         BubblerAction::Identification(act) => {
    //             let inferred = act.identify(self).unwrap();
    //             match inferred {
    //                 InferredFacts::Implications(imps) => {
    //                     self.implications.extend(imps);
    //                 }
    //                 InferredFacts::Rewrites(rws) => {
    //                     self.rules.extend(rws);
    //                 }
    //             }
    //         }
    //         BubblerAction::Minimization(act) => {
    //             let candidates = InferredFacts::Rewrites(self.rules.clone());
    //             let minimized = act.minimize(self, candidates).unwrap();
    //             match minimized {
    //                 InferredFacts::Implications(imps) => {
    //                     self.implications = imps;
    //                 }
    //                 InferredFacts::Rewrites(rws) => {
    //                     self.rules = rws;
    //                 }
    //             }
    //         }
    //     }
    // }

    /// Adds the predicate to the e-graph. If `add_pvec` is true, the
    /// `predicate`'s `[PVec]` with respect to the Bubbler's environment
    /// will be added to the e-graph as well.
    pub fn add_predicate(
        &mut self,
        predicate: &PredicateTerm<L>,
        add_pvec: bool,
    ) -> Result<(), String> {
        if !predicate.term.is_concrete() {
            return Err("Predicates must be concrete.".into());
        }

        let pvec = if add_pvec {
            Some(
                predicate
                    .term
                    .evaluate(&self.environment)
                    .iter()
                    .map(|c| match c {
                        None => false,
                        Some(c) => {
                            let bc: BubbleConstant = L::constant_to_bubble(c);
                            bc.to_bool()
                        }
                    })
                    .collect(),
            )
        } else {
            None
        };

        self.backend.add_predicate(predicate.clone(), pvec).unwrap();
        Ok(())
    }

    /// Adds the term to the e-graph. If `add_cvec` is true, the
    /// `term`'s `[CVec]` with respect to the Bubbler's environment
    /// will be added to the e-graph as well.
    pub fn add_term(&mut self, term: &Term<L>, add_cvec: bool) -> Result<(), String> {
        if !term.is_concrete() {
            return Err("Terms must be concrete.".into());
        }

        let cvec: Option<CVec<L>> = if add_cvec {
            Some(term.evaluate(&self.environment))
        } else {
            None
        };

        self.backend.add_term(term.clone(), cvec).unwrap();
        Ok(())
    }
}

/// The stuff that Bubbler figures out.
#[derive(Debug, Clone)]
pub enum InferredFacts<L: Language> {
    Implications(Vec<Implication<L>>),
    Rewrites(Vec<Rewrite<L>>),
}
