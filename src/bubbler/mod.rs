use backend::EgglogBackend;
use egglog::SerializeConfig;
use identification::{
    ConditionalCvecMatch, CvecMatch, IdentificationConfig, IdentificationMode, PvecMatch,
};
use minimization::score_fns::implication_score_fns;
use minimization::BasicImplicationMinimize;
use ruler::enumo::Workload;
use schedule::{Enumeration, Identification, Minimization};

use crate::colors::implication::Implication;
use crate::language::constant::BubbleConstant;
use crate::language::rewrite::Rewrite;
use crate::language::term::PredicateTerm;
use crate::language::{term::Term, CVec, Environment, Language};

mod backend;
mod enumeration;
mod identification;
mod minimization;
mod schedule;

use enumeration::BasicEnumerate;

type RewriteFacts<L> = InferredFacts<L>;
type ImplicationFacts<L> = InferredFacts<L>;

/// The stuff that Bubbler figures out.
#[derive(Debug, Clone)]
pub enum InferredFacts<L: Language> {
    Implications(Vec<Implication<L>>),
    Rewrites(Vec<Rewrite<L>>),
}

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

/// The Bubbler. This is the thing you use to run a `[BubblerSchedule]`, which
/// defines how to explore a domain.
pub struct Bubbler<L: Language> {
    pub environment: Environment<L>,
    pub rules: Vec<Rewrite<L>>,
    pub implications: Vec<Implication<L>>,
}

impl<L: Language> Bubbler<L> {
    pub fn new(cfg: BubblerConfig<L>) -> Self {
        let environment = L::make_environment(&cfg.vars);

        Self {
            environment,
            rules: vec![],
            implications: vec![],
        }
    }

    /// Find implications in the given `workload` of predicates. Uses the Bubbler's
    /// current inferred rules and implications to help prune redundant implications.
    /// Returns the implications found.
    pub fn find_implications(&self, wkld: &Workload) -> ImplicationFacts<L> {
        let mut backend = self.new_backend();

        // 1. Enumerate predicates in the workload.
        let enumerator = BasicEnumerate::new(
            enumeration::EnumerationConfig {
                mode: enumeration::EnumerationMode::Predicates,
                evaluate: true,
            },
            self.environment.clone(),
        );

        enumerator
            .enumerate_bubbler(&mut backend, wkld.clone())
            .unwrap();

        backend
            .egraph
            .parse_and_run_program(None, r#"(check (Ge (Var "x") (Var "y")))"#)
            .unwrap();

        backend
            .egraph
            .parse_and_run_program(None, r#"(check (Le (Var "y") (Var "x")))"#)
            .unwrap();

        backend
            .egraph
            .parse_and_run_program(
                None,
                r#"(rewrite (Le ?a ?b) (Ge ?b ?a) :ruleset term-rewrites)"#,
            )
            .unwrap();
        // 2. Run the existing rules and implications.
        backend.run_rewrites().unwrap();
        backend.run_implications().unwrap();

        // the above two terms should be equivalent now.
        backend
            .egraph
            .parse_and_run_program(
                None,
                r#"(check (= (Le (Var "y") (Var "x")) (Ge (Var "x") (Var "y"))))"#,
            )
            .unwrap();

        // this fails?

        // 3. Identify candidates for implications from the predicates.
        let imp_candidates = PvecMatch::new(IdentificationConfig {
            mode: IdentificationMode::Implications,
            validate_now: false,
        })
        .identify(&mut backend)
        .unwrap();

        let score_fn = implication_score_fns::prioritize_vars();

        // 4. Using what we know, minimize the candidates to only keep the best ones.
        let minimizer: BasicImplicationMinimize<L> =
            BasicImplicationMinimize::new(Box::new(score_fn), self.implications.clone(), 1);

        let found = minimizer.minimize(&mut backend, imp_candidates).unwrap();

        let InferredFacts::Implications(imps) = found else {
            panic!("Implication minimizer returned non-implication facts");
        };

        InferredFacts::Implications(imps)
    }

    /// Using the Bubbler's current inferred rules and implications,
    /// discover new rewrites from the given `wkld` and `pred_wkld`
    /// and add them to the Bubbler's knowledge base.
    /// We will almost certainly change this to have more granularity.
    /// The values returned are the discovered total and conditional rewrites.
    pub fn find_rewrites(
        &mut self,
        wkld: &Workload,
        pred_wkld: &Workload,
    ) -> (RewriteFacts<L>, RewriteFacts<L>) {
        // 1. Enumerate terms and conditions in the workload.
        let mut backend = self.new_backend();

        let enumerator = BasicEnumerate::new(
            enumeration::EnumerationConfig {
                mode: enumeration::EnumerationMode::Terms,
                evaluate: true,
            },
            self.environment.clone(),
        );

        enumerator
            .enumerate_bubbler(&mut backend, wkld.clone())
            .unwrap();

        let enumerator = BasicEnumerate::new(
            enumeration::EnumerationConfig {
                mode: enumeration::EnumerationMode::Predicates,
                evaluate: true,
            },
            self.environment.clone(),
        );

        enumerator
            .enumerate_bubbler(&mut backend, pred_wkld.clone())
            .unwrap();

        // 2. Run the rules and implications.
        backend.run_rewrites().unwrap();
        backend.run_implications().unwrap();

        // 3. Identify total rules from the workload.
        let identifier = CvecMatch::new(IdentificationConfig {
            mode: IdentificationMode::Rewrites,
            validate_now: false,
        });

        let total_candidates = identifier.identify(&mut backend).unwrap();

        // 4. Minimize the total candidates.
        let minimizer = minimization::BasicRewriteMinimize::new(
            Box::new(minimization::score_fns::rewrite_score_fns::prioritize_vars()),
            self.rules.clone(),
            1,
        );

        let total_rewrites = minimizer.minimize(&mut backend, total_candidates).unwrap();

        // 5. Add the new total rewrites to the backend.
        let InferredFacts::Rewrites(total_rws) = total_rewrites else {
            panic!("Rewrite minimizer returned non-rewrite facts");
        };

        // 6. Identify conditional rules from the workload.
        let identifier = ConditionalCvecMatch::new(IdentificationConfig {
            mode: IdentificationMode::Rewrites,
            validate_now: false,
        });

        let cond_candidates = identifier.identify(&mut backend).unwrap();

        // 7. Minimize the conditional candidates.
        let minimizer = minimization::BasicRewriteMinimize::new(
            Box::new(minimization::score_fns::rewrite_score_fns::prioritize_vars()),
            self.rules.clone(),
            1,
        );

        let cond_rewrites = minimizer.minimize(&mut backend, cond_candidates).unwrap();

        let InferredFacts::Rewrites(cond_rws) = cond_rewrites else {
            panic!("Rewrite minimizer returned non-rewrite facts");
        };

        (
            InferredFacts::Rewrites(total_rws),
            InferredFacts::Rewrites(cond_rws),
        )
    }

    /// Returns a new `EgglogBackend` that has been initialized with the Bubbler's
    /// inferred rules and implications.
    pub fn new_backend(&self) -> EgglogBackend<L> {
        let mut backend = EgglogBackend::<L>::new();
        backend.set_environment(self.environment.clone());
        for rule in &self.rules {
            backend.register(rule).unwrap();
        }

        for imp in &self.implications {
            backend.register_implication(imp).unwrap();
        }

        backend
    }

    pub fn register_implication(&mut self, imp: &Implication<L>) -> Result<(), String> {
        self.implications.push(imp.clone());
        Ok(())
    }

    pub fn register_rewrite(&mut self, rw: &Rewrite<L>) -> Result<(), String> {
        self.rules.push(rw.clone());
        Ok(())
    }

    pub fn run_implications(&self, backend: &mut EgglogBackend<L>) {
        backend.run_implications().unwrap();
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
        &self,
        backend: &mut EgglogBackend<L>,
        predicate: &PredicateTerm<L>,
        add_pvec: bool,
    ) -> Result<(), String> {
        if !predicate.term.is_concrete() {
            return Err("Predicates must be concrete.".into());
        }

        backend.add_predicate(predicate.clone(), add_pvec).unwrap();
        Ok(())
    }

    /// Adds the term to the e-graph. If `add_cvec` is true, the
    /// `term`'s `[CVec]` with respect to the Bubbler's environment
    /// will be added to the e-graph as well.
    pub fn add_term(
        &self,
        backend: &mut EgglogBackend<L>,
        term: &Term<L>,
        add_cvec: bool,
    ) -> Result<(), String> {
        if !term.is_concrete() {
            return Err("Terms must be concrete.".into());
        }

        backend.add_term(term.clone(), add_cvec).unwrap();
        Ok(())
    }
}
