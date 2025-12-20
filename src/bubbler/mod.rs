use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use backend::EgglogBackend;
use egglog::ast::{Expr, GenericCommand};
use egglog::prelude::{span, RustSpan, Span};
use egglog::{call, EGraph};

use crate::colors::Implication;
use crate::language::rule::Rewrite;
use crate::language::{term::Term, CVec, Environment, Language};

pub(crate) const GET_CVEC_FN: &str = "get-cvec";
/// This relation records pairs of terms which external validation
/// has determined to not ever be equal.
pub(crate) const NOT_EQUAL_FN: &str = "not-equal";
pub(crate) const COND_EQUAL_FN: &str = "cond-equal";
pub(crate) const HASH_CODE_FN: &str = "HashCode";
pub(crate) const INVARIANT_RULESET: &str = "preserve-invariants";
pub(crate) const REWRITE_RULESET: &str = "bubbler-rewrites";
/// For matching on _any_ term/condition.
pub(crate) const UNIVERSAL_TERM_RELATION: &str = "universe-term";
pub(crate) const UNIVERSAL_PREDICATE_RELATION: &str = "universe-pred";

mod backend;

#[macro_export]
macro_rules! run_prog {
    ($egraph:expr, $prog:expr) => {{
        println!("Running egglog program:\n{}", $prog);
        $egraph
            .parse_and_run_program(None, $prog)
            .map_err(|e| e.to_string())
    }};
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

pub enum BubblerStep {
    FindRewrites,
    FindImplications,
}

/// One "step" of the Bubbler algorithm.
pub struct BubblerSchedule {
    pub steps: Vec<BubblerStep>,
}

impl Default for BubblerSchedule {
    fn default() -> Self {
        // Rewrites first, then implications.
        Self {
            steps: vec![BubblerStep::FindRewrites, BubblerStep::FindImplications],
        }
    }
}

pub struct CVecCache<L: Language>(pub HashMap<u64, CVec<L>>);

impl<L: Language> Default for CVecCache<L> {
    fn default() -> Self {
        Self::new()
    }
}

impl<L: Language> CVecCache<L> {
    pub fn lookup_from_str(&self, s: &str) -> Option<&CVec<L>> {
        let hash_code: u64 = s
            .trim()
            .trim_start_matches(format!("({} \"", HASH_CODE_FN).as_str())
            .trim_end_matches("\")")
            .parse()
            .ok()?;

        self.get(&hash_code)
    }

    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn insert(&mut self, hash: u64, cvec: CVec<L>) {
        self.0.insert(hash, cvec);
    }

    pub fn get(&self, hash: &u64) -> Option<&CVec<L>> {
        self.0.get(hash)
    }
}

/// The Bubbler struct, which manages a core Bubbler e-graph.
pub struct Bubbler<L: Language> {
    pub backend: EgglogBackend<L>,
    pub egraph: EGraph,
    pub environment: Environment<L>,
    pub cache: CVecCache<L>,
    pub rules: Vec<Rewrite<L>>,
    pub implications: Vec<Implication<L>>,
    pub schedule: BubblerSchedule,
}

impl<L: Language> Bubbler<L> {
    pub fn new(cfg: BubblerConfig<L>) -> Self {
        let egraph = EGraph::default();
        let environment = L::make_environment(&cfg.vars);
        let bubbler = Self {
            backend: EgglogBackend::<L>::new(),
            egraph,
            environment,
            cache: CVecCache::<L>::new(),
            rules: vec![],
            implications: vec![],
            schedule: BubblerSchedule::default(),
        };

        bubbler
    }

    /// Adds the given rule to the Bubbler's set of rewrite rules.
    /// Errors if the rule already exists.
    pub fn register(&mut self, rule: &Rewrite<L>) -> Result<(), String> {
        self.backend.register(rule)
    }

    /// Runs the Bubbler's rewrites on the e-graph.
    pub fn run_rewrites(&mut self, iterations: usize) -> Result<(), String> {
        let prog = format!(
            r#"
        (run {REWRITE_RULESET} {iterations})
        "#
        );
        run_prog!(self.egraph, &prog)?;
        Ok(())
    }

    /// Adds the condition to the e-graph, erroring if the condition is malformed.
    pub fn add_condition(&mut self, condition: &Term<L>) -> Result<(), String> {
        // if !condition.is_concrete() {
        //     return Err("Conditions must be concrete terms.".into());
        // }
        // let expr: Expr = condition.clone().into();
        // let expr: Expr = call!(PREDICATE_DATATYPE, vec![expr]);
        // self.egraph.run_program(vec![GenericCommand::Relation {
        //     span: span!(),
        //     name: UNIVERSAL_PREDICATE_RELATION.to_string(),
        //     inputs: expr,
        // }]);

        // let sexp = Bubbler::egglogify(condition);
        // let cond_prog = format!(
        //     r#"
        //     ({UNIVERSAL_PREDICATE_RELATION} (PredTerm {sexp}))
        // "#
        // );
        // run_prog!(self.egraph, &cond_prog)?;
        Ok(())
    }

    /// Adds the term to the e-graph, erroring if the term is malformed.
    pub fn add_term(&mut self, term: &Term<L>) -> Result<CVec<L>, String> {
        let cvec = term.evaluate(&self.environment);
        self.backend.add_term(term.clone(), Some(cvec.clone()))?;
        Ok(cvec)
    }

    /// Returns the characteristic vector for a given term, if it exists.
    /// Errors if the term is not in the e-graph.
    pub fn lookup_cvec(&mut self, term: &Term<L>) -> Result<CVec<L>, &'static str> {
        let sexp = term.to_sexp();
        let egglog_prog = format!(
            r#"
            (extract ({GET_CVEC_FN} {sexp}))
        "#
        );
        let result = self.egraph.parse_and_run_program(None, &egglog_prog);

        match result {
            Ok(res) => {
                if res.is_empty() {
                    Err("Term not found in e-graph.")
                } else {
                    assert_eq!(res.len(), 1, "Termlookup returned multiple cvec results.");
                    let hash_code: u64 = res[0]
                        .to_string()
                        .trim()
                        .trim_start_matches(format!("({} \"", HASH_CODE_FN).as_str())
                        .trim_end_matches("\")")
                        .parse()
                        .unwrap();

                    // I think it's okay to panic here. If the cvec is in the egraph but
                    // not in the cache, something has gone very wrong.
                    match self.cache.get(&hash_code) {
                        None => panic!("CVec not found in cache for hash code {hash_code}."),
                        Some(code) => Ok(code.clone()),
                    }
                }
            }
            Err(_) => Err("Failed to run egglog program."),
        }
    }
}

// mod tests {
//     #[allow(unused_imports)]
//     use crate::language::{term::Term, BubbleLang, BubbleLangOp};

//     use super::*;

//     #[allow(dead_code)]
//     fn get_cfg() -> BubblerConfig<BubbleLang> {
//         BubblerConfig {
//             vars: vec!["x".into(), "y".into()],
//             vals: vec![0, 1],
//             _marker: std::marker::PhantomData,
//         }
//     }

//     #[test]
//     fn bubbler_egraph_ok() {
//         use super::Bubbler;
//         use crate::language::BubbleLang;

//         let mut bubbler: Bubbler<BubbleLang> = Bubbler::new(get_cfg());
//         bubbler.add_term(&Term::Const(42)).unwrap();
//         let cvec = bubbler.lookup_cvec(&Term::Const(42)).unwrap();
//         for v in &cvec {
//             assert_eq!(*v, Some(42_i64));
//         }
//         // `BubbleLang::make_environment(&["x".into(), "y".into()])`
//         // produces 7 * 7 = 49 entries, which is the size of the
//         // cartesian product of the constant values for x and y.
//         assert_eq!(cvec.len(), 7 * 7);
//     }

//     #[test]
//     fn bubbler_egraph_fails_if_not_equal() {
//         use super::Bubbler;
//         use crate::language::BubbleLang;

//         let mut bubbler: Bubbler<BubbleLang> = Bubbler::new(get_cfg());
//         bubbler.add_term(&Term::Const(42)).unwrap();
//         bubbler.add_term(&Term::Const(43)).unwrap();

//         let res = bubbler.egraph.parse_and_run_program(
//             None,
//             r#"
//             (not-equal (Const 42) (Const 43))
//             (union (Const 42) (Const 43))
//             (run 10)
//         "#,
//         );

//         assert!(res.is_err());

//         let e = res.err().unwrap();

//         assert!(e.to_string().contains("Illegal merge"));
//     }

//     // TODO Maxim: it would be a good idea to make this a doctest.
//     #[test]
//     fn egglogify_transforms_vars_to_holes() {
//         let r: Rewrite<BubbleLang> = Rewrite::new(
//             None,
//             Term::Call(
//                 BubbleLangOp::Add,
//                 vec![Term::Var("x".into()), Term::Const(1)],
//             ),
//             Term::Call(
//                 BubbleLangOp::Add,
//                 vec![Term::Const(1), Term::Var("x".into())],
//             ),
//         );
//         let sexp_lhs = Bubbler::egglogify(&r.lhs);
//         let sexp_rhs = Bubbler::egglogify(&r.rhs);
//         assert_eq!(
//             sexp_lhs.to_string().replace(" ", ""),
//             "(Add ?a (Const 1))".replace(" ", "")
//         );
//         assert_eq!(
//             sexp_rhs.to_string().replace(" ", ""),
//             "(Add (Const 1) ?a)".replace(" ", "")
//         );
//     }

//     #[test]
//     fn conditional_rewrite() {
//         // x / x ~> 1 if x != 0
//         let r: Rewrite<BubbleLang> = Rewrite::new(
//             Some(Term::Call(
//                 BubbleLangOp::Neq,
//                 vec![Term::Var("x".into()), Term::Const(0)],
//             )),
//             Term::Call(
//                 BubbleLangOp::Div,
//                 vec![Term::Var("x".into()), Term::Var("x".into())],
//             ),
//             Term::Const(1),
//         );

//         let mut bubbler: Bubbler<BubbleLang> = Bubbler::new(get_cfg());
//         bubbler.register(&r).unwrap();

//         bubbler
//             .add_condition(&Term::Call(
//                 BubbleLangOp::Neq,
//                 vec![Term::Var("x".into()), Term::Const(0)],
//             ))
//             .unwrap();

//         bubbler
//             .add_term(&Term::Call(
//                 BubbleLangOp::Div,
//                 vec![Term::Var("x".into()), Term::Var("x".into())],
//             ))
//             .unwrap();

//         bubbler.run_rewrites(7).unwrap();

//         assert!(bubbler.egraph.parse_and_run_program(None,
//             format!("(check ({COND_EQUAL_FN} (PredTerm (Neq (Var \"x\") (Const 0))) (Div (Var \"x\") (Var \"x\")) (Const 1)))").as_str()).is_ok());
//     }

//     #[test]
//     fn total_rewrite() {
//         let r: Rewrite<BubbleLang> = Rewrite::new(
//             None,
//             Term::Call(
//                 BubbleLangOp::Add,
//                 vec![Term::Var("x".into()), Term::Var("y".into())],
//             ),
//             Term::Call(
//                 BubbleLangOp::Add,
//                 vec![Term::Var("y".into()), Term::Var("x".into())],
//             ),
//         );

//         let mut bubbler: Bubbler<BubbleLang> = Bubbler::new(get_cfg());
//         bubbler.register(&r).unwrap();
//         bubbler
//             .add_term(&Term::Call(
//                 BubbleLangOp::Add,
//                 vec![Term::Var("x".into()), Term::Var("y".into())],
//             ))
//             .unwrap();

//         bubbler.run_rewrites(7).unwrap();

//         assert!(bubbler
//             .egraph
//             .parse_and_run_program(
//                 None,
//                 format!("(check (= (Add (Var \"x\") (Var \"y\")) (Add (Var \"y\") (Var \"x\"))))")
//                     .as_str()
//             )
//             .is_ok());
//     }
// }
