use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use egglog::EGraph;

use egglog::ast::Rule;

use crate::language::implication::Implication;
use crate::language::rule::Rewrite;
use crate::language::{CVec, Environment, Language, Term};

pub(crate) const GET_CVEC_FN: &str = "get-cvec";

/// This relation records pairs of terms which external validation
/// has determined to not ever be equal.
pub(crate) const NOT_EQUAL_FN: &str = "not-equal";

pub(crate) const HASH_CODE_FN: &str = "HashCode";

#[macro_export]
macro_rules! run_prog {
    ($egraph:expr, $prog:expr) => {{
        println!("Running egglog program:\n{}", $prog);
        $egraph.parse_and_run_program(None, $prog)
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
        let mut bubbler = Self {
            egraph,
            environment,
            cache: CVecCache::<L>::new(),
            rules: vec![],
            implications: vec![],
            schedule: BubblerSchedule::default(),
        };

        bubbler.initialize_egraph();
        bubbler
    }

    /// Given a blank e-graph for some language L, populate it with the
    /// necessary machinery to do Bubbler.
    /// This includes:
    /// - The datatype definition for L.
    /// - The datatype definition for cvecs.
    fn initialize_egraph(&mut self) {
        todo!()
        // run_prog!(self.egraph, &L::to_egglog_src()).unwrap();
        // let name = L::name();
        // run_prog!(
        //     self.egraph,
        //     format!(
        //         r#"
        //     (datatype cvec ({HASH_CODE_FN} String))

        //     ;;; A relation that associates terms with their characteristic vectors.
        //     ;;; If two things are merged, then their cvecs must be the same.
        //     (function {GET_CVEC_FN} ({name}) cvec :no-merge)

        //     ;;; If Bubbler discovers that two terms are not equal (through
        //     ;;; validation), then we record that information here.
        //     (relation {NOT_EQUAL_FN} ({name} {name} {name}))
        //     "#
        //     )
        //     .as_str()
        // )
        // .unwrap();
    }

    /// Adds the term to the e-graph, erroring if the term is malformed.
    pub fn add_term(&mut self, term: &Term<L>) -> Result<CVec<L>, egglog::Error> {
        let sexp = term.to_sexp();
        let cvec = term.evaluate(&self.environment);
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        cvec.hash(&mut hasher);
        let hash = hasher.finish();

        // Cache the cvec.
        if let Some(prev) = self.cache.get(&hash) {
            assert_eq!(
                prev, &cvec,
                "Hash collision detected for cvecs: {prev:?} and {cvec:?}"
            );
        }
        self.cache.insert(hash, cvec.clone());

        let egglog_prog = format!(
            r#"
            {sexp}
            (set ({GET_CVEC_FN} {sexp}) ({HASH_CODE_FN} "{hash}"))
        "#
        );
        run_prog!(self.egraph, &egglog_prog)?;
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

mod tests {
    use crate::language::BubbleLang;

    use super::*;

    #[allow(dead_code)]
    fn get_cfg() -> BubblerConfig<BubbleLang> {
        BubblerConfig {
            vars: vec!["x".into(), "y".into()],
            vals: vec![0, 1],
            _marker: std::marker::PhantomData,
        }
    }

    // #[test]
    // fn bubbler_egraph_ok() {
    //     use super::Bubbler;
    //     use crate::language::BubbleLang;

    //     let mut bubbler: Bubbler<BubbleLang> = Bubbler::new(get_cfg());
    //     bubbler.add_term(&BubbleLang::Const(42)).unwrap();
    //     let cvec = bubbler.lookup_cvec(&BubbleLang::Int(42)).unwrap();
    //     for v in &cvec {
    //         assert_eq!(*v, Some(42_i64));
    //     }
    //     // `BubbleLang::make_environment(&["x".into(), "y".into()])`
    //     // produces 7 * 7 = 49 entries, which is the size of the
    //     // cartesian product of the constant values for x and y.
    //     assert_eq!(cvec.len(), 7 * 7);
    // }
}
