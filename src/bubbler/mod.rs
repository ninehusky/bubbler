use std::hash::{Hash, Hasher};

use egglog::EGraph;

use crate::language::rule::{Implication, Rewrite};
use crate::language::{CVec, Environment, Language};

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

pub type CVecCache<L> = std::collections::HashMap<u64, CVec<L>>;

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
        run_prog!(self.egraph, &L::to_egglog_src()).unwrap();
        let name = L::name();
        run_prog!(
            self.egraph,
            format!(
                r#"
            (datatype cvec (HashCode String))

            ;;; A relation that associates terms with their characteristic vectors.
            ;;; If two things are merged, then their cvecs must be the same.
            (function get-cvec ({name}) cvec :no-merge)
            "#
            )
            .as_str()
        )
        .unwrap();
    }

    /// Adds the term to the e-graph, erroring if the term is malformed.
    pub fn add_term(&mut self, term: &L) -> Result<Vec<String>, egglog::Error> {
        let sexp = term.to_sexp();
        let cvec = term.evaluate(&self.environment);
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        cvec.hash(&mut hasher);
        let hash = hasher.finish();

        // Cache the cvec.
        self.cache.insert(hash, cvec);

        let egglog_prog = format!(
            r#"
            {sexp}
            (set (get-cvec {sexp}) (HashCode "{hash}"))
        "#
        );
        let results = run_prog!(self.egraph, &egglog_prog)?;
        Ok(results
            .into_iter()
            .map(|output| output.to_string())
            .collect())
    }

    /// Returns the characteristic vector for a given term, if it exists.
    /// Errors if the term is not in the e-graph.
    pub fn lookup_cvec(&mut self, term: &L) -> Result<CVec<L>, &'static str> {
        let sexp = term.to_sexp();
        let egglog_prog = format!(
            r#"
            (extract (get-cvec {sexp}))
        "#
        );
        let result = self.egraph.parse_and_run_program(None, &egglog_prog);

        match result {
            Ok(res) => {
                if res.is_empty() {
                    Err("Term not found in e-graph.")
                } else {
                    assert_eq!(res.len(), 1, "Termlookup returned multiple cvec results.");
                    println!("Result: {res:?}");
                    let hash_code: u64 = res[0]
                        .to_string()
                        .trim()
                        .trim_start_matches("(HashCode \"")
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

    #[test]
    fn bubbler_egraph_ok() {
        use super::Bubbler;
        use crate::language::BubbleLang;

        let mut bubbler: Bubbler<BubbleLang> = Bubbler::new(get_cfg());
        bubbler.add_term(&BubbleLang::Int(42)).unwrap();
        let cvec = bubbler.lookup_cvec(&BubbleLang::Int(42)).unwrap();
        for v in &cvec {
            assert_eq!(*v, Some(42_i64));
        }
        // `BubbleLang::make_environment(&["x".into(), "y".into()])`
        // produces 7 * 7 = 49 entries, which is the size of the
        // cartesian product of the constant values for x and y.
        assert_eq!(cvec.len(), 7 * 7);
    }
}
