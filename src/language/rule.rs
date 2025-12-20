use std::{collections::HashMap, fmt::Display, str::FromStr, sync::Arc};

use super::{CVec, Language};
use crate::{
    bubbler::{Bubbler, CVecCache, GET_CVEC_FN},
    language::{sexp::Sexp, Term},
    run_prog,
};
use egglog::{util::IndexMap, CommandOutput, EGraph};

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Rewrite<L: Language> {
    Conditional {
        cond: Term<L>,
        lhs: Term<L>,
        rhs: Term<L>,
    },
    Unconditional {
        lhs: Term<L>,
        rhs: Term<L>,
    },
}

impl<L: Language> Display for Rewrite<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Rewrite::Conditional { cond, lhs, rhs } => {
                write!(
                    f,
                    "if {} then {} ~> {}",
                    cond.to_sexp(),
                    lhs.to_sexp(),
                    rhs.to_sexp()
                )
            }
            Rewrite::Unconditional { lhs, rhs } => {
                write!(f, "{} ~> {}", lhs.to_sexp(), rhs.to_sexp())
            }
        }
    }
}

impl<L: Language> Rewrite<L> {
    // NOTE: do not make another constructor for this that is _not_ generalized,
    // without a very good, very documented reason.
    pub fn new(cond: Option<Term<L>>, lhs: Term<L>, rhs: Term<L>) -> Self {
        let mut map = HashMap::new();
        let cond = cond.map(|c| {
            c.generalize(&mut map)
                .expect("Failed to generalize condition.")
        });

        let lhs = lhs.generalize(&mut map).expect("Failed to generalize LHS.");
        let rhs = rhs.generalize(&mut map).expect("Failed to generalize RHS.");

        match cond {
            Some(c) => Self::Conditional { cond: c, lhs, rhs },
            None => Self::Unconditional { lhs, rhs },
        }
    }
}

pub struct RewriteSet<L: Language>(pub IndexMap<Arc<str>, Rewrite<L>>);

impl<L: Language> RewriteSet<L> {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn add(&mut self, rw: Rewrite<L>) {
        match rw {
            Rewrite::Conditional {
                ref cond,
                ref lhs,
                ref rhs,
            } => {
                let key = format!(
                    "if {} then {} ~> {}",
                    cond.to_sexp(),
                    lhs.to_sexp(),
                    rhs.to_sexp()
                );
                self.0.insert(Arc::from(key), rw);
            }
            Rewrite::Unconditional { ref lhs, ref rhs } => {
                let key = format!("{} ~> {}", lhs.to_sexp(), rhs.to_sexp());
                self.0.insert(Arc::from(key), rw);
            }
        }
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl<L: Language> Default for RewriteSet<L> {
    fn default() -> Self {
        Self(IndexMap::default())
    }
}

fn get_term<L: Language>(relation: String) -> Result<Term<L>, String> {
    if !(relation.starts_with(format!("({GET_CVEC_FN}").as_str()) && relation.ends_with(')')) {
        return Err(format!(
            "Expected relation to start with '({})', got: {}",
            GET_CVEC_FN, relation
        ));
    }

    // chop off the "(get-cvec " and the ending ")"
    let relation =
        relation[..relation.len() - 1].trim_start_matches(format!("({} ", GET_CVEC_FN).as_str());

    println!("the relation is: {}", relation);

    let res = Term::<L>::from_sexp(&Sexp::from_str(relation)?)?;
    Ok(res)
}

pub fn cvec_match<L: Language>(
    egraph: &mut EGraph,
    cache: &CVecCache<L>,
) -> Result<RewriteSet<L>, String> {
    todo!()
    // let get_cvec_command = format!("(print-function {GET_CVEC_FN})");
    // let result = match run_prog!(egraph, &get_cvec_command) {
    //     Ok(res) => res,
    //     Err(_) => {
    //         return Err("Failed to run PrintFunction command.".into());
    //     }
    // };

    // if result.len() != 1 {
    //     panic!("Expected exactly one result from PrintFunction command.");
    // }

    // let mut by_cvec: IndexMap<CVec<L>, Vec<Term<L>>> = IndexMap::default();
    // let mut res = RewriteSet::default();

    // // 1. Go through all cvecs for un-merged (black) e-classes.
    // match &result[0] {
    //     CommandOutput::PrintFunction(_func, dag, tuples, _size) => {
    //         for (term, cvec) in tuples.iter() {
    //             let term = get_term::<L>(dag.to_string(term))?;
    //             let cvec = cache.lookup_from_str(&dag.to_string(cvec)).unwrap();
    //             by_cvec.entry(cvec.clone()).or_default().push(term);
    //         }
    //     }
    //     _ => unreachable!("Expected PrintFunctionOutput."),
    // }

    // // 2. Go pairwise through all terms with the same cvec and create rewrites
    // //    from them.
    // for (_, terms) in by_cvec.iter() {
    //     for lhs in terms.iter() {
    //         // Here, even though equality is symmetric, we want to consider both
    //         // directions separately. For some reason.
    //         for rhs in terms.iter() {
    //             if lhs == rhs {
    //                 continue;
    //             }

    //             // Check if lhs and rhs are in different e-classes.
    //             let lhs_egglog = Bubbler::egglogify(lhs);
    //             let rhs_egglog = Bubbler::egglogify(rhs);

    //             let check_neq_command = format!(
    //                 r#"
    //                 (extract (not-equal {lhs_egglog} {rhs_egglog}))
    //             "#
    //             );
    //             let neq_result = run_prog!(egraph, &check_neq_command);

    //             if neq_result.is_err() {
    //                 println!("{:?}", neq_result);
    //                 return Err("Failed to run not-equal check.".into());
    //             }

    //             if neq_result.unwrap().is_empty() {
    //                 // They are equal, skip.
    //                 continue;
    //             }

    //             // They are not equal, create a rewrite.
    //             let rewrite = Rewrite::new(None, lhs.clone(), rhs.clone());
    //             res.add(rewrite);
    //         }
    //     }
    // }

    // Ok(res)
}

// #[cfg(test)]
// mod cvec_match_tests {
//     use crate::{
//         bubbler::{Bubbler, BubblerConfig},
//         language::{BubbleLang, BubbleLangOp},
//     };

//     use super::*;

//     #[allow(dead_code)]
//     fn get_cfg() -> BubblerConfig<BubbleLang> {
//         BubblerConfig::new(vec!["x".into(), "y".into()], vec![0, 1])
//     }

//     #[test]
//     pub fn cvec_match_ok() {
//         let mut bubbler: Bubbler<BubbleLang> = Bubbler::new(get_cfg());
//         bubbler
//             .add_term(&Term::Call(
//                 BubbleLangOp::Add,
//                 vec![Term::Const(0), Term::Var("x".into())],
//             ))
//             .unwrap();

//         bubbler.add_term(&Term::Var("x".into())).unwrap();

//         let rws: RewriteSet<BubbleLang> = cvec_match(&mut bubbler.egraph, &bubbler.cache).unwrap();
//         for rw in rws.0.values() {
//             println!("Rewrite: {} ~> {}", rw.lhs.to_sexp(), rw.rhs.to_sexp());
//         }
//         // There should be two rewrite rules: one for the forward and one for the backward direction.
//         assert_eq!(rws.len(), 2);
//     }
// }
