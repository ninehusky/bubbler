use std::{collections::HashMap, str::FromStr, sync::Arc};

use super::{CVec, Language};
use crate::{
    bubbler::{CVecCache, GET_CVEC_FN},
    language::{sexp::Sexp, Term},
    run_prog,
};
use egglog::{util::IndexMap, CommandOutput, EGraph};

/// The type of rewrite: concrete or generalized.
/// You usually want rewrites to be generalized, but in
/// certain situations (e.g., validation by SMT),
/// you need to translate these to concrete rewrites.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RewriteType {
    Concrete,
    Generalized,
}

#[derive(Clone)]
pub struct Rewrite<L: Language> {
    pub rw_type: RewriteType,
    pub cond: Option<L>,
    pub lhs: L,
    pub rhs: L,
}

impl<L: Language> Rewrite<L> {
    pub fn new_concrete(cond: Option<L>, lhs: L, rhs: L) -> Self {
        Self {
            rw_type: RewriteType::Concrete,
            cond,
            lhs,
            rhs,
        }
    }

    pub fn new_generalized(cond: Option<L>, lhs: L, rhs: L) -> Self {
        Self {
            rw_type: RewriteType::Generalized,
            cond,
            lhs,
            rhs,
        }
    }

    pub fn generalize(&self) -> Self {
        match self.rw_type {
            RewriteType::Concrete => Self {
                rw_type: RewriteType::Generalized,
                cond: self.cond.clone(),
                lhs: self.lhs.clone(),
                rhs: self.rhs.clone(),
            },
            RewriteType::Generalized => self.clone(),
        }
    }

    pub fn concretize(&self) -> Self {
        match self.rw_type {
            RewriteType::Generalized => Self {
                rw_type: RewriteType::Concrete,
                cond: self.cond.clone(),
                lhs: self.lhs.clone(),
                rhs: self.rhs.clone(),
            },
            RewriteType::Concrete => self.clone(),
        }
    }
}

pub struct RewriteSet<L: Language>(pub IndexMap<Arc<str>, Rewrite<L>>);

impl<L: Language> RewriteSet<L> {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
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

// pub fn cvec_match<L: Language>(
//     egraph: &mut EGraph,
//     cache: &CVecCache<L>,
// ) -> Result<RewriteSet<L>, String> {
//     let get_cvec_command = format!("(print-function {GET_CVEC_FN})");
//     let result = match run_prog!(egraph, &get_cvec_command) {
//         Ok(res) => res,
//         Err(_) => {
//             return Err("Failed to run PrintFunction command.".into());
//         }
//     };

//     if result.len() != 1 {
//         panic!("Expected exactly one result from PrintFunction command.");
//     }

//     let mut by_cvec: IndexMap<CVec<L>, Vec<L>> = IndexMap::default();

//     // 1. Go through all cvecs for un-merged (black) e-classes.
//     match &result[0] {
//         CommandOutput::PrintFunction(_func, dag, tuples, _size) => {
//             for (term, cvec) in tuples.iter() {
//                 let term = get_term::<L>(dag.to_string(term))?;
//                 let cvec = cache.lookup_from_str(&dag.to_string(cvec)).unwrap();
//                 by_cvec.entry(cvec.clone()).or_default().push(term);
//             }
//         }
//         _ => unreachable!("Expected PrintFunctionOutput."),
//     }

//     // 2. Go pairwise through all terms with the same cvec and create rewrites
//     //    from them.
//     //    For each rule:
//     //    - see if not equal by pinging egraph for (not-equal lhs rhs)
//     //    - generalize the rule
//     //    - add (no-op if already exists) to RewriteSet
//     for (cvec, terms) in by_cvec.iter() {
//         for term1 in terms.iter() {
//             // Here, even though equality is symmetric, we want to consider both
//             // directions separately. For some reason.
//             for term2 in terms.iter() {
//                 // Check if lhs and rhs are in different e-classes.
//                 let check_neq_command = format!(
//                     r#"
//                     (extract (not-equal {lhs} {rhs}))
//                 "#
//                 );
//                 let neq_result = run_prog!(egraph, &check_neq_command)?;

//                 if neq_result.is_empty() {
//                     // They are equal, skip.
//                     continue;
//                 }

//                 // They are not equal, create a rewrite.
//                 let rewrite = Rewrite::new(None, lhs.clone(), rhs.clone());
//                 let key = format!("{} -> {}", format!("{lhs}"), format!("{rhs}"));
//                 by_cvec.entry(cvec.clone()).or_default().push(lhs.clone());
//             }
//         }
//     }

//     println!("Found {} unique CVecs.", by_cvec.len());
//     println!("{:?}", by_cvec);

//     Ok(Default::default())
// }

// #[cfg(test)]
// mod cvec_match_tests {
//     use crate::{
//         bubbler::{Bubbler, BubblerConfig},
//         language::BubbleLang,
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
//             .add_term(&BubbleLang::Add(
//                 Box::new(BubbleLang::Var("x".into())),
//                 Box::new(BubbleLang::Int(1)),
//             ))
//             .unwrap();

//         bubbler
//             .add_term(&BubbleLang::Add(
//                 Box::new(BubbleLang::Int(1)),
//                 Box::new(BubbleLang::Var("x".into())),
//             ))
//             .unwrap();

//         let rws: RewriteSet<BubbleLang> = cvec_match(&mut bubbler.egraph, &bubbler.cache).unwrap();
//         assert!(!rws.is_empty());
//     }
// }
