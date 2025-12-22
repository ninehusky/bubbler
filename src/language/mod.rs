//! This module defines the core language abstractions for Bubbler.
//! Generally, this module should only describe the stuff you need to
//! evaluate terms; e-graph/egglog stuff outside of `L::to_egglog_src`
//! should go elsewhere.

use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    hash::Hash,
    str::FromStr,
};

use constant::BubbleConstant;
use term::Term;

pub mod constant;
pub mod rewrite;
pub mod term;

/// A characteristic vector.
pub type CVec<L> = Vec<Option<<L as Language>::Constant>>;

/// A predicate vector.
pub type PVec = Vec<bool>;

pub type Constant<L> = <L as Language>::Constant;

/// An environment mapping variable names to a set of constants.
pub type Environment<L> = HashMap<String, Vec<Constant<L>>>;

pub trait OpTrait: Clone + Debug + PartialEq + Eq {
    fn arity(&self) -> usize;

    fn name(&self) -> &'static str;
}

pub trait Language: Clone + Debug + PartialEq + Eq {
    type Constant: Clone + Debug + PartialEq + Eq + Hash + FromStr;
    type Op: Clone + Debug + Display + PartialEq + Eq + OpTrait + Hash + FromStr;

    fn name() -> &'static str;

    fn interesting_constants() -> Vec<Self::Constant>;

    fn constant_from_bubble(b: BubbleConstant) -> Self::Constant;

    fn constant_to_bubble(c: &Self::Constant) -> BubbleConstant;

    fn make_environment(vars: &[String]) -> Environment<Self> {
        let vals: Vec<Self::Constant> = Self::interesting_constants().into_iter().collect();

        let mut env: Environment<Self> = HashMap::new();
        let cross_product = self_product(&vals, vars.len());

        for (i, var) in vars.iter().enumerate() {
            env.insert(var.clone(), cross_product[i].clone());
        }

        env
    }

    /// List all operators in this language.
    fn ops() -> Vec<Self::Op>;

    /// Default Egglog source generation
    fn to_egglog_src() -> String {
        panic!("Don't call this anymore.");
    }

    fn evaluate_op(op: &Self::Op, child_vecs: &[CVec<Self>]) -> CVec<Self>;
}

/// A simple language for demonstration purposes.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BubbleLangOp {
    Neg,
    Neq,
    Add,
    Div,
}

impl Display for BubbleLangOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BubbleLangOp::Neg => write!(f, "Neg"),
            BubbleLangOp::Neq => write!(f, "Neq"),
            BubbleLangOp::Add => write!(f, "Add"),
            BubbleLangOp::Div => write!(f, "Div"),
        }
    }
}

impl FromStr for BubbleLangOp {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Neg" => Ok(BubbleLangOp::Neg),
            "Add" => Ok(BubbleLangOp::Add),
            "Neq" => Ok(BubbleLangOp::Neq),
            "Div" => Ok(BubbleLangOp::Div),
            _ => Err(format!("Unknown BubbleLangOp: {}", s)),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BubbleLang;

impl Language for BubbleLang {
    type Constant = i64;
    type Op = BubbleLangOp;

    fn name() -> &'static str {
        "BubbleLang"
    }

    fn ops() -> Vec<BubbleLangOp> {
        vec![
            BubbleLangOp::Neg,
            BubbleLangOp::Add,
            BubbleLangOp::Neq,
            BubbleLangOp::Div,
        ]
    }

    fn constant_from_bubble(b: BubbleConstant) -> Self::Constant {
        match b {
            BubbleConstant::Int(i) => i,
            _ => panic!("Expected BubbleConstant::Int"),
        }
    }

    fn constant_to_bubble(c: &Self::Constant) -> BubbleConstant {
        BubbleConstant::Int(*c)
    }

    fn interesting_constants() -> Vec<Self::Constant> {
        vec![-10, -1, 0, 1, 2, 5, 100]
    }

    fn evaluate_op(op: &Self::Op, child_vecs: &[CVec<Self>]) -> CVec<Self> {
        match op {
            BubbleLangOp::Neg => child_vecs[0]
                .iter()
                .map(|v| v.as_ref().map(|c| -c))
                .collect(),
            BubbleLangOp::Add => child_vecs[0]
                .iter()
                .zip(child_vecs[1].iter())
                .map(|(v1, v2)| match (v1, v2) {
                    (Some(c1), Some(c2)) => Some(c1 + c2),
                    _ => None,
                })
                .collect(),
            BubbleLangOp::Neq => child_vecs[0]
                .iter()
                .zip(child_vecs[1].iter())
                .map(|(v1, v2)| match (v1, v2) {
                    (Some(c1), Some(c2)) => Some((c1 != c2) as i64),
                    _ => None,
                })
                .collect(),
            BubbleLangOp::Div => child_vecs[0]
                .iter()
                .zip(child_vecs[1].iter())
                .map(|(v1, v2)| match (v1, v2) {
                    (Some(_), Some(0)) => None, // Division by zero
                    (Some(c1), Some(c2)) => Some(c1 / c2),
                    _ => None,
                })
                .collect(),
        }
    }
}

impl OpTrait for BubbleLangOp {
    fn arity(&self) -> usize {
        match self {
            BubbleLangOp::Neg => 1,
            BubbleLangOp::Add => 2,
            BubbleLangOp::Neq => 2,
            BubbleLangOp::Div => 2,
        }
    }

    fn name(&self) -> &'static str {
        match self {
            BubbleLangOp::Neg => "Neg",
            BubbleLangOp::Add => "Add",
            BubbleLangOp::Neq => "Neq",
            BubbleLangOp::Div => "Div",
        }
    }
}

pub fn say_hi() {
    println!("Ahoy-I am a Bubbler! 🐳");
}

/// Helper function to cross product a list of values `ts` across `n` variables.
pub fn self_product<T: Clone>(ts: &[T], n: usize) -> Vec<Vec<T>> {
    let num_consts = ts.len();
    let num_rows = num_consts.pow(n as u32);
    let mut res = vec![];
    for i in 0..n {
        let mut entry = vec![];
        while entry.len() < num_rows {
            for c in ts {
                for _ in 0..num_consts.pow(i as u32) {
                    entry.push(c.clone());
                }
            }
        }
        res.push(entry);
    }
    res
}

#[cfg(test)]
mod lang_tests {
    use super::*;
    use BubbleLangOp::*;

    #[test]
    fn test_arity() {
        use BubbleLangOp::*;

        // Correct arity
        let ok = Term::<BubbleLang>::make_call(Add, vec![Term::Const(1), Term::Const(2)]);
        assert!(ok.is_ok());

        // Wrong arity
        let fail = Term::<BubbleLang>::make_call(Add, vec![Term::Const(1)]);
        assert!(fail.is_err());
    }
}
