use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    hash::Hash,
    str::FromStr,
};

pub mod implication;
pub mod rule;
pub(crate) mod sexp;

use sexp::Sexp;

/// A characteristic vector.
pub type CVec<L> = Vec<Option<<L as Language>::Constant>>;

/// A predicate vector.
pub type PVec = Vec<bool>;

pub type Constant<L> = <L as Language>::Constant;

/// An environment mapping variable names to a set of constants.
pub type Environment<L> = HashMap<String, Vec<Constant<L>>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term<L: Language> {
    Hole(String),
    Var(String),
    Const(Constant<L>),
    Node(L::Op, Vec<Term<L>>),
}

pub trait OpTrait {
    fn arity(&self) -> usize;

    fn name(&self) -> &'static str;
}

pub trait Language: Clone + Debug + PartialEq + Eq {
    type Constant: Clone + Debug + PartialEq + Eq + Hash + Display + FromStr;
    type Op: Clone + Debug + Display + PartialEq + Eq + Hash + OpTrait + FromStr;

    fn name() -> &'static str;

    fn interesting_constants() -> Vec<Self::Constant>;

    fn make_environment(vars: &[String]) -> Environment<Self> {
        let vals: Vec<Self::Constant> = Self::interesting_constants().into_iter().collect();

        let mut env: Environment<Self> = HashMap::new();
        let cross_product = self_product(&vals, vars.len());

        for (i, var) in vars.iter().enumerate() {
            // Debug print
            println!("Assigning {} to {:?}", var, cross_product[i]);
            env.insert(var.clone(), cross_product[i].clone());
        }

        env
    }

    /// List all operators in this language.
    fn ops() -> Vec<Self::Op>;

    /// Default Egglog source generation
    fn to_egglog_src() -> String {
        let mut s = format!("(datatype {}\n", Self::name());
        // Add Var and Const constructors
        s.push_str(format!("    (Const {})\n", std::any::type_name::<Self::Constant>()).as_str());
        s.push_str("    (Var String)\n");
        for op in Self::ops() {
            assert!(
                op.to_string() != "Var" && op.to_string() != "Const",
                "Operators cannot be named 'Var' or 'Const'."
            );
            let mut op_str = format!("    ({}", op.name());
            for _ in 0..op.arity() {
                op_str.push(' ');
                op_str.push_str(Self::name());
            }
            op_str.push(')');
            s.push_str(&op_str);
            s.push('\n');
        }
        s.push(')');
        s
    }

    fn evaluate_op(op: &Self::Op, child_vecs: &[CVec<Self>]) -> CVec<Self>;
}

impl<L: Language> Term<L> {
    pub fn from_sexp(sexp: &Sexp) -> Result<Term<L>, String> {
        match sexp {
            Sexp::Atom(s) => {
                // remove all quotes.
                let s = s.trim_matches('"').to_string();
                if s.starts_with('?') {
                    Ok(Term::Hole(s.clone()))
                } else if s.chars().all(|c| c.is_alphabetic()) {
                    Ok(Term::Var(s.clone()))
                } else {
                    // Try to parse as constant
                    let const_val = s
                        .parse::<L::Constant>()
                        .map_err(|_| format!("Failed to parse constant: {}", s))?;
                    Ok(Term::Const(const_val))
                }
            }
            Sexp::List(list) => {
                if list.is_empty() {
                    return Err("Empty S-expression list.".to_string());
                }
                let op_sexp = &list[0];
                let mut children = vec![];
                for child_sexp in &list[1..] {
                    let child_term = Self::from_sexp(child_sexp)?;
                    children.push(child_term);
                }

                if let Sexp::Atom(op_name) = op_sexp {
                    match op_name.as_str() {
                        "Var" => {
                            if children.len() != 1 {
                                return Err("Var operator must have exactly one child.".to_string());
                            }
                            if let Term::Var(var_name) = &children[0] {
                                Ok(Term::Var(var_name.clone()))
                            } else {
                                Err("Var operator child must be a variable name.".to_string())
                            }
                        }
                        "Const" => {
                            if children.len() != 1 {
                                return Err(
                                    "Const operator must have exactly one child.".to_string()
                                );
                            }
                            if let Term::Const(c) = &children[0] {
                                Ok(Term::Const(c.clone()))
                            } else {
                                Err("Const operator child must be a constant value.".to_string())
                            }
                        }
                        _ => {
                            let op = L::Op::from_str(op_name)
                                .map_err(|_| format!("Failed to parse operator {}", op_name))?;
                            Term::make_node(op, children)
                        }
                    }
                } else {
                    Err("Operator must be an atom.".to_string())
                }
            }
        }
    }

    pub fn to_sexp(&self) -> Sexp {
        match self {
            Term::Hole(name) => Sexp::Atom(name.clone()),
            Term::Var(name) => Sexp::List(vec![
                Sexp::Atom("Var".to_string()),
                Sexp::Atom(name.clone()),
            ]),
            Term::Const(c) => Sexp::List(vec![
                Sexp::Atom("Const".to_string()),
                Sexp::Atom(format!("{}", c)),
            ]),
            Term::Node(op, children) => {
                let mut list = vec![Sexp::Atom(op.name().to_string())];
                for child in children {
                    list.push(child.to_sexp());
                }
                Sexp::List(list)
            }
        }
    }

    pub fn make_node(op: L::Op, children: Vec<Term<L>>) -> Result<Self, String> {
        if children.len() != op.arity() {
            return Err(format!(
                "Operator {} expects {} children, got {}",
                op.name(),
                op.arity(),
                children.len()
            ));
        }
        Ok(Term::Node(op, children))
    }

    pub fn children(&self) -> &[Term<L>] {
        match self {
            Term::Node(_, children) => children,
            _ => &[],
        }
    }

    pub fn is_concrete(&self) -> bool {
        match self {
            Term::Hole(_) => false,
            Term::Var(_) => true,
            Term::Const(_) => true,
            Term::Node(_, children) => children.iter().all(|c| c.is_concrete()),
        }
    }

    pub fn generalize(&self, cache: &mut HashMap<String, String>) -> Result<Self, String> {
        let letters = "abcdefghijklmnopqrstuvwxyz";
        match self {
            Term::Hole(name) => Err(format!("Found metavariable {} in generalization.", name)),
            Term::Var(name) => {
                if let Some(gen_name) = cache.get(name) {
                    Ok(Term::Var(gen_name.clone()))
                } else {
                    let gen_name = format!(
                        "?{}",
                        letters
                            .chars()
                            .nth(cache.len())
                            .ok_or("Too many variables to generalize.")?
                    );
                    cache.insert(name.clone(), gen_name.clone());
                    Ok(Term::Var(gen_name))
                }
            }
            Term::Const(c) => Ok(Term::Const(c.clone())),
            Term::Node(op, children) => {
                let gen_children: Result<Vec<Term<L>>, String> =
                    children.iter().map(|c| c.generalize(cache)).collect();
                Ok(Term::Node(op.clone(), gen_children?))
            }
        }
    }

    pub fn concretize(&self) -> Result<Self, String> {
        match self {
            Term::Hole(name) => {
                assert!(name.starts_with('?'), "A hole must start with '?'.");
                let con_name = name.trim_start_matches('?').to_string();
                Ok(Term::Var(con_name))
            }
            Term::Var(name) => Err(format!(
                "Found concrete variable {} in concretization.",
                name
            )),
            Term::Const(c) => Ok(Term::Const(c.clone())),
            Term::Node(op, children) => {
                let conc_children: Result<Vec<Term<L>>, String> =
                    children.iter().map(|c| c.concretize()).collect();
                Ok(Term::Node(op.clone(), conc_children?))
            }
        }
    }

    /// Evaluate a term as a CVec, using a language-specific vectorized evaluator
    pub fn evaluate(&self, env: &Environment<L>) -> CVec<L> {
        match self {
            Term::Const(c) => {
                // Broadcast constant across the CVec length
                vec![Some(c.clone()); env.values().next().map_or(1, |v| v.len())]
            }
            Term::Var(v) => env.get(v).cloned().unwrap().into_iter().map(Some).collect(),
            Term::Node(op, children) => {
                let child_vecs: Vec<_> = children.iter().map(|c| c.evaluate(env)).collect();
                L::evaluate_op(op, &child_vecs)
            }
            Term::Hole(_) => {
                panic!("Cannot evaluate a term with holes. You should `concretize` it first.")
            }
        }
    }
}

impl<L: Language> Display for Term<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sexp = self.to_sexp();
        write!(f, "{}", sexp)
    }
}

/// A simple language for demonstration purposes.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BubbleLangOp {
    Neg,
    Add,
}

impl Display for BubbleLangOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BubbleLangOp::Neg => write!(f, "Neg"),
            BubbleLangOp::Add => write!(f, "Add"),
        }
    }
}

impl FromStr for BubbleLangOp {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Neg" => Ok(BubbleLangOp::Neg),
            "Add" => Ok(BubbleLangOp::Add),
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
        vec![BubbleLangOp::Neg, BubbleLangOp::Add]
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
        }
    }
}

impl OpTrait for BubbleLangOp {
    fn arity(&self) -> usize {
        match self {
            BubbleLangOp::Neg => 1,
            BubbleLangOp::Add => 2,
        }
    }

    fn name(&self) -> &'static str {
        match self {
            BubbleLangOp::Neg => "Neg",
            BubbleLangOp::Add => "Add",
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

    #[test]
    fn test_arity() {
        use BubbleLangOp::*;

        // Correct arity
        let ok = Term::<BubbleLang>::make_node(Add, vec![Term::Const(1), Term::Const(2)]);
        assert!(ok.is_ok());

        // Wrong arity
        let fail = Term::<BubbleLang>::make_node(Add, vec![Term::Const(1)]);
        assert!(fail.is_err());
    }
}
