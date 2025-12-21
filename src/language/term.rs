use std::{fmt::Display, hash::Hash, str::FromStr};

use super::{CVec, Constant, Environment, Language, OpTrait, sexp::Sexp};

use std::collections::HashMap;

/// The AST nodes of a Predicate Language.
// NOTE(@ninehusky): I'm still unsure how this should get structured.
// You might imagine later on something like a `Condition<L>`,
// and then `PredicateTerm<L>` is the AST node for that condition language.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PredicateTerm<L: Language> {
    pub term: Term<L>,
}

impl<L: Language> PredicateTerm<L> {
    pub fn from_term(term: Term<L>) -> Self {
        Self { term }
    }
}

impl<L: Language> Hash for PredicateTerm<L>
where
    L::Op: Hash,
    L::Constant: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.term.hash(state);
    }
}

/// The AST nodes of a [`Language`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term<L: Language> {
    Hole(String),
    Var(String),
    Const(Constant<L>),
    Call(L::Op, Vec<Term<L>>),
}

impl<L: Language> Hash for Term<L>
where
    L::Op: Hash,
    L::Constant: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Term::Hole(name) => {
                state.write_u8(0);
                name.hash(state);
            }
            Term::Var(name) => {
                state.write_u8(1);
                name.hash(state);
            }
            Term::Const(c) => {
                state.write_u8(2);
                c.hash(state);
            }
            Term::Call(op, children) => {
                state.write_u8(3);
                op.hash(state);
                for child in children {
                    child.hash(state);
                }
            }
        }
    }
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
                            Term::make_call(op, children)
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
                Sexp::Atom(format!("{:?}", c)),
            ]),
            Term::Call(op, children) => {
                let mut list = vec![Sexp::Atom(op.name().to_string())];
                for child in children {
                    list.push(child.to_sexp());
                }
                Sexp::List(list)
            }
        }
    }

    pub fn make_call(op: L::Op, children: Vec<Term<L>>) -> Result<Self, String> {
        if children.len() != op.arity() {
            return Err(format!(
                "Operator {} expects {} children, got {}",
                op.name(),
                op.arity(),
                children.len()
            ));
        }
        Ok(Term::Call(op, children))
    }

    pub fn children(&self) -> &[Term<L>] {
        match self {
            Term::Call(_, children) => children,
            _ => &[],
        }
    }

    pub fn is_concrete(&self) -> bool {
        match self {
            Term::Hole(_) => false,
            Term::Var(_) => true,
            Term::Const(_) => true,
            Term::Call(_, children) => children.iter().all(|c| c.is_concrete()),
        }
    }

    /// Generalize variables in this term into metavariables ?a, ?b, etc.
    /// That is, "x + y" becomes "?a + ?b".
    /// Errors if the term already contains a Hole.
    /// Panics if there are more than 26 variables to generalize.
    /// But if you get that far, there are bigger things to worry about.
    ///
    /// ```
    /// use bubbler::language::{Language, term::Term, BubbleLang};
    /// use bubbler::language::BubbleLangOp;
    /// use std::collections::HashMap;
    ///
    /// let x_plus_1 = Term::<BubbleLang>::make_call(
    ///   BubbleLangOp::Add,
    ///   vec![
    ///       Term::Var("x".to_string()),
    ///       Term::Var("y".to_string()),
    ///   ]);
    /// let mut cache = HashMap::new();
    /// let gen_term = x_plus_1.unwrap().generalize(&mut cache).unwrap();
    /// assert_eq!(gen_term.to_string().replace(" ", ""), "(Add ?a ?b)".replace(" ", ""));
    ///
    /// ```
    pub fn generalize(&self, cache: &mut HashMap<String, String>) -> Result<Self, String> {
        let letters = "abcdefghijklmnopqrstuvwxyz";
        match self {
            Term::Hole(name) => Err(format!(
                "Found metavariable {} in generalization of {}.",
                name, self
            )),
            Term::Var(name) => {
                if let Some(gen_name) = cache.get(name) {
                    Ok(Term::Hole(gen_name.clone()))
                } else {
                    let gen_name = format!(
                        "?{}",
                        letters
                            .chars()
                            .nth(cache.len())
                            .ok_or("Too many variables to generalize.")?
                    );
                    cache.insert(name.clone(), gen_name.clone());
                    Ok(Term::Hole(gen_name))
                }
            }
            Term::Const(c) => Ok(Term::Const(c.clone())),
            Term::Call(op, children) => {
                println!("op: {}", op);
                println!("children: {:?}", children);
                let gen_children: Result<Vec<Term<L>>, String> =
                    children.iter().map(|c| c.generalize(cache)).collect();
                Ok(Term::Call(op.clone(), gen_children?))
            }
        }
    }

    /// Given a term with holes, concretize it by replacing holes with variables.
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
            Term::Call(op, children) => {
                let conc_children: Result<Vec<Term<L>>, String> =
                    children.iter().map(|c| c.concretize()).collect();
                Ok(Term::Call(op.clone(), conc_children?))
            }
        }
    }

    /// Evaluate a term as a CVec, using a language-specific vectorized evaluator
    // TODO(@ninehusky): Egg systems get cvec-memoization for free; you get
    // speedup because you're not evaluating _every_ single term you add to the
    // e-graph. Bubbler doesn't support this for now, but it shouldn't impact
    // performance in any meaningful way. Evaluation of terms is not the bottleneck
    // for these kinds of systems.
    pub fn evaluate(&self, env: &Environment<L>) -> CVec<L> {
        match self {
            Term::Const(c) => {
                vec![Some(c.clone()); env.values().next().map_or(1, |v| v.len())]
            }
            Term::Var(v) => env.get(v).cloned().unwrap().into_iter().map(Some).collect(),
            Term::Call(op, children) => {
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
