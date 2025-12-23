use std::{collections::HashMap, fmt::Display, sync::Arc};

use super::Language;
use crate::language::Term;
use egglog::util::IndexMap;

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
    pub fn new(cond: Option<Term<L>>, lhs: Term<L>, rhs: Term<L>) -> Result<Self, String> {
        let mut map = HashMap::new();
        let cond = cond.map(|c| {
            c.generalize(&mut map)
                .expect("Failed to generalize condition.")
        });

        let lhs = lhs.generalize(&mut map).expect("Failed to generalize LHS.");
        let rhs = rhs.generalize(&mut map).expect("Failed to generalize RHS.");

        if lhs.vars().iter().any(|v| !rhs.vars().contains(v)) {
            return Err("LHS of a rewrite cannot contain variables not in the RHS.".to_string());
        }

        // If the lhs is just a Hole, reject it. See #21 for why
        // Bubbler doesn't have the machinery to handle rules like this.
        if matches!(lhs, Term::Hole(_)) {
            return Err("LHS of a rewrite cannot be just a Hole.".to_string());
        }

        Ok(match cond {
            Some(c) => Self::Conditional { cond: c, lhs, rhs },
            None => Self::Unconditional { lhs, rhs },
        })
    }

    pub fn lhs_concrete(&self) -> Term<L> {
        match self {
            Rewrite::Conditional { lhs, .. } => lhs.concretize().unwrap(),
            Rewrite::Unconditional { lhs, .. } => lhs.concretize().unwrap(),
        }
    }

    pub fn rhs_concrete(&self) -> Term<L> {
        match self {
            Rewrite::Conditional { rhs, .. } => rhs.concretize().unwrap(),
            Rewrite::Unconditional { rhs, .. } => rhs.concretize().unwrap(),
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
