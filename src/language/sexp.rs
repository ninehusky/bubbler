use std::collections::HashMap;
use std::str::FromStr;

use egglog::ast::Expr;
use egglog::prelude::{RustSpan, Span};
use egglog::{call, lit, var};

/// S-expressions.
/// Lovingly stolen from Enumo's implementation: https://github.com/uwplse/ruler/blob/main/src/enumo/sexp.rs
/// Mwahahaha! Take that, UW!
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Sexp {
    Atom(String),
    List(Vec<Self>),
}

impl From<Sexp> for Expr {
    fn from(val: Sexp) -> Self {
        match val {
            Sexp::Atom(s) => {
                if let Ok(ival) = s.parse::<i64>() {
                    lit!(ival)
                } else {
                    var!(s)
                }
            }
            Sexp::List(l) => {
                let exprs: Vec<Expr> = l[1..].iter().map(|s| s.clone().into()).collect();
                let Sexp::Atom(s) = &l[0] else {
                    panic!(
                        "Expected atom at head of S-expression list, but found {:?}",
                        &l[0]
                    );
                };
                call!(s, exprs)
            }
        }
    }
}

impl FromStr for Sexp {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use symbolic_expressions::parser::parse_str;
        let sexp = parse_str(s).unwrap();
        Ok(Self::from_symbolic_expr(sexp))
    }
}

impl std::fmt::Display for Sexp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Sexp::Atom(x) => write!(f, "{x}"),
            Sexp::List(l) => {
                write!(f, "(").expect("not written");
                for x in l {
                    write!(f, "{x} ").expect("not written");
                }
                write!(f, ")").expect("not written");
                Ok(())
            }
        }
    }
}

#[allow(dead_code)]
impl Sexp {
    fn from_symbolic_expr(sexp: symbolic_expressions::Sexp) -> Self {
        match sexp {
            symbolic_expressions::Sexp::String(s) => Self::Atom(s),
            symbolic_expressions::Sexp::List(ss) => Self::List(
                ss.iter()
                    .map(|s| Sexp::from_symbolic_expr(s.clone()))
                    .collect(),
            ),
            symbolic_expressions::Sexp::Empty => Self::List(vec![]),
        }
    }

    fn mk_canon(
        &self,
        symbols: &[String],
        mut idx: usize,
        mut subst: HashMap<String, String>,
    ) -> (HashMap<String, String>, usize) {
        match self {
            Sexp::Atom(x) => {
                if symbols.contains(x) && !subst.contains_key(x) {
                    subst.insert(x.into(), symbols[idx].clone());
                    idx += 1;
                }
                (subst, idx)
            }
            Sexp::List(exps) => exps.iter().fold((subst, idx), |(acc, idx), item| {
                item.mk_canon(symbols, idx, acc)
            }),
        }
    }

    fn apply_subst(&self, subst: &HashMap<String, String>) -> Self {
        match self {
            Sexp::Atom(s) => {
                if let Some(v) = subst.get(s) {
                    Sexp::Atom(v.into())
                } else {
                    Sexp::Atom(s.into())
                }
            }
            Sexp::List(exps) => Sexp::List(exps.iter().map(|s| s.apply_subst(subst)).collect()),
        }
    }

    pub(crate) fn canon(&self, symbols: &[String]) -> Self {
        let (subst, _) = self.mk_canon(symbols, 0, Default::default());
        self.apply_subst(&subst)
    }

    pub(crate) fn size(&self) -> usize {
        match self {
            Sexp::Atom(_) => 1,
            Sexp::List(s) => s.iter().map(|x| x.size()).sum::<usize>(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn from_str() {
        assert_eq!("a".parse::<Sexp>().unwrap(), Sexp::Atom("a".into()));
        assert_eq!(
            "(+ (- 1 2) 0)".parse::<Sexp>().unwrap(),
            Sexp::List(vec![
                Sexp::Atom("+".into()),
                Sexp::List(vec![
                    Sexp::Atom("-".into()),
                    Sexp::Atom("1".into()),
                    Sexp::Atom("2".into()),
                ]),
                Sexp::Atom("0".into()),
            ])
        )
    }

    #[test]
    fn measure_atoms() {
        let exprs = vec![
            ("a", 1),
            ("(a b)", 2),
            ("(a b c)", 3),
            ("(a (b c))", 3),
            ("(a b (c d))", 4),
            ("(a (b (c d)))", 4),
            ("(a (b c) (d e))", 5),
        ];
        for (expr, size) in exprs {
            assert_eq!(expr.parse::<Sexp>().unwrap().size(), size);
        }
    }

    #[test]
    fn test_egglog_lowering() {
        let sexp: Sexp = "?x".parse().unwrap();
        let expr: Expr = sexp.clone().into();
        // match on the not-span part of the expr.
        if let Expr::Var(_, name) = expr {
            assert_eq!(name, "?x");
        } else {
            panic!("Expected Expr::Var");
        }
    }
}
