use std::{
    fmt::{Debug, Display},
    hash::Hash,
    str::FromStr,
};

mod sexp;

use sexp::Sexp;

/// A characteristic vector.
pub type CVec<L> = Vec<Option<<L as Language>::Constant>>;

/// A predicate vector.
pub type PVec = Vec<bool>;

pub type Constant<L> = <L as Language>::Constant;

pub trait Language: Sized + Clone {
    type Var: Clone + Into<String>;
    type Constant: Clone + Hash + Eq + Debug + Display + Ord;

    fn name() -> &'static str;
    fn op(&self) -> &str;

    fn from_sexp(sexp: &Sexp) -> Result<Self, &'static str>;
    fn to_sexp(&self) -> Sexp;

    fn parse(s: &str) -> Result<Self, &'static str> {
        let sexp = s
            .parse::<Sexp>()
            .map_err(|_| "Failed to parse S-expression.")?;
        Self::from_sexp(&sexp)
    }

    fn schema() -> Vec<(&'static str, Vec<&'static str>)>;

    fn to_egglog_src() -> String {
        let mut s = format!("(datatype {}\n", Self::name());
        for (variant, fields) in Self::schema() {
            let fields_str = fields.join(" ");
            s.push_str(&format!("  ({variant} {fields_str})\n"));
        }
        s.push(')');
        s
    }
}

/// A simple Bubbler language.
#[derive(Clone, Debug)]
pub enum BubbleLang {
    Int(i64),
    Var(String),
    Add(Box<BubbleLang>, Box<BubbleLang>),
    Lt(Box<BubbleLang>, Box<BubbleLang>),
}

#[derive(Clone, Debug)]
pub enum BubbleType {
    Int,
    Bool,
}

impl FromStr for BubbleType {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "IntTy" => Ok(BubbleType::Int),
            "BoolTy" => Ok(BubbleType::Bool),
            _ => Err("Unknown type"),
        }
    }
}

impl From<&BubbleType> for String {
    fn from(t: &BubbleType) -> Self {
        match t {
            BubbleType::Int => "IntTy".into(),
            BubbleType::Bool => "BoolTy".into(),
        }
    }
}

impl Language for BubbleLang {
    type Var = String;
    type Constant = i64;

    fn op(&self) -> &str {
        match self {
            BubbleLang::Int(_) => "Int",
            BubbleLang::Var(_) => "Var",
            BubbleLang::Add(_, _) => "Add",
            BubbleLang::Lt(_, _) => "Lt",
        }
    }

    fn name() -> &'static str {
        "BubbleLang"
    }

    fn schema() -> Vec<(&'static str, Vec<&'static str>)> {
        vec![
            ("Int", vec!["i64"]),
            ("Var", vec!["String"]),
            ("Add", vec!["BubbleLang", "BubbleLang"]),
            ("Lt", vec!["BubbleLang", "BubbleLang"]),
        ]
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, &'static str> {
        match sexp {
            Sexp::Atom(_) => Err("Unexpected atom."),
            Sexp::List(l) => {
                if l.is_empty() {
                    return Err("Empty list.");
                }
                let op = if let Sexp::Atom(op) = &l[0] {
                    op.as_str()
                } else {
                    return Err("Expected an atom as the first element.");
                };
                let arg_len = l.len() - 1;
                match (op, arg_len) {
                    ("Var", 1) => {
                        if let Sexp::Atom(v) = &l[1] {
                            Ok(BubbleLang::Var(v.clone()))
                        } else {
                            Err("Var expects an atom.")
                        }
                    }
                    ("Int", 1) => {
                        if let Sexp::Atom(v) = &l[1] {
                            if let Ok(num) = v.parse::<i64>() {
                                Ok(BubbleLang::Int(num))
                            } else {
                                Err("Int expects a valid integer.")
                            }
                        } else {
                            Err("Int expects an atom.")
                        }
                    }
                    ("Add", 2) => {
                        let left = BubbleLang::from_sexp(&l[1])?;
                        let right = BubbleLang::from_sexp(&l[2])?;
                        Ok(BubbleLang::Add(Box::new(left), Box::new(right)))
                    }
                    ("Lt", 2) => {
                        let left = BubbleLang::from_sexp(&l[1])?;
                        let right = BubbleLang::from_sexp(&l[2])?;
                        Ok(BubbleLang::Lt(Box::new(left), Box::new(right)))
                    }
                    _ => Err("Unknown operation."),
                }
            }
        }
    }

    fn to_sexp(&self) -> Sexp {
        match self {
            BubbleLang::Int(n) => {
                Sexp::List(vec![Sexp::Atom("Int".into()), Sexp::Atom(n.to_string())])
            }
            BubbleLang::Var(v) => Sexp::List(vec![Sexp::Atom("Var".into()), Sexp::Atom(v.clone())]),
            BubbleLang::Add(left, right) => Sexp::List(vec![
                Sexp::Atom("Add".into()),
                left.to_sexp(),
                right.to_sexp(),
            ]),
            BubbleLang::Lt(left, right) => Sexp::List(vec![
                Sexp::Atom("Lt".into()),
                left.to_sexp(),
                right.to_sexp(),
            ]),
        }
    }
}

pub fn say_hi() {
    println!("I am a Bubbler! 🐳");
}
