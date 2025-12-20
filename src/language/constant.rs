/// The types that can be used as your constants for bubbler.
/// These map directly to the types of literals supported in Egglog.
/// "Constant" is kind of a misnomer here, because one can imagine
/// e.g. a bitvector DSL which uses a `(Bitvector i64 i64)` value
/// as its base constant type. But whatever. You'll get over it.
// TODO: see how bad it would be to do stuff like having a
// union type here.
use egglog::ast::Literal;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum BubbleConstant {
    Int(i64),
    String(String),
    Bool(bool),
}

impl From<Literal> for BubbleConstant {
    fn from(lit: Literal) -> Self {
        match lit {
            egglog::ast::Literal::Int(i) => BubbleConstant::Int(i),
            egglog::ast::Literal::String(s) => BubbleConstant::String(s),
            egglog::ast::Literal::Bool(b) => BubbleConstant::Bool(b),
            _ => panic!("Unsupported literal type for BubbleConstant"),
        }
    }
}

impl From<BubbleConstant> for egglog::ast::Literal {
    fn from(c: BubbleConstant) -> Self {
        match c {
            BubbleConstant::Int(i) => egglog::ast::Literal::Int(i),
            BubbleConstant::String(s) => egglog::ast::Literal::String(s),
            BubbleConstant::Bool(b) => egglog::ast::Literal::Bool(b),
        }
    }
}

impl From<i64> for BubbleConstant {
    fn from(s: i64) -> Self {
        BubbleConstant::Int(s)
    }
}

impl From<String> for BubbleConstant {
    fn from(s: String) -> Self {
        BubbleConstant::String(s)
    }
}

impl From<bool> for BubbleConstant {
    fn from(s: bool) -> Self {
        BubbleConstant::Bool(s)
    }
}
