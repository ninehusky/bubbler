/// The types that can be used as your constants for bubbler.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum BubbleConstant {
    Int(i64),
    String(String),
    Bool(bool),
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
