//! Defines an Analysis, which mirrors the egg::Analysis trait.
//! An analysis is a metalanguage for a particular domain that
//! allows you to define facts about e-classes in an e-graph.
//! These facts in Bubbler are almost always used to (1)
//! allow conditional rewrites to fire, or (2) filter out
//! unproductive rewrite candidates during rule generation.

use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::str::FromStr;

use super::BubbleConstant;
use super::CVec;
use super::{Language, OpTrait};

// There's a lot of type fuckery in this file. I have
// no idea what the hell is going on here LOL
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum MetaOp<BOp: OpTrait, MOp: OpTrait> {
    Base(BOp),
    Meta(MOp),
}

impl<BOp, MOp> FromStr for MetaOp<BOp, MOp>
where
    BOp: OpTrait + FromStr,
    MOp: OpTrait + FromStr,
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(bop) = s.parse::<BOp>() {
            Ok(MetaOp::Base(bop))
        } else if let Ok(mop) = s.parse::<MOp>() {
            Ok(MetaOp::Meta(mop))
        } else {
            Err(format!("Failed to parse MetaOp from string: {}", s))
        }
    }
}

impl<BOp, MOp> OpTrait for MetaOp<BOp, MOp>
where
    BOp: OpTrait,
    MOp: OpTrait,
{
    fn arity(&self) -> usize {
        match self {
            MetaOp::Base(op) => op.arity(),
            MetaOp::Meta(op) => op.arity(),
        }
    }

    fn name(&self) -> &'static str {
        match self {
            MetaOp::Base(op) => op.name(),
            MetaOp::Meta(op) => op.name(),
        }
    }
}

impl<BOp: OpTrait, MOp: OpTrait> Display for MetaOp<BOp, MOp> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MetaOp::Base(_) => write!(f, "BaseOp"),
            MetaOp::Meta(_) => write!(f, "MetaOp"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum MetaConstant<BaseC, MetaC> {
    Base(BaseC),
    Meta(MetaC),
}

impl<BaseC, MetaC> From<MetaConstant<BaseC, MetaC>> for BubbleConstant
where
    BaseC: Into<BubbleConstant>,
    MetaC: Into<BubbleConstant>,
{
    fn from(c: MetaConstant<BaseC, MetaC>) -> Self {
        match c {
            MetaConstant::Base(b) => b.into(),
            MetaConstant::Meta(m) => m.into(),
        }
    }
}

impl<BaseC, MetaC> FromStr for MetaConstant<BaseC, MetaC>
where
    BaseC: FromStr,
    MetaC: FromStr,
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // try to parse it as a base constant first.
        if let Ok(bc) = s.parse::<BaseC>() {
            Ok(MetaConstant::Base(bc))
        } else if let Ok(mc) = s.parse::<MetaC>() {
            Ok(MetaConstant::Meta(mc))
        } else {
            Err(format!("Failed to parse MetaConstant from string: {}", s))
        }
    }
}

pub trait MetaLanguage: Language {
    type Base: Language;
    type MetaOp: OpTrait + Hash + FromStr;
    type MetaConst: Clone + Debug + PartialEq + Eq + Hash + FromStr + Into<BubbleConstant>;

    fn evaluate_meta_op(
        op: &Self::MetaOp,
        child_vecs: &[CVec<MetaLang<Self>>],
    ) -> CVec<MetaLang<Self>>
    where
        Self: Sized;

    fn meta_ops() -> Vec<Self::MetaOp>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MetaLang<L: MetaLanguage>(std::marker::PhantomData<L>);

impl<L> Language for MetaLang<L>
where
    L: MetaLanguage,
{
    type Constant = MetaConstant<<L::Base as Language>::Constant, <L as MetaLanguage>::MetaConst>;
    type Op = MetaOp<<L::Base as Language>::Op, <L as MetaLanguage>::MetaOp>;

    fn name() -> &'static str {
        "MetaLanguage"
    }

    fn ops() -> Vec<Self::Op> {
        let mut ops = <L::Base as Language>::ops()
            .into_iter()
            .map(MetaOp::Base)
            .collect::<Vec<_>>();

        ops.extend(L::meta_ops().into_iter().map(MetaOp::Meta));

        ops
    }

    fn interesting_constants() -> Vec<Self::Constant> {
        <L::Base as Language>::interesting_constants()
            .into_iter()
            .map(MetaConstant::Base)
            .collect::<Vec<_>>()
    }

    fn evaluate_op(op: &Self::Op, child_vecs: &[CVec<Self>]) -> CVec<Self> {
        match op {
            MetaOp::Base(bop) => {
                let base_child_vecs: Vec<CVec<L::Base>> = child_vecs
                    .iter()
                    .map(|cv| {
                        cv.iter()
                            .map(|mc| match mc {
                                Some(c) => match c {
                                    MetaConstant::Base(bc) => Some(bc.clone()),
                                    MetaConstant::Meta(_) => {
                                        panic!("Expected base constant, found meta constant.")
                                    }
                                },
                                None => None,
                            })
                            .collect()
                    })
                    .collect();
                L::Base::evaluate_op(bop, &base_child_vecs)
                    .iter()
                    .map(|base_c| base_c.as_ref().map(|bc| MetaConstant::Base(bc.clone())))
                    .collect()
            }
            MetaOp::Meta(mop) => L::evaluate_meta_op(mop, child_vecs),
        }
    }
}
