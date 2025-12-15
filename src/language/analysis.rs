//! Defines an Analysis, which mirrors the egg::Analysis trait.
//! An analysis is a metalanguage for a particular domain that
//! allows you to define facts about e-classes in an e-graph.
//! These facts in Bubbler are almost always used to (1)
//! allow conditional rewrites to fire, or (2) filter out
//! unproductive rewrite candidates during rule generation.

use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

use super::CVec;
use super::{Language, OpTrait};

// There's a lot of type fuckery in this file. I have
// no idea what the hell is going on here LOL
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum MetaOp<BOp, MOp> {
    Base(BOp),
    Meta(MOp),
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

impl<BOp, MOp> Display for MetaOp<BOp, MOp> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MetaOp::Base(_) => write!(f, "BaseOp"),
            MetaOp::Meta(_) => write!(f, "MetaOp"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum MetaConstant<BaseC, MetaC> {
    Base(BaseC),
    Meta(MetaC),
}

trait MetaLanguage: Language {
    type Base: Language;
    type MetaOp: OpTrait;
    type MetaConst: Clone + Debug + PartialEq + Eq + Hash;

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
        let consts = <L::Base as Language>::interesting_constants()
            .into_iter()
            .map(MetaConstant::Base)
            .collect::<Vec<_>>();

        consts
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
                    .map(|base_c| match base_c {
                        Some(bc) => Some(MetaConstant::Base(bc.clone())),
                        None => None,
                    })
                    .collect()
            }
            MetaOp::Meta(mop) => L::evaluate_meta_op(mop, child_vecs),
        }
    }
}
