//! A language inspired by LLVM IR instructions.
//! Some nuances (`icmp slt` signed vs unsigned comparisons) are not yet modeled.
//! So in essence, this "LLVM" language is just a wrapper for integer arithmetic for now.

use std::{fmt::Display, str::FromStr};

use crate::language::{constant::BubbleConstant, Language, OpTrait};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LLVMLang;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum LLVMLangOp {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Gt,
    Min,
    Max,
    Neq,
}

impl OpTrait for LLVMLangOp {
    fn arity(&self) -> usize {
        match self {
            LLVMLangOp::Add => 2,
            LLVMLangOp::Sub => 2,
            LLVMLangOp::Mul => 2,
            LLVMLangOp::Div => 2,
            LLVMLangOp::Lt => 2,
            LLVMLangOp::Gt => 2,
            LLVMLangOp::Min => 2,
            LLVMLangOp::Max => 2,
            LLVMLangOp::Neq => 2,
        }
    }

    fn name(&self) -> &'static str {
        match self {
            LLVMLangOp::Add => "Add",
            LLVMLangOp::Sub => "Sub",
            LLVMLangOp::Mul => "Mul",
            LLVMLangOp::Div => "Div",
            LLVMLangOp::Lt => "Lt",
            LLVMLangOp::Gt => "Gt",
            LLVMLangOp::Min => "Min",
            LLVMLangOp::Max => "Max",
            LLVMLangOp::Neq => "Neq",
        }
    }
}

impl Display for LLVMLangOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl FromStr for LLVMLangOp {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Add" => Ok(LLVMLangOp::Add),
            "Sub" => Ok(LLVMLangOp::Sub),
            "Mul" => Ok(LLVMLangOp::Mul),
            "Div" => Ok(LLVMLangOp::Div),
            "Lt" => Ok(LLVMLangOp::Lt),
            "Gt" => Ok(LLVMLangOp::Gt),
            "Min" => Ok(LLVMLangOp::Min),
            "Max" => Ok(LLVMLangOp::Max),
            "Neq" => Ok(LLVMLangOp::Neq),
            _ => Err(format!("Unknown LLVMLangOp: {}", s)),
        }
    }
}

impl Language for LLVMLang {
    type Constant = i64;
    type Op = LLVMLangOp;

    fn name() -> &'static str {
        "LLVMLang"
    }

    fn constant_from_bubble(b: BubbleConstant) -> Self::Constant {
        match b {
            BubbleConstant::Int(i) => i,
            _ => panic!("Expected integer constant for LLVMLang"),
        }
    }

    fn constant_to_bubble(c: &Self::Constant) -> BubbleConstant {
        BubbleConstant::Int(*c)
    }

    fn interesting_constants() -> Vec<Self::Constant> {
        vec![-10, -1, 0, 1, 2, 5, 100]
    }

    fn ops() -> Vec<Self::Op> {
        vec![
            LLVMLangOp::Add,
            LLVMLangOp::Sub,
            LLVMLangOp::Mul,
            LLVMLangOp::Div,
            LLVMLangOp::Lt,
            LLVMLangOp::Gt,
            LLVMLangOp::Min,
            LLVMLangOp::Max,
            LLVMLangOp::Neq,
        ]
    }

    fn evaluate_op(
        op: &Self::Op,
        child_vecs: &[crate::language::CVec<Self>],
    ) -> crate::language::CVec<Self> {
        match op {
            LLVMLangOp::Add => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some(lv + rv),
                        _ => None,
                    })
                    .collect()
            }
            LLVMLangOp::Sub => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some(lv - rv),
                        _ => None,
                    })
                    .collect()
            }
            LLVMLangOp::Mul => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some(lv * rv),
                        _ => None,
                    })
                    .collect()
            }
            LLVMLangOp::Div => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(_), Some(0)) => None, // Division by zero
                        (Some(lv), Some(rv)) => Some(lv / rv),
                        _ => None,
                    })
                    .collect()
            }
            LLVMLangOp::Lt => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some((lv < rv) as i64),
                        _ => None,
                    })
                    .collect()
            }
            LLVMLangOp::Gt => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some((lv > rv) as i64),
                        _ => None,
                    })
                    .collect()
            }
            LLVMLangOp::Min => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some(std::cmp::min(*lv, *rv)),
                        _ => None,
                    })
                    .collect()
            }
            LLVMLangOp::Max => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some(std::cmp::max(*lv, *rv)),
                        _ => None,
                    })
                    .collect()
            }
            LLVMLangOp::Neq => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some((lv != rv) as i64),
                        _ => None,
                    })
                    .collect()
            }
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum LLVMMetaOp {
    IsPositive,
    IsNegative,
    IsNotZero,
}

impl OpTrait for LLVMMetaOp {
    fn arity(&self) -> usize {
        match self {
            LLVMMetaOp::IsPositive => 1,
            LLVMMetaOp::IsNegative => 1,
            LLVMMetaOp::IsNotZero => 1,
        }
    }

    fn name(&self) -> &'static str {
        match self {
            LLVMMetaOp::IsPositive => "IsPositive",
            LLVMMetaOp::IsNegative => "IsNegative",
            LLVMMetaOp::IsNotZero => "IsNotZero",
        }
    }
}

impl FromStr for LLVMMetaOp {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "IsPositive" => Ok(LLVMMetaOp::IsPositive),
            "IsNegative" => Ok(LLVMMetaOp::IsNegative),
            "IsNotZero" => Ok(LLVMMetaOp::IsNotZero),
            _ => Err(format!("Unknown LLVMMetaOp: {}", s)),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum LLVMMetaConst {
    Int(i64),
    Bool(bool),
}

impl From<LLVMMetaConst> for BubbleConstant {
    fn from(c: LLVMMetaConst) -> Self {
        match c {
            LLVMMetaConst::Int(i) => BubbleConstant::Int(i),
            LLVMMetaConst::Bool(b) => BubbleConstant::Bool(b),
        }
    }
}

impl FromStr for LLVMMetaConst {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "true" => Ok(LLVMMetaConst::Bool(true)),
            "false" => Ok(LLVMMetaConst::Bool(false)),
            _ => {
                if let Ok(i) = s.parse::<i64>() {
                    Ok(LLVMMetaConst::Int(i))
                } else {
                    Err(format!("Unknown LLVMMetaConst: {}", s))
                }
            }
        }
    }
}
