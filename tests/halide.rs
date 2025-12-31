use std::{fmt::Display, str::FromStr};

use bubbler::language::{BubbleConstant, Language, OpTrait};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HalideLang;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum HalideOp {
    // Boolean ops
    Not,
    And,
    Or,

    // Comparators
    Neq,
    Lt,
    Gt,
    Ge,

    // Arithmetic ops
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Misc
    Min,
    Max,
    Select,
}

impl OpTrait for HalideOp {
    fn arity(&self) -> usize {
        match self {
            HalideOp::Not => 1,
            HalideOp::And => 2,
            HalideOp::Or => 2,
            HalideOp::Neq => 2,
            HalideOp::Lt => 2,
            HalideOp::Gt => 2,
            HalideOp::Ge => 2,
            HalideOp::Add => 2,
            HalideOp::Sub => 2,
            HalideOp::Mul => 2,
            HalideOp::Div => 2,
            HalideOp::Mod => 2,
            HalideOp::Min => 2,
            HalideOp::Max => 2,
            HalideOp::Select => 3,
        }
    }

    fn name(&self) -> &'static str {
        match self {
            HalideOp::Not => "Not",
            HalideOp::And => "And",
            HalideOp::Or => "Or",
            HalideOp::Neq => "Neq",
            HalideOp::Lt => "Lt",
            HalideOp::Gt => "Gt",
            HalideOp::Ge => "Ge",
            HalideOp::Add => "Add",
            HalideOp::Sub => "Sub",
            HalideOp::Mul => "Mul",
            HalideOp::Div => "Div",
            HalideOp::Mod => "Mod",
            HalideOp::Min => "Min",
            HalideOp::Max => "Max",
            HalideOp::Select => "Select",
        }
    }
}

impl FromStr for HalideOp {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Not" => Ok(HalideOp::Not),
            "And" => Ok(HalideOp::And),
            "Or" => Ok(HalideOp::Or),
            "Neq" => Ok(HalideOp::Neq),
            "Lt" => Ok(HalideOp::Lt),
            "Gt" => Ok(HalideOp::Gt),
            "Ge" => Ok(HalideOp::Ge),
            "Add" => Ok(HalideOp::Add),
            "Sub" => Ok(HalideOp::Sub),
            "Mul" => Ok(HalideOp::Mul),
            "Div" => Ok(HalideOp::Div),
            "Mod" => Ok(HalideOp::Mod),
            "Min" => Ok(HalideOp::Min),
            "Max" => Ok(HalideOp::Max),
            "Select" => Ok(HalideOp::Select),
            _ => Err(format!("Unknown HalideOp: {}", s)),
        }
    }
}

impl Display for HalideOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl Language for HalideLang {
    type Op = HalideOp;
    type Constant = i64;

    fn name() -> &'static str {
        "Halide"
    }

    fn interesting_constants() -> Vec<Self::Constant> {
        vec![-10, -1, 0, 1, 2, 5, 100]
    }

    fn constant_from_bubble(b: BubbleConstant) -> Self::Constant {
        match b {
            BubbleConstant::Int(i) => i,
            _ => panic!("Expected integer constant for HalideLang"),
        }
    }

    fn constant_to_bubble(c: &Self::Constant) -> BubbleConstant {
        BubbleConstant::Int(*c)
    }

    fn ops() -> Vec<Self::Op> {
        vec![
            HalideOp::Not,
            HalideOp::And,
            HalideOp::Or,
            HalideOp::Neq,
            HalideOp::Lt,
            HalideOp::Gt,
            HalideOp::Ge,
            HalideOp::Add,
            HalideOp::Sub,
            HalideOp::Mul,
            HalideOp::Div,
            HalideOp::Mod,
            HalideOp::Min,
            HalideOp::Max,
            HalideOp::Select,
        ]
    }

    fn evaluate_op(
        op: &Self::Op,
        child_vecs: &[bubbler::language::CVec<Self>],
    ) -> bubbler::language::CVec<Self> {
        match op {
            HalideOp::Not => {
                let child_vec = &child_vecs[0];
                child_vec
                    .iter()
                    .map(|v| match v {
                        Some(cv) => Some(if *cv == 0 { 1 } else { 0 }),
                        None => None,
                    })
                    .collect()
            }
            HalideOp::And => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some(if *lv != 0 && *rv != 0 { 1 } else { 0 }),
                        _ => None,
                    })
                    .collect()
            }
            HalideOp::Or => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some(if *lv != 0 || *rv != 0 { 1 } else { 0 }),
                        _ => None,
                    })
                    .collect()
            }
            HalideOp::Neq => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some(if lv != rv { 1 } else { 0 }),
                        _ => None,
                    })
                    .collect()
            }
            HalideOp::Lt => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some(if lv < rv { 1 } else { 0 }),
                        _ => None,
                    })
                    .collect()
            }
            HalideOp::Gt => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some(if lv > rv { 1 } else { 0 }),
                        _ => None,
                    })
                    .collect()
            }
            HalideOp::Ge => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some(if lv >= rv { 1 } else { 0 }),
                        _ => None,
                    })
                    .collect()
            }
            HalideOp::Add => {
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
            HalideOp::Sub => {
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
            HalideOp::Mul => {
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
            HalideOp::Div => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        // x / 0 == 0 in Halide semantics
                        (Some(_), Some(0)) => Some(0),
                        (Some(lv), Some(rv)) => lv.div_euclid(*rv).into(),
                        _ => None,
                    })
                    .collect()
            }
            HalideOp::Mod => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        // x % 0 == 0 in Halide semantics
                        (Some(_), Some(0)) => Some(0),
                        (Some(lv), Some(rv)) => lv.rem_euclid(*rv).into(),
                        _ => None,
                    })
                    .collect()
            }
            HalideOp::Min => {
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
            HalideOp::Max => {
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
            HalideOp::Select => {
                let condition_vec = &child_vecs[0];
                let true_vec = &child_vecs[1];
                let false_vec = &child_vecs[2];
                condition_vec
                    .iter()
                    .zip(true_vec.iter())
                    .zip(false_vec.iter())
                    .map(|((cond, t), f)| match (cond, t, f) {
                        (Some(cv), Some(tv), Some(fv)) => {
                            if *cv != 0 {
                                Some(*tv)
                            } else {
                                Some(*fv)
                            }
                        }
                        _ => None,
                    })
                    .collect()
            }
        }
    }
}
