//! A language inspired by LLVM IR instructions.
//! Some nuances (`icmp slt` signed vs unsigned comparisons) are not yet modeled.
//! So in essence, this "LLVM" language is just a wrapper for integer arithmetic for now.

use std::{fmt::Display, str::FromStr};

use crate::language::{Language, OpTrait, constant::BubbleConstant};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LLVMLang;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum LLVMLangOp {
    And,
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Gt,
    Ge,
    Le,
    Min,
    Max,
    Neq,
}

impl OpTrait for LLVMLangOp {
    fn arity(&self) -> usize {
        match self {
            LLVMLangOp::And => 2,
            LLVMLangOp::Add => 2,
            LLVMLangOp::Sub => 2,
            LLVMLangOp::Mul => 2,
            LLVMLangOp::Div => 2,
            LLVMLangOp::Lt => 2,
            LLVMLangOp::Gt => 2,
            LLVMLangOp::Ge => 2,
            LLVMLangOp::Le => 2,
            LLVMLangOp::Min => 2,
            LLVMLangOp::Max => 2,
            LLVMLangOp::Neq => 2,
        }
    }

    fn name(&self) -> &'static str {
        match self {
            LLVMLangOp::And => "And",
            LLVMLangOp::Add => "Add",
            LLVMLangOp::Sub => "Sub",
            LLVMLangOp::Mul => "Mul",
            LLVMLangOp::Div => "Div",
            LLVMLangOp::Lt => "Lt",
            LLVMLangOp::Gt => "Gt",
            LLVMLangOp::Ge => "Ge",
            LLVMLangOp::Le => "Le",
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
            "And" => Ok(LLVMLangOp::And),
            "Add" => Ok(LLVMLangOp::Add),
            "Sub" => Ok(LLVMLangOp::Sub),
            "Mul" => Ok(LLVMLangOp::Mul),
            "Div" => Ok(LLVMLangOp::Div),
            "Lt" => Ok(LLVMLangOp::Lt),
            "Gt" => Ok(LLVMLangOp::Gt),
            "Ge" => Ok(LLVMLangOp::Ge),
            "Le" => Ok(LLVMLangOp::Le),
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
            LLVMLangOp::And,
            LLVMLangOp::Add,
            LLVMLangOp::Sub,
            LLVMLangOp::Mul,
            LLVMLangOp::Div,
            LLVMLangOp::Lt,
            LLVMLangOp::Gt,
            LLVMLangOp::Ge,
            LLVMLangOp::Le,
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
            LLVMLangOp::And => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some(((*lv != 0) && (*rv != 0)) as i64),
                        _ => None,
                    })
                    .collect()
            }
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
            LLVMLangOp::Ge => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some((lv >= rv) as i64),
                        _ => None,
                    })
                    .collect()
            }
            LLVMLangOp::Le => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some((lv <= rv) as i64),
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

#[cfg(test)]
#[allow(unused_imports)]
mod tests {
    use super::*;
    use crate::bubbler::{Bubbler, BubblerConfig, InferredFacts};
    use ruler::enumo::Workload;

    #[test]
    fn find_implications_poor_schedule() {
        // TODO(@ninehusky): we need a parser for implications/rules to make this easier.
        // let expected: Vec<Implication<LLVMLang>> = vec![
        //     Implication::new("(Gt x y)", "(Neq x y)"),
        //     Implication::new("(Lt x y)", "(Neq x y)"),
        //     Implication::new("(Ge x y)", "(Neq x y)"),
        //     Implication::new("(Le x y)", "(Neq x y)"),
        //     Implication::new("(Neq x y)", "(Gt x y)"),
        //     Implication::new("(Neq x y)", "(Lt x y)"),
        // ];

        let bubbler: Bubbler<LLVMLang> = Bubbler::new(BubblerConfig::new(
            vec!["x".into(), "y".into()],
            vec![1, 2, 3],
        ));

        let implications = bubbler.find_implications(
            &Workload::new(&["(OP2 VAR VAR)"])
                .plug("OP2", &Workload::new(&["Gt", "Lt", "Ge", "Le", "Neq"]))
                .plug("VAR", &Workload::new(&["x", "y"])),
        );

        let InferredFacts::Implications(implications) = implications else {
            panic!("Expected implications");
        };

        // We discover redundant implications like:
        // (Gt ?a ?b ) --> (Neq ?b ?a )
        // (Gt ?a ?b ) --> (Neq ?a ?b )
        // Because we didn't first discover rewrites over the condition language.
        // See `find_implications_better_schedule` test for a better schedule.

        // With no implications, this should be 8.
        // Discovered implication: (Lt ?a ?b ) --> (Le ?a ?b )
        // Discovered implication: (Lt ?a ?b ) --> (Ge ?b ?a )
        // Discovered implication: (Gt ?a ?b ) --> (Le ?b ?a )
        // Discovered implication: (Gt ?a ?b ) --> (Ge ?a ?b )
        // Discovered implication: (Lt ?a ?b ) --> (Neq ?b ?a )
        // Discovered implication: (Lt ?a ?b ) --> (Neq ?a ?b )
        // Discovered implication: (Gt ?a ?b ) --> (Neq ?a ?b )
        // Discovered implication: (Gt ?a ?b ) --> (Neq ?b ?a )
        assert_eq!(implications.len(), 8);
    }

    #[test]
    fn find_implications_better_schedule() {
        let mut bubbler: Bubbler<LLVMLang> =
            Bubbler::new(BubblerConfig::new(vec!["x".into(), "y".into()], vec![]));

        let predicate_workload = Workload::new(&["(OP2 VAR VAR)"])
            .plug("OP2", &Workload::new(&["Gt", "Lt", "Ge", "Le", "Neq"]))
            .plug("VAR", &Workload::new(&["x", "y"]));

        // Notice that here, we're passing in the predicate workload _as_
        // the term workload to find rewrites over it. This will come in handy.
        let (rewrites, conditional) =
            bubbler.find_rewrites(&predicate_workload, &Workload::empty());

        let InferredFacts::Rewrites(rewrites) = rewrites else {
            panic!("Expected rewrites");
        };

        let InferredFacts::Rewrites(conditional) = conditional else {
            panic!("Expected rewrites");
        };

        assert!(conditional.is_empty(), "Expected no conditional rewrites");

        for r in rewrites {
            bubbler.register_rewrite(&r).unwrap();
        }

        let implications = bubbler.find_implications(&predicate_workload);

        let InferredFacts::Implications(implications) = implications else {
            panic!("Expected implications");
        };

        // We went from 8 to 3 implications!
        assert_eq!(implications.len(), 3);
    }
}

// We're not going to use this anywhere yet. This is still the dream!
// We just have some lower hanging fruit to pick first.
//
// #[derive(Clone, Debug, Hash, PartialEq, Eq)]
// enum LLVMMetaOp {
//     IsPositive,
//     IsNegative,
//     IsNotZero,
// }

// impl OpTrait for LLVMMetaOp {
//     fn arity(&self) -> usize {
//         match self {
//             LLVMMetaOp::IsPositive => 1,
//             LLVMMetaOp::IsNegative => 1,
//             LLVMMetaOp::IsNotZero => 1,
//         }
//     }

//     fn name(&self) -> &'static str {
//         match self {
//             LLVMMetaOp::IsPositive => "IsPositive",
//             LLVMMetaOp::IsNegative => "IsNegative",
//             LLVMMetaOp::IsNotZero => "IsNotZero",
//         }
//     }
// }

// impl FromStr for LLVMMetaOp {
//     type Err = String;

//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         match s {
//             "IsPositive" => Ok(LLVMMetaOp::IsPositive),
//             "IsNegative" => Ok(LLVMMetaOp::IsNegative),
//             "IsNotZero" => Ok(LLVMMetaOp::IsNotZero),
//             _ => Err(format!("Unknown LLVMMetaOp: {}", s)),
//         }
//     }
// }

// #[derive(Clone, Debug, Hash, PartialEq, Eq)]
// enum LLVMMetaConst {
//     Int(i64),
//     Bool(bool),
// }

// impl From<LLVMMetaConst> for BubbleConstant {
//     fn from(c: LLVMMetaConst) -> Self {
//         match c {
//             LLVMMetaConst::Int(i) => BubbleConstant::Int(i),
//             LLVMMetaConst::Bool(b) => BubbleConstant::Bool(b),
//         }
//     }
// }

// impl FromStr for LLVMMetaConst {
//     type Err = String;

//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         match s {
//             "true" => Ok(LLVMMetaConst::Bool(true)),
//             "false" => Ok(LLVMMetaConst::Bool(false)),
//             _ => {
//                 if let Ok(i) = s.parse::<i64>() {
//                     Ok(LLVMMetaConst::Int(i))
//                 } else {
//                     Err(format!("Unknown LLVMMetaConst: {}", s))
//                 }
//             }
//         }
//     }
// }
