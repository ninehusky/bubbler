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
    Eq,
    Neg,
    Abs,
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
            LLVMLangOp::Eq => 2,
            LLVMLangOp::Neg => 1,
            LLVMLangOp::Abs => 1,
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
            LLVMLangOp::Eq => "Eq",
            LLVMLangOp::Neg => "Neg",
            LLVMLangOp::Abs => "Abs",
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
            "Eq" => Ok(LLVMLangOp::Eq),
            "Neg" => Ok(LLVMLangOp::Neg),
            "Abs" => Ok(LLVMLangOp::Abs),
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
            LLVMLangOp::Eq,
            LLVMLangOp::Neg,
            LLVMLangOp::Abs,
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
            LLVMLangOp::Eq => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some((lv == rv) as i64),
                        _ => None,
                    })
                    .collect()
            }
            LLVMLangOp::Neg => {
                let child_vec = &child_vecs[0];
                child_vec.iter().map(|v| v.map(|cv| -cv)).collect()
            }
            LLVMLangOp::Abs => {
                let child_vec = &child_vecs[0];
                child_vec.iter().map(|v| v.map(|cv| cv.abs())).collect()
            }
        }
    }
}

#[cfg(test)]
#[allow(unused_imports)]
mod tests {
    use super::*;
    use crate::bubbler::{Bubbler, BubblerConfig, Condition, InferredFacts};
    use crate::language::term::Term;
    use ruler::enumo::Workload;

    // --- Phase 2: Axiom discovery ---

    /// Discriminating test: checks both that genuine axioms are found AND
    /// that contingent predicates are excluded.
    #[test]
    fn find_sign_axioms() {
        let bubbler: Bubbler<LLVMLang> =
            Bubbler::new(BubblerConfig::new(vec!["x".into()], vec![]));

        // Mix of axioms and non-axioms. interesting_constants includes -10, -1, 0, 1, 2, ...
        // so all slots are exercised.
        let wkld = Workload::new(&[
            "(Ge (Abs x) 0)",   // axiom: abs always >= 0
            "(Ge (Mul x x) 0)", // axiom: square always >= 0
            "(Ge x 0)",         // NOT axiom: false at x = -1
            "(Gt (Abs x) 0)",   // NOT axiom: false at x = 0
        ]);

        let result = bubbler.find_axioms(&wkld);
        let InferredFacts::Axioms(axioms) = result else {
            panic!("Expected axioms");
        };

        // to_sexp() produces "(Ge (Abs (Var x ) ) (Const 0 ) )" — match structurally.
        let matches = |axioms: &[_], op: &str, inner: &str| -> bool {
            axioms.iter().any(|p: &crate::language::PredicateTerm<LLVMLang>| {
                let s = p.term.to_sexp().to_string();
                s.contains(op) && s.contains(inner)
            })
        };

        assert!(
            matches(&axioms, "Ge", "Abs"),
            "expected (Ge (Abs x) 0) as axiom; got: {:?}",
            axioms.iter().map(|p| p.term.to_sexp().to_string()).collect::<Vec<_>>()
        );
        assert!(
            matches(&axioms, "Ge", "Mul"),
            "expected (Ge (Mul x x) 0) as axiom; got: {:?}",
            axioms.iter().map(|p| p.term.to_sexp().to_string()).collect::<Vec<_>>()
        );

        // Contingent predicates must not appear.
        let contingent_present = axioms.iter().any(|p| {
            let s = p.term.to_sexp().to_string();
            // (Ge x 0) — no Abs or Mul, just a bare Var
            (s.contains("Ge") && !s.contains("Abs") && !s.contains("Mul"))
                // (Gt (Abs x) 0)
                || (s.contains("Gt") && s.contains("Abs"))
        });
        assert!(
            !contingent_present,
            "contingent predicate found in axioms: {:?}",
            axioms.iter().map(|p| p.term.to_sexp().to_string()).collect::<Vec<_>>()
        );
    }

    // --- Phase 3: Signedness propagation (gating experiment) ---
    //
    // Ground truth for recall measurement:
    //   pos(x) ∧ pos(y) → pos(x+y)          [needs conjunction]
    //   neg(x) ∧ neg(y) → neg(x+y)          [needs conjunction]
    //   nonneg(x) ∧ nonneg(y) → nonneg(x+y) [needs conjunction]
    //   zero(x) → zero(x*y)                  [single-predicate — discoverable]
    //   pos(x) ∧ pos(y) → pos(x*y)           [needs conjunction]
    //   neg(x) ∧ neg(y) → pos(x*y)           [needs conjunction]
    //   pos(x) → neg(-x)                     [single-predicate — discoverable]
    //   neg(x) → pos(-x)                     [single-predicate — discoverable]
    //   zero(x) → zero(-x)                   [single-predicate — discoverable]
    //   nonneg(abs(x))                        [axiom — Phase 2]
    //
    // The current PvecMatch only surfaces single-predicate antecedent rules.
    // Conjunctive rules require an extended matcher. This test measures what
    // the existing loop actually finds and is intended to document the gap.

    // NOTE: two-variable predicate workloads (e.g. (Gt (Add x y) 0)) produce
    // implication candidates where the consequent contains a variable (`y`) that
    // does not appear in the antecedent. Egglog rejects these with an "Unbound"
    // error. Multi-variable discovery requires extending the implication model to
    // support conjunctive antecedents; this test documents what is discoverable
    // today with single-variable predicates only.
    #[test]
    fn discover_sign_implications() {
        let mut bubbler: Bubbler<LLVMLang> =
            Bubbler::new(BubblerConfig::new(vec!["x".into()], vec![]));

        // Single-variable predicate workload: sign predicates over x and unary terms.
        // pos(x) := (Gt x 0), neg(x) := (Lt x 0), nonneg(x) := (Ge x 0),
        // nonpos(x) := (Le x 0), zero(x) := (Eq x 0)
        let depth1_terms = Workload::new(&["x", "(Neg x)", "(Abs x)", "(Mul x x)"]);

        let pred_wkld = Workload::new(&["(SIGN TERM 0)"])
            .plug("SIGN", &Workload::new(&["Gt", "Lt", "Ge", "Le", "Eq"]))
            .plug("TERM", &depth1_terms);

        // Phase 1: find rewrites over the predicate language to reduce redundancy.
        let (pred_rewrites, _) = bubbler.find_rewrites(&pred_wkld, &Workload::empty());
        let InferredFacts::Rewrites(pred_rewrites) = pred_rewrites else {
            panic!("Expected rewrites");
        };
        for rw in &pred_rewrites {
            bubbler.register_rewrite(rw).unwrap();
        }

        // Phase 2: find implications.
        let result = bubbler.find_implications(&pred_wkld);
        let InferredFacts::Implications(implications) = result else {
            panic!("Expected implications");
        };

        println!("=== Discovered sign implications ({}) ===", implications.len());
        for imp in &implications {
            println!("  {}", imp);
        }

        // Rules 3–5 (pos↔neg(-x), neg↔pos(-x), zero↔zero(-x)) have identical
        // PVecs, so they are found as rewrites in phase 1 — not implications here.
        // The strictly one-way lattice-ordering implications are:
        //   pos(x)  → nonneg(x)   from: Gt, to: Ge
        //   neg(x)  → nonpos(x)   from: Lt, to: Le
        let has_implication = |from_op: &LLVMLangOp, to_op: &LLVMLangOp| {
            implications.iter().any(|imp| {
                let Condition::Predicate(from_pred) = &imp.from else {
                    return false;
                };
                let Term::Call(f, _) = &from_pred.term else {
                    return false;
                };
                let Term::Call(t, _) = &imp.to.term else {
                    return false;
                };
                f == from_op && t == to_op
            })
        };

        assert!(
            has_implication(&LLVMLangOp::Gt, &LLVMLangOp::Ge),
            "Expected pos(x) → nonneg(x); got: {:?}",
            implications.iter().map(|i| i.to_string()).collect::<Vec<_>>()
        );
        assert!(
            has_implication(&LLVMLangOp::Lt, &LLVMLangOp::Le),
            "Expected neg(x) → nonpos(x); got: {:?}",
            implications.iter().map(|i| i.to_string()).collect::<Vec<_>>()
        );
    }

    // --- Original implication tests ---

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
