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
    Or,
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
            LLVMLangOp::Or => 2,
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
            LLVMLangOp::Or => "Or",
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

    fn is_conjunction(&self) -> bool { matches!(self, LLVMLangOp::And) }
    fn is_disjunction(&self) -> bool { matches!(self, LLVMLangOp::Or) }
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
            "Or" => Ok(LLVMLangOp::Or),
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
            LLVMLangOp::Or,
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
            LLVMLangOp::Or => {
                let left_vec = &child_vecs[0];
                let right_vec = &child_vecs[1];
                left_vec
                    .iter()
                    .zip(right_vec.iter())
                    .map(|(l, r)| match (l, r) {
                        (Some(lv), Some(rv)) => Some(((*lv != 0) || (*rv != 0)) as i64),
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
        let bubbler: Bubbler<LLVMLang> = Bubbler::new(BubblerConfig::new(vec!["x".into()], vec![]));

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
            axioms
                .iter()
                .any(|p: &crate::language::PredicateTerm<LLVMLang>| {
                    let s = p.term.to_sexp().to_string();
                    s.contains(op) && s.contains(inner)
                })
        };

        assert!(
            matches(&axioms, "Ge", "Abs"),
            "expected (Ge (Abs x) 0) as axiom; got: {:?}",
            axioms
                .iter()
                .map(|p| p.term.to_sexp().to_string())
                .collect::<Vec<_>>()
        );
        assert!(
            matches(&axioms, "Ge", "Mul"),
            "expected (Ge (Mul x x) 0) as axiom; got: {:?}",
            axioms
                .iter()
                .map(|p| p.term.to_sexp().to_string())
                .collect::<Vec<_>>()
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
            axioms
                .iter()
                .map(|p| p.term.to_sexp().to_string())
                .collect::<Vec<_>>()
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

    // Schedule:
    //   Step 1 — synthesize predicate symmetry rewrites (Gt(x,0)≡Lt(0,x), And
    //            commutativity, etc.) so the e-graph collapses redundant forms.
    //   Step 2 — synthesize the predicate-strength lattice (pos→nonneg etc.)
    //            using the compressed representation.
    //   Step 3 — discover conjunctive implications over the narrowed And workload.
    #[test]
    fn discover_sign_implications() {
        let mut bubbler: Bubbler<LLVMLang> =
            Bubbler::new(BubblerConfig::new(vec!["x".into(), "y".into()], vec![]));

        let sign_ops = Workload::new(&["Gt", "Lt", "Ge", "Le", "Eq"]);

        // === Step 0: Find rewrites over the term language to simplify the CVec space before finding
        // implications.

        let vars = Workload::new(&["x", "y"]);
        let terms = Workload::new(&["(OP VAR VAR)"])
            .plug("OP", &Workload::new(&["And", "Gt", "Lt", "Ge", "Le", "Eq"]))
            .plug("VAR", &vars);

        let (rws, _) = bubbler.find_rewrites(&terms, &Workload::empty());
        let InferredFacts::Rewrites(rws) = rws else {
            panic!("Expected rewrites")
        };
        for rw in &rws {
            println!("=== Pre-implication rewrite: {}", rw);
            bubbler.register_rewrite(rw).unwrap();
        }

        // === Step 1: Predicate symmetry rewrites ===
        // Include both (SIGN VAR 0) and (SIGN 0 VAR) so find_rewrites sees that
        // e.g. Gt(x,0) and Lt(0,x) have the same CVec → discovered as equivalent.
        // Also include cross-variable And predicates to find commutativity.
        let x_preds = Workload::new(&["(SIGN x 0)"]).plug("SIGN", &sign_ops);
        let y_preds = Workload::new(&["(SIGN y 0)"]).plug("SIGN", &sign_ops);

        // Signedness only: (SIGN x 0) and (SIGN 0 x) to discover the constant-flip
        // rewrites (Gt(x,0) ↔ Lt(0,x) etc.). General order rewrites (Gt ?a ?b) ↔
        // (Lt ?b ?a) are a separate phase outside signedness analysis.
        let sym_atom_preds = Workload::new(&["(SIGN VAR 0)", "(SIGN 0 VAR)"])
            .plug("SIGN", &sign_ops)
            .plug("VAR", &Workload::new(&["x", "y"]));
        let sym_and_preds = Workload::new(&["(And P Q)"])
            .plug("P", &x_preds)
            .plug("Q", &y_preds);
        let sym_wkld = sym_atom_preds.append(sym_and_preds);

        let (sym_rewrites, _) = bubbler.find_rewrites(&sym_wkld, &Workload::empty());
        let InferredFacts::Rewrites(sym_rewrites) = sym_rewrites else {
            panic!("Expected rewrites")
        };
        println!("=== Symmetry rewrites ({}) ===", sym_rewrites.len());
        for rw in &sym_rewrites {
            println!("  {}", rw);
        }
        for rw in &sym_rewrites {
            bubbler.register_rewrite(rw).unwrap();
        }

        // === Step 2: Predicate-strength lattice (Phase 0) ===
        // Run find_implications over single-variable predicates only; the symmetry
        // rewrites above compress the PVec space before this runs.
        let lattice_wkld = Workload::new(&["(SIGN VAR 0)"])
            .plug("SIGN", &sign_ops)
            .plug("VAR", &Workload::new(&["x", "y"]));

        let lattice_imps = bubbler.find_implications(&lattice_wkld);
        let InferredFacts::Implications(lattice_imps) = lattice_imps else {
            panic!("Expected implications")
        };
        println!("=== Lattice implications ({}) ===", lattice_imps.len());
        for imp in &lattice_imps {
            println!("  {}", imp);
        }
        for imp in &lattice_imps {
            bubbler.register_implication(imp).unwrap();
        }

        // === Step 3: Conjunctive implication discovery ===
        // Now with symmetries and lattice known, enumerate single predicates over
        // compound terms plus cross-variable And predicates.
        let compound_terms =
            Workload::new(&["x", "y", "(Add x y)", "(Mul x y)", "(Neg x)", "(Abs x)"]);
        let single_preds = Workload::new(&["(SIGN TERM 0)"])
            .plug("SIGN", &sign_ops)
            .plug("TERM", &compound_terms);
        let and_preds = Workload::new(&["(And P Q)"])
            .plug("P", &x_preds)
            .plug("Q", &y_preds);

        // Within-variable Or, distinct-predicate pairs only (no Or(P,P)).
        // Or(Gt,Ge), Or(Lt,Le), Or(Ge,Le) etc. collapse to the broader predicate
        // or a tautology; the PVec-lattice filter in PvecMatch prunes those.
        // Or(Gt,Lt) = nonzero is the genuinely new case-split predicate.
        let x_or_preds = Workload::new(&[
            "(Or (Gt x 0) (Lt x 0))", "(Or (Gt x 0) (Ge x 0))", "(Or (Gt x 0) (Le x 0))",
            "(Or (Gt x 0) (Eq x 0))", "(Or (Lt x 0) (Ge x 0))", "(Or (Lt x 0) (Le x 0))",
            "(Or (Lt x 0) (Eq x 0))", "(Or (Ge x 0) (Le x 0))", "(Or (Ge x 0) (Eq x 0))",
            "(Or (Le x 0) (Eq x 0))",
        ]);
        let y_or_preds = Workload::new(&[
            "(Or (Gt y 0) (Lt y 0))", "(Or (Gt y 0) (Ge y 0))", "(Or (Gt y 0) (Le y 0))",
            "(Or (Gt y 0) (Eq y 0))", "(Or (Lt y 0) (Ge y 0))", "(Or (Lt y 0) (Le y 0))",
            "(Or (Lt y 0) (Eq y 0))", "(Or (Ge y 0) (Le y 0))", "(Or (Ge y 0) (Eq y 0))",
            "(Or (Le y 0) (Eq y 0))",
        ]);

        let pred_wkld = single_preds.append(and_preds).append(x_or_preds).append(y_or_preds);

        let result = bubbler.find_implications(&pred_wkld);
        let InferredFacts::Implications(implications) = result else {
            panic!("Expected implications")
        };

        println!("=== Conjunctive implications ({}) ===", implications.len());
        for imp in &implications {
            println!("  {}", imp);
        }

        // Helper: check a single-antecedent implication by outer operator.
        let has_impl = |from_op: &LLVMLangOp, to_op: &LLVMLangOp| {
            implications.iter().any(|imp| {
                let Condition::Predicate(fp) = &imp.from else {
                    return false;
                };
                let Term::Call(f, _) = &fp.term else {
                    return false;
                };
                let Term::Call(t, _) = &imp.to.term else {
                    return false;
                };
                f == from_op && t == to_op
            })
        };

        // Helper: check a conjunctive And(sign1, sign2) → sign3 implication.
        let has_conj_impl = |p_op: &LLVMLangOp, q_op: &LLVMLangOp, to_op: &LLVMLangOp| {
            implications.iter().any(|imp| {
                let Condition::Predicate(fp) = &imp.from else {
                    return false;
                };
                let Term::Call(LLVMLangOp::And, args) = &fp.term else {
                    return false;
                };
                if args.len() != 2 {
                    return false;
                }
                let Term::Call(p, _) = &args[0] else {
                    return false;
                };
                let Term::Call(q, _) = &args[1] else {
                    return false;
                };
                let Term::Call(t, _) = &imp.to.term else {
                    return false;
                };
                p == p_op && q == q_op && t == to_op
            })
        };

        let dump = || implications.iter().map(|i| i.to_string()).collect::<Vec<_>>();

        // Lattice ordering: these are in lattice_imps (registered as context),
        // NOT in the conjunctive phase output. Check lattice_imps directly.
        // neg→nonpos appears as (Lt ?a 0) → (Ge 0 ?a) due to Le↔Ge symmetry.
        let has_lattice_impl = |from_op: &LLVMLangOp, to_op: &LLVMLangOp| {
            lattice_imps.iter().any(|imp| {
                let Condition::Predicate(fp) = &imp.from else { return false };
                let Term::Call(f, _) = &fp.term else { return false };
                let Term::Call(t, _) = &imp.to.term else { return false };
                f == from_op && t == to_op
            })
        };
        assert!(has_lattice_impl(&LLVMLangOp::Gt, &LLVMLangOp::Ge), "pos → nonneg");
        // neg → nonpos appears as (Lt ?a 0) → (Ge 0 ?a) via Le↔Ge symmetry rewrite.
        assert!(has_lattice_impl(&LLVMLangOp::Lt, &LLVMLangOp::Ge), "neg → nonpos (as Ge(0,x))");

        // Mul sign case-splits via Or: Gt(Mul(a,b), 0) → Or(nonzero(a/b)).
        // The antecedent outer op is Gt or Lt; Mul is nested as the first argument.
        // NOTE: pos(a)∧pos(b)→pos(a+b) is not yet discovered — likely a workload gap.
        // Add(x,y) is in compound_terms but PVec for And(pos(x),pos(y))→pos(Add(x,y))
        // may not be generated. Next thing to investigate.
        assert!(
            implications.iter().any(|imp| {
                let Condition::Predicate(fp) = &imp.from else { return false };
                let Term::Call(sign, args) = &fp.term else { return false };
                if !matches!(sign, LLVMLangOp::Gt | LLVMLangOp::Lt) { return false }
                if args.is_empty() { return false }
                let is_mul_arg = matches!(&args[0], Term::Call(LLVMLangOp::Mul, _));
                is_mul_arg && matches!(&imp.to.term, Term::Call(LLVMLangOp::Or, _))
            }),
            "Expected Mul-sign → Or(nonzero) rule; got: {:?}", dump()
        );
    }

    // Phase C: conditional rewrites via find_rewrites.
    // Expected: nonneg(x) → abs(x) ≡ x  and  nonpos(x) → abs(x) ≡ -x
    #[test]
    fn discover_sign_conditional_rewrites() {
        let mut bubbler: Bubbler<LLVMLang> =
            Bubbler::new(BubblerConfig::new(vec!["x".into()], vec![]));

        let term_wkld = Workload::new(&["x", "(Abs x)", "(Neg x)"]);
        let sign_preds = Workload::new(&["(Ge x 0)", "(Gt x 0)", "(Le x 0)", "(Lt x 0)"]);

        let (_, cond_rewrites) = bubbler.find_rewrites(&term_wkld, &sign_preds);
        let InferredFacts::Rewrites(cond_rewrites) = cond_rewrites else {
            panic!("Expected rewrites")
        };

        println!(
            "=== Discovered conditional rewrites ({}) ===",
            cond_rewrites.len()
        );
        for rw in &cond_rewrites {
            println!("  {}", rw);
        }

        // nonneg(x) → abs(x) ≡ x
        let has_cond_rw = |cond_op: &LLVMLangOp, lhs_op: &LLVMLangOp, rhs_is_var: bool| {
            cond_rewrites.iter().any(|rw| {
                let Some(cond) = rw.cond_concrete() else {
                    return false;
                };
                let Term::Call(c, _) = &cond.term else {
                    return false;
                };
                if c != cond_op {
                    return false;
                }
                let lhs = rw.lhs_concrete();
                let rhs = rw.rhs_concrete();
                let lhs_ok = matches!(&lhs, Term::Call(op, _) if op == lhs_op);
                let rhs_ok = if rhs_is_var {
                    matches!(rhs, Term::Var(_))
                } else {
                    matches!(rhs, Term::Call(LLVMLangOp::Neg, _))
                };
                lhs_ok && rhs_ok
            })
        };

        assert!(
            has_cond_rw(&LLVMLangOp::Ge, &LLVMLangOp::Abs, true),
            "Expected nonneg(x) → abs(x) ≡ x; got: {:?}",
            cond_rewrites
                .iter()
                .map(|r| r.to_string())
                .collect::<Vec<_>>()
        );
        assert!(
            has_cond_rw(&LLVMLangOp::Le, &LLVMLangOp::Abs, false),
            "Expected nonpos(x) → abs(x) ≡ -x; got: {:?}",
            cond_rewrites
                .iter()
                .map(|r| r.to_string())
                .collect::<Vec<_>>()
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
