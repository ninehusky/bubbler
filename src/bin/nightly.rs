use std::vec;

use bubbler::bubbler::{Bubbler, BubblerConfig, InferredFacts};
use bubbler::eval::{derivability::can_derive, stats::EvalStats};
use bubbler::language::{Language, PredicateTerm, Rewrite, Term};
use bubbler::record;

use bubbler::test_langs::llvm::{LLVMLang, LLVMLangOp};
// TEMP imports — you will replace these gradually
use ruler::enumo::Workload;

fn handwritten_ruleset() -> InferredFacts<LLVMLang> {
    // min(z, y) < min(x, y + c0) ==>  min(z, y) < x if  c0 > 0
    // min(z, y) < min(y + c0, x) ==>  min(z, y) < x if  c0 > 0
    // min(z, y + c0) < min(x, y) ==>  min(z, y + c0) < x if  c0 < 0
    // min(z, y + c0) < min(y, x) ==>  min(z, y + c0) < x if  c0 < 0
    // min(y, z) < min(x, y + c0) ==>  min(z, y) < x if  c0 > 0
    // min(y, z) < min(y + c0, x) ==>  min(z, y) < x if  c0 > 0
    // min(y + c0, z) < min(x, y) ==>  min(z, y + c0) < x if  c0 < 0
    // min(y + c0, z) < min(y, x) ==>  min(z, y + c0) < x if  c0 < 0
    // max(z, y) < max(x, y + c0) ==>  max(z, y) < x if  c0 < 0
    // max(z, y) < max(y + c0, x) ==>  max(z, y) < x if  c0 < 0
    // max(z, y + c0) < max(x, y) ==>  max(z, y + c0) < x if  c0 > 0
    // max(z, y + c0) < max(y, x) ==>  max(z, y + c0) < x if  c0 > 0
    // max(y, z) < max(x, y + c0) ==>  max(z, y) < x if  c0 < 0
    // max(y, z) < max(y + c0, x) ==>  max(z, y) < x if  c0 < 0
    // max(y + c0, z) < max(x, y) ==>  max(z, y + c0) < x if  c0 > 0
    // max(y + c0, z) < max(y, x) ==>  max(z, y + c0) < x if  c0 > 0

    let mut results: Vec<Rewrite<LLVMLang>> = vec![];

    // x * c0 < y * c0 ==>  x < y if  c0 > 0
    results.push(
        Rewrite::new(
            Some(PredicateTerm::from_term(Term::Call(
                LLVMLangOp::Gt,
                vec![Term::Var("c0".into()), Term::Const(0)],
            ))),
            Term::Call(
                LLVMLangOp::Lt,
                vec![
                    Term::Call(
                        LLVMLangOp::Mul,
                        vec![Term::Var("x".into()), Term::Var("c0".into())],
                    ),
                    Term::Call(
                        LLVMLangOp::Mul,
                        vec![Term::Var("y".into()), Term::Var("c0".into())],
                    ),
                ],
            ),
            Term::Call(
                LLVMLangOp::Lt,
                vec![Term::Var("x".into()), Term::Var("y".into())],
            ),
        )
        .unwrap(),
    );

    // x * c0 < y * c0 ==>  y < x if  c0 < 0
    results.push(
        Rewrite::new(
            Some(PredicateTerm::from_term(Term::Call(
                LLVMLangOp::Lt,
                vec![Term::Var("c0".into()), Term::Const(0)],
            ))),
            Term::Call(
                LLVMLangOp::Lt,
                vec![
                    Term::Call(
                        LLVMLangOp::Mul,
                        vec![Term::Var("x".into()), Term::Var("c0".into())],
                    ),
                    Term::Call(
                        LLVMLangOp::Mul,
                        vec![Term::Var("y".into()), Term::Var("c0".into())],
                    ),
                ],
            ),
            Term::Call(
                LLVMLangOp::Lt,
                vec![Term::Var("y".into()), Term::Var("x".into())],
            ),
        )
        .unwrap(),
    );

    // x / c0 < c1 ==>  x < c1 * c0 if  c0 > 0
    results.push(
        Rewrite::new(
            Some(PredicateTerm::from_term(Term::Call(
                LLVMLangOp::Gt,
                vec![Term::Var("c0".into()), Term::Const(0)],
            ))),
            Term::Call(
                LLVMLangOp::Lt,
                vec![
                    Term::Call(
                        LLVMLangOp::Div,
                        vec![Term::Var("x".into()), Term::Var("c0".into())],
                    ),
                    Term::Var("c1".into()),
                ],
            ),
            Term::Call(
                LLVMLangOp::Lt,
                vec![
                    Term::Var("x".into()),
                    Term::Call(
                        LLVMLangOp::Mul,
                        vec![Term::Var("c1".into()), Term::Var("c0".into())],
                    ),
                ],
            ),
        )
        .unwrap(),
    );

    InferredFacts::Rewrites(results)
}

fn inferred_ruleset(stats: &mut EvalStats) -> (InferredFacts<LLVMLang>, InferredFacts<LLVMLang>) {
    // 2. bubbler config (keep tiny)
    let mut bubbler: Bubbler<LLVMLang> = Bubbler::new(BubblerConfig::new(
        vec!["x".into(), "y".into()],
        vec![1, 2, 3],
    ));

    // 3. predicate workload (reuse what you already have)
    let predicate_workload = Workload::new(&["(OP2 VAR VAR)"])
        .plug("OP2", &Workload::new(&["Gt", "Lt", "Ge", "Le", "Neq"]))
        .plug("VAR", &Workload::new(&["x", "y"]));

    // 4. rewrite discovery
    let (rewrites, _) = record!(
        stats,
        "rewrite_discovery",
        bubbler.find_rewrites(&predicate_workload, &Workload::empty())
    );

    // 5. implication discovery
    let implications = record!(
        stats,
        "implication_discovery",
        bubbler.find_implications(&predicate_workload)
    );
    // Placeholder for inferred ruleset
    (rewrites, implications)
}

fn main() {
    println!("=== Bubbler nightly ===");

    let mut stats = EvalStats::default();

    let (inferred_rewrites, inferred_implications) = inferred_ruleset(&mut stats);

    let InferredFacts::Rewrites(inferred_rewrites) = inferred_rewrites else {
        panic!("Expected inferred rewrites");
    };

    let InferredFacts::Implications(inferred_implications) = inferred_implications else {
        panic!("Expected inferred implications");
    };

    let InferredFacts::Rewrites(handwritten_rewrites) = handwritten_ruleset() else {
        panic!("Expected handwritten rewrites");
    };

    for rw in &inferred_rewrites {
        println!("Inferred rewrite: {}", rw);
    }

    for imp in &inferred_implications {
        println!("Inferred implication: {}", imp);
    }

    let total_rules = handwritten_rewrites.len();
    let mut subsumed_rules = 0;
    for rule in &handwritten_rewrites {
        if can_derive(&inferred_rewrites, &inferred_implications, &rule) {
            subsumed_rules += 1;
        }
    }

    // 6. coarse “result” (placeholder)
    println!("Rules checked: {total_rules}");
    println!(
        "Subsumed: {} ({:.1}%)",
        subsumed_rules,
        100.0 * subsumed_rules as f64 / total_rules as f64
    );

    println!("\nTiming:");
    for (k, v) in stats.times {
        println!("  {k}: {} ms", v.as_millis());
    }
}
