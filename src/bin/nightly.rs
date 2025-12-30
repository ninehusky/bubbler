use std::vec;

use bubbler::bubbler::{Bubbler, BubblerConfig, InferredFacts};
use bubbler::colors::Implication;
use bubbler::eval::{derivability::can_derive, stats::EvalStats};
use bubbler::language::{Language, PredicateTerm, Rewrite, Term};
use bubbler::record;

use bubbler::test_langs::llvm::{LLVMLang, LLVMLangOp};
use ruler::enumo::Workload;

fn handwritten_ruleset() -> InferredFacts<LLVMLang> {
    let mut results: Vec<Rewrite<LLVMLang>> = vec![];

    // min(z, y) < min(x, y + c0) ==>  min(z, y) < x if  c0 > 0
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
                        LLVMLangOp::Min,
                        vec![Term::Var("z".into()), Term::Var("y".into())],
                    ),
                    Term::Call(
                        LLVMLangOp::Min,
                        vec![
                            Term::Var("x".into()),
                            Term::Call(
                                LLVMLangOp::Add,
                                vec![Term::Var("y".into()), Term::Var("c0".into())],
                            ),
                        ],
                    ),
                ],
            ),
            Term::Call(
                LLVMLangOp::Lt,
                vec![
                    Term::Call(
                        LLVMLangOp::Min,
                        vec![Term::Var("z".into()), Term::Var("y".into())],
                    ),
                    Term::Var("x".into()),
                ],
            ),
        )
        .unwrap(),
    );

    // min(z, y) < min(y + c0, x) ==>  min(z, y) < x if  c0 > 0
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
                        LLVMLangOp::Min,
                        vec![Term::Var("z".into()), Term::Var("y".into())],
                    ),
                    Term::Call(
                        LLVMLangOp::Min,
                        vec![
                            Term::Call(
                                LLVMLangOp::Add,
                                vec![Term::Var("y".into()), Term::Var("c0".into())],
                            ),
                            Term::Var("x".into()),
                        ],
                    ),
                ],
            ),
            Term::Call(
                LLVMLangOp::Lt,
                vec![
                    Term::Call(
                        LLVMLangOp::Min,
                        vec![Term::Var("z".into()), Term::Var("y".into())],
                    ),
                    Term::Var("x".into()),
                ],
            ),
        )
        .unwrap(),
    );

    InferredFacts::Rewrites(results)
}

fn inferred_ruleset(stats: &mut EvalStats) -> (Vec<Rewrite<LLVMLang>>, Vec<Implication<LLVMLang>>) {
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

    let InferredFacts::Rewrites(rewrites) = rewrites else {
        panic!("Expected inferred rewrites");
    };

    for rw in &rewrites {
        bubbler
            .register_rewrite(rw)
            .expect("Failed to register inferred rewrite");
    }

    // 5. implication discovery
    let implications = record!(
        stats,
        "implication_discovery",
        bubbler.find_implications(&predicate_workload)
    );

    let InferredFacts::Implications(implications) = implications else {
        panic!("Expected inferred implications");
    };

    for imp in &implications {
        bubbler
            .register_implication(imp)
            .expect("Failed to register inferred implication");
    }

    let term_workload = Workload::new(&["(OP2 EXPR EXPR)"])
        .plug("OP2", &Workload::new(&["Gt", "Lt"]))
        .plug(
            "EXPR",
            &Workload::new(&["(Mul EXPR EXPR)"]).plug(
                "EXPR",
                &Workload::new(&["VAR", "(Add VAR VAR)"])
                    .plug("VAR", &Workload::new(&["x", "y", "z"])),
            ),
        );

    // Placeholder for inferred ruleset
    (rewrites, implications)
}

fn main() {
    println!("=== Bubbler nightly ===");

    let mut stats = EvalStats::default();

    let (inferred_rewrites, inferred_implications) = inferred_ruleset(&mut stats);

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
