use std::vec;

use bubbler::bubbler::{Bubbler, BubblerConfig, InferredFacts};
use bubbler::colors::{Condition, Implication};
use bubbler::eval::{report::NightlyReport, stats::EvalStats};
use bubbler::language::{PredicateTerm, Rewrite, Term};
use bubbler::record;

use bubbler::test_langs::llvm::{LLVMLang, LLVMLangOp};
use ruler::enumo::Workload;

#[allow(clippy::vec_init_then_push)]
fn halide_handwritten_ruleset() -> InferredFacts<LLVMLang> {
    let mut results: Vec<Rewrite<LLVMLang>> = vec![];

    // x * c0 < y * c0 ~> x < y if c0 > 0
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

    InferredFacts::Rewrites(results)
}

fn fake_inferred_ruleset() -> (Vec<Rewrite<LLVMLang>>, Vec<Implication<LLVMLang>>) {
    let mut ruleset = vec![];

    // a < b ~> b > a
    ruleset.push(
        Rewrite::new(
            None,
            Term::Call(
                LLVMLangOp::Lt,
                vec![Term::Var("a".into()), Term::Var("b".into())],
            ),
            Term::Call(
                LLVMLangOp::Gt,
                vec![Term::Var("b".into()), Term::Var("a".into())],
            ),
        )
        .unwrap(),
    );

    // a < b ~> (b - a) > 0
    ruleset.push(
        Rewrite::new(
            None,
            Term::Call(
                LLVMLangOp::Lt,
                vec![Term::Var("a".into()), Term::Var("b".into())],
            ),
            Term::Call(
                LLVMLangOp::Gt,
                vec![
                    Term::Call(
                        LLVMLangOp::Sub,
                        vec![Term::Var("b".into()), Term::Var("a".into())],
                    ),
                    Term::Const(0),
                ],
            ),
        )
        .unwrap(),
    );

    // x * z - y * z ~> z * (x - y)
    ruleset.push(
        Rewrite::new(
            None,
            Term::Call(
                LLVMLangOp::Sub,
                vec![
                    Term::Call(
                        LLVMLangOp::Mul,
                        vec![Term::Var("x".into()), Term::Var("z".into())],
                    ),
                    Term::Call(
                        LLVMLangOp::Mul,
                        vec![Term::Var("y".into()), Term::Var("z".into())],
                    ),
                ],
            ),
            Term::Call(
                LLVMLangOp::Mul,
                vec![
                    Term::Var("z".into()),
                    Term::Call(
                        LLVMLangOp::Sub,
                        vec![Term::Var("x".into()), Term::Var("y".into())],
                    ),
                ],
            ),
        )
        .unwrap(),
    );

    // if b > 0 then a * b > c ~> a > c / b
    ruleset.push(
        Rewrite::new(
            Some(PredicateTerm::from_term(Term::Call(
                LLVMLangOp::Gt,
                vec![Term::Var("b".into()), Term::Const(0)],
            ))),
            Term::Call(
                LLVMLangOp::Gt,
                vec![
                    Term::Call(
                        LLVMLangOp::Mul,
                        vec![Term::Var("a".into()), Term::Var("b".into())],
                    ),
                    Term::Var("c".into()),
                ],
            ),
            Term::Call(
                LLVMLangOp::Gt,
                vec![
                    Term::Var("a".into()),
                    Term::Call(
                        LLVMLangOp::Div,
                        vec![Term::Var("c".into()), Term::Var("b".into())],
                    ),
                ],
            ),
        )
        .unwrap(),
    );

    // (b - a) > 0 ~> a < b
    ruleset.push(
        Rewrite::new(
            None,
            Term::Call(
                LLVMLangOp::Gt,
                vec![
                    Term::Call(
                        LLVMLangOp::Sub,
                        vec![Term::Var("b".into()), Term::Var("a".into())],
                    ),
                    Term::Const(0),
                ],
            ),
            Term::Call(
                LLVMLangOp::Lt,
                vec![Term::Var("a".into()), Term::Var("b".into())],
            ),
        )
        .unwrap(),
    );

    // if b < 0 then a * b > c ~> a < c / b
    ruleset.push(
        Rewrite::new(
            Some(PredicateTerm::from_term(Term::Call(
                LLVMLangOp::Lt,
                vec![Term::Var("b".into()), Term::Const(0)],
            ))),
            Term::Call(
                LLVMLangOp::Gt,
                vec![
                    Term::Call(
                        LLVMLangOp::Mul,
                        vec![Term::Var("a".into()), Term::Var("b".into())],
                    ),
                    Term::Var("c".into()),
                ],
            ),
            Term::Call(
                LLVMLangOp::Lt,
                vec![
                    Term::Var("a".into()),
                    Term::Call(
                        LLVMLangOp::Div,
                        vec![Term::Var("c".into()), Term::Var("b".into())],
                    ),
                ],
            ),
        )
        .unwrap(),
    );

    // (b - a) < c ~> b < a + c
    ruleset.push(
        Rewrite::new(
            None,
            Term::Call(
                LLVMLangOp::Lt,
                vec![
                    Term::Call(
                        LLVMLangOp::Sub,
                        vec![Term::Var("b".into()), Term::Var("a".into())],
                    ),
                    Term::Var("c".into()),
                ],
            ),
            Term::Call(
                LLVMLangOp::Lt,
                vec![
                    Term::Var("b".into()),
                    Term::Call(
                        LLVMLangOp::Add,
                        vec![Term::Var("a".into()), Term::Var("c".into())],
                    ),
                ],
            ),
        )
        .unwrap(),
    );

    // x + 0 ~> x
    ruleset.push(
        Rewrite::new(
            None,
            Term::Call(LLVMLangOp::Add, vec![Term::Var("x".into()), Term::Const(0)]),
            Term::Var("x".into()),
        )
        .unwrap(),
    );

    // a + b ~> b + a
    ruleset.push(
        Rewrite::new(
            None,
            Term::Call(
                LLVMLangOp::Add,
                vec![Term::Var("a".into()), Term::Var("b".into())],
            ),
            Term::Call(
                LLVMLangOp::Add,
                vec![Term::Var("b".into()), Term::Var("a".into())],
            ),
        )
        .unwrap(),
    );

    // a * b / b ~> a if b != 0
    ruleset.push(
        Rewrite::new(
            Some(PredicateTerm::from_term(Term::Call(
                LLVMLangOp::Neq,
                vec![Term::Var("b".into()), Term::Const(0)],
            ))),
            Term::Call(
                LLVMLangOp::Div,
                vec![
                    Term::Call(
                        LLVMLangOp::Mul,
                        vec![Term::Var("a".into()), Term::Var("b".into())],
                    ),
                    Term::Var("b".into()),
                ],
            ),
            Term::Var("a".into()),
        )
        .unwrap(),
    );

    let mut imps = vec![];

    // a < b --> a != b
    imps.push(
        Implication::new(
            Condition::from(PredicateTerm::from_term(Term::Call(
                LLVMLangOp::Lt,
                vec![Term::Var("a".into()), Term::Var("b".into())],
            ))),
            PredicateTerm::from_term(Term::Call(
                LLVMLangOp::Neq,
                vec![Term::Var("a".into()), Term::Var("b".into())],
            )),
        )
        .unwrap(),
    );

    // a > b --> a != b
    imps.push(
        Implication::new(
            Condition::from(PredicateTerm::from_term(Term::Call(
                LLVMLangOp::Gt,
                vec![Term::Var("a".into()), Term::Var("b".into())],
            ))),
            PredicateTerm::from_term(Term::Call(
                LLVMLangOp::Neq,
                vec![Term::Var("a".into()), Term::Var("b".into())],
            )),
        )
        .unwrap(),
    );

    (ruleset, imps)
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

    // Placeholder for inferred ruleset
    (rewrites, implications)
}

fn main() {
    println!("=== Running tha Bubbler nightly ===");

    let mut stats = EvalStats::default();

    // let (inferred_rewrites, inferred_implications) = inferred_ruleset(&mut stats);
    let (inferred_rewrites, inferred_implications) = fake_inferred_ruleset();

    let InferredFacts::Rewrites(handwritten_rewrites) = halide_handwritten_ruleset() else {
        panic!("Expected handwritten rewrites");
    };

    let report = NightlyReport::from_run(
        &inferred_rewrites,
        &inferred_implications,
        &handwritten_rewrites,
        &stats,
    );

    println!(
        "checked: {} | subsumed: {} ({:.1}%)",
        report.summary.rules_checked,
        report.summary.rules_subsumed,
        100.0 * report.summary.rules_subsumed as f64 / report.summary.rules_checked.max(1) as f64
    );

    std::fs::create_dir_all("artifacts").unwrap();
    std::fs::write(
        "artifacts/nightly.json",
        serde_json::to_string_pretty(&report).unwrap(),
    )
    .unwrap();

    let total_ms: u128 = report.stats.timings_ms.iter().map(|(_, t)| *t).sum();

    println!("time: {} ms total", total_ms);

    for (name, ms) in &report.stats.timings_ms {
        println!("  - {name}: {ms} ms");
    }

    println!("Wrote report to artifacts/nightly.json");
    println!("=== Done ===");
}
