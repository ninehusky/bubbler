//! Test examples from README to ensure they work correctly

use crate::language::{BubbleLang, Language};

#[test]
fn test_readme_examples() {
    // Generate egglog datatype definition
    let egglog_def = BubbleLang::to_egglog_src();
    assert!(egglog_def.contains("(datatype BubbleLang"));
    assert!(egglog_def.contains("(Int i64)"));
    assert!(egglog_def.contains("(Var String)"));
    assert!(egglog_def.contains("(Add BubbleLang BubbleLang)"));
    assert!(egglog_def.contains("(Lt BubbleLang BubbleLang)"));

    // Parse S-expressions from README examples
    let examples = vec![
        "(Int 42)",
        "(Var \"x\")",
        "(Add (Int 1) (Int 2))",
        "(Lt (Var \"x\") (Int 10))",
        "(Add (Int 1) (Lt (Var \"y\") (Int 5)))",
    ];

    for example in examples {
        let expr = BubbleLang::parse(example)
            .unwrap_or_else(|e| panic!("Failed to parse '{}': {}", example, e));

        // Convert back to S-expression and ensure it's parseable
        let sexp = expr.to_sexp();
        let reparsed = BubbleLang::parse(&sexp.to_string())
            .unwrap_or_else(|e| panic!("Failed to reparse '{}': {}", sexp, e));

        // Verify round-trip parsing works
        assert_eq!(format!("{:?}", expr), format!("{:?}", reparsed));
    }
}

#[test]
fn test_hello_functions() {
    // These functions should not panic
    crate::hello();
    crate::language::say_hi();
}
