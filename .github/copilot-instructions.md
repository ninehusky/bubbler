# Copilot Instructions for Bubbler

## Project Overview

Bubbler is a Rust library that implements term rewriting and analysis using e-graphs and characteristic vectors. The project builds on top of the `egglog` library to provide a framework for symbolic computation and program analysis.

## Key Concepts

- **E-graphs**: Equivalence graphs used for representing and manipulating expressions
- **Characteristic Vectors (CVec)**: Vectors that characterize terms by evaluating them across different variable assignments
- **BubbleLang**: A domain-specific language with integers, variables, addition, and comparison operations
- **Environment**: Maps variable names to sets of constant values for evaluation
- **Rewrites and Implications**: Rules for transforming and reasoning about terms

## Code Structure

- `src/lib.rs`: Main library entry point
- `src/main.rs`: Executable entry point demonstrating egglog output
- `src/bubbler/mod.rs`: Core Bubbler implementation with e-graph management
- `src/language/mod.rs`: Language trait definition and BubbleLang implementation
- `src/language/sexp.rs`: S-expression parsing and formatting utilities
- `src/language/rule.rs`: Rewrite rules and implications data structures

## Key Types and Traits

- `Language`: Core trait defining language operations (parsing, evaluation, egglog generation)
- `Bubbler<L>`: Main struct managing e-graphs and characteristic vector caching
- `BubbleLang`: Concrete implementation supporting integers, variables, addition, and less-than comparison
- `CVec<L>`: Type alias for characteristic vectors (Vec<Option<Constant>>)
- `Environment<L>`: Type alias for variable-to-constants mappings

## Testing and Development

- Run tests with `cargo test`
- Check formatting with `cargo fmt --check`
- Run linting with `cargo clippy --all-targets --all-features -- -D warnings`
- The project uses standard Rust 2024 edition practices

## Code Style Guidelines

- Follow standard Rust formatting (enforced by rustfmt)
- Use descriptive variable names that reflect the mathematical/logical concepts
- Maintain clear separation between language-agnostic traits and concrete implementations
- Include comprehensive error handling, especially for egglog interactions
- Write tests that validate both individual components and integration behavior

## Dependencies

- `egglog 0.5.0`: E-graph library for term rewriting and analysis
- `symbolic_expressions 5.0.3`: S-expression parsing and manipulation

## Common Patterns

- Use the `run_prog!` macro for executing egglog programs with debug output
- Cache characteristic vectors using hash-based lookups for performance
- Implement `Language` trait methods consistently for new language extensions
- Use `Result` types for operations that can fail (parsing, e-graph operations)
- Leverage Rust's type system to ensure language-specific operations are safe