# Bubbler 🐳

A simple programming language implementation in Rust that generates egglog datatype definitions.

## Overview

Bubbler is a Rust library and binary that implements a minimal programming language called **BubbleLang**. The language supports basic arithmetic and comparison operations, and can convert between S-expression representations and native AST structures. It's designed to generate egglog datatype definitions for use with the [egglog](https://github.com/egraphs-good/egglog) framework.

## Features

- **Simple Language**: BubbleLang supports integers, variables, addition, and less-than comparisons
- **S-expression Parsing**: Convert between S-expressions and AST representations
- **Egglog Integration**: Generate egglog datatype definitions
- **Type System**: Basic type checking with integer and boolean types
- **Aquatic Theme**: Because who doesn't love fish? 🐟

## Installation

### Prerequisites

- Rust 1.70+ (uses Rust edition 2024)
- Cargo (comes with Rust)

### Building from Source

```bash
git clone https://github.com/ninehusky/bubbler.git
cd bubbler
cargo build --release
```

### Running

```bash
# Run the main binary
cargo run

# Run tests
cargo test

# Run with optimizations
cargo run --release
```

## Usage

### As a Binary

The main binary demonstrates the egglog datatype generation:

```bash
$ cargo run
Egglog definition:
(datatype BubbleLang
  (Int i64)
  (Var String)
  (Add BubbleLang BubbleLang)
  (Lt BubbleLang BubbleLang)
)
```

### As a Library

Add Bubbler to your `Cargo.toml`:

```toml
[dependencies]
bubbler = "0.1.0"
```

Then use it in your code:

```rust
use bubbler::language::{BubbleLang, Language};

fn main() {
    // Generate egglog datatype definition
    let egglog_def = BubbleLang::to_egglog_src();
    println!("{}", egglog_def);

    // Parse S-expressions
    let expr = BubbleLang::parse("(Add (Int 1) (Int 2))").unwrap();
    println!("Parsed: {:?}", expr);

    // Convert back to S-expression
    let sexp = expr.to_sexp();
    println!("S-expression: {}", sexp);

    // Say hello from a fish! 🐟
    bubbler::hello();
}
```

## Language Reference

### BubbleLang Syntax

BubbleLang expressions are represented as S-expressions with the following grammar:

```
<expr> ::= (Int <integer>)
         | (Var <variable-name>)
         | (Add <expr> <expr>)
         | (Lt <expr> <expr>)
```

### Examples

```lisp
; Integer literal
(Int 42)

; Variable
(Var "x")

; Addition
(Add (Int 1) (Int 2))

; Less-than comparison
(Lt (Var "x") (Int 10))

; Nested expressions
(Add (Int 1) (Lt (Var "y") (Int 5)))
```

### Type System

BubbleLang has a simple type system with two types:

- `Int`: 64-bit signed integers
- `Bool`: Boolean values (result of comparisons)

## API Documentation

### Core Traits

#### `Language` Trait

The main trait that defines language operations:

```rust
pub trait Language: Sized + Clone {
    type Var: Clone + Into<String>;
    type Constant: Clone + Hash + Eq + Debug + Display + Ord;

    fn name() -> &'static str;
    fn op(&self) -> &str;
    fn from_sexp(sexp: &Sexp) -> Result<Self, &'static str>;
    fn to_sexp(&self) -> Sexp;
    fn parse(s: &str) -> Result<Self, &'static str>;
    fn schema() -> Vec<(&'static str, Vec<&'static str>)>;
    fn to_egglog_src() -> String;
}
```

#### `BubbleLang` Enum

The main language AST:

```rust
pub enum BubbleLang {
    Int(i64),
    Var(String),
    Add(Box<BubbleLang>, Box<BubbleLang>),
    Lt(Box<BubbleLang>, Box<BubbleLang>),
}
```

### Utility Functions

- `bubbler::hello()` - Prints a friendly fish message 🐟
- `bubbler::language::say_hi()` - Says hello from a Bubbler 🐳

## Development

### Project Structure

```
src/
├── lib.rs              # Library entry point
├── main.rs             # Binary entry point
└── language/
    ├── mod.rs          # Language implementation
    └── sexp.rs         # S-expression parsing
```

### Running Tests

```bash
# Run all tests
cargo test

# Run tests with output
cargo test -- --nocapture

# Run specific test
cargo test sexp::test::from_str
```

### Code Quality

The project uses several tools to maintain code quality:

```bash
# Format code
cargo fmt

# Lint code
cargo clippy

# Check all targets
cargo clippy --all-targets --all-features -- -D warnings
```

### Dependencies

- `egglog` (0.5.0) - Egglog framework integration
- `symbolic_expressions` (5.0.3) - S-expression parsing

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Run tests (`cargo test`)
5. Run formatting (`cargo fmt`)
6. Run linting (`cargo clippy`)
7. Commit your changes (`git commit -m 'Add amazing feature'`)
8. Push to the branch (`git push origin feature/amazing-feature`)
9. Open a Pull Request

Please ensure your code follows the existing style and includes appropriate tests.

## CI/CD

The project uses GitHub Actions for continuous integration:

- **Rustfmt**: Ensures consistent code formatting
- **Clippy**: Catches common mistakes and enforces best practices  
- **Tests**: Runs the full test suite in release mode

All checks must pass before merging pull requests.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- The S-expression implementation is "lovingly stolen" from [Enumo's ruler project](https://github.com/uwplse/ruler/blob/main/src/enumo/sexp.rs) 
- Built with the [egglog](https://github.com/egraphs-good/egglog) framework
- Inspired by aquatic life 🐟🐳

---

*Made with 🐟 by the Bubbler team*