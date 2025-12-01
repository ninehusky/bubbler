//! Defines an API for colored e-graph operations in egglog.
//! These are probably not going to be implemented nearly as efficiently
//! as they would be in Egg, but for prototyping this should reveal where
//! the bottlenecks are.
//!
//! In particular, we define operations for colored merge and find.
//! Maybe there are more later. Meh!

use egglog::{
    ast::{Expr, GenericExpr},
    CommandOutput, Error, UserDefinedCommand,
};

/// A colored find command for egglog.
/// Syntax:
/// (colored-find color term)
pub struct ColoredFind {}
i
impl UserDefinedCommand for ColoredFind {
    fn update(
        &self,
        egraph: &mut egglog::EGraph,
        args: &[Expr],
    ) -> Result<Option<CommandOutput>, Error> {
        // 1. Parse arguments.
        if args.len() != 2 {
            panic!(
                "Expected exactly 2 arguments to colored-find, got {}",
                args.len()
            );
        }

        match &args {
            &[GenericExpr::<String, String>::Lit(_, ref a), sexpr] => {
                println!("First arg (color): {}", a);
                println!("Second arg (term): {}", sexpr);
            }
        };

        let term_sexp = match &args[1] {
            Expr::Sexp(s) => s.clone(),
            _ => {
                return Err(Error::UserDefinedCommandError(
                    "Expected second argument to be a s-expression (term)".into(),
                ))
            }
        };
    }
}
