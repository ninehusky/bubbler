use std::{any::Any, fmt::Display, str::FromStr, sync::Arc};

use egglog::{CommandOutput, UserDefinedCommand, Value, sort::S};

use crate::{
    bubbler::backend::{
        EgglogBackend, colors::context::with_lattice, enodes::EClassId, uf::UFContext,
    },
    language::{Language, PredicateTerm, Term},
};

/// `(colored-find c t)`
/// A command that returns the canonical representation for a term `t`
/// in a given color `c`.
/// This almost always means finding the representative term for
/// a colored e-class in the lattice structure.
#[allow(dead_code)]
pub struct ColoredFind<L: Language>(std::marker::PhantomData<fn() -> L>);

impl<L: Language> Default for ColoredFind<L> {
    fn default() -> Self {
        ColoredFind(std::marker::PhantomData)
    }
}

impl<L: Language> UserDefinedCommand for ColoredFind<L> {
    fn update(
        &self,
        egraph: &mut egglog::EGraph,
        args: &[egglog::ast::Expr],
    ) -> Result<Option<egglog::CommandOutput>, egglog::Error> {
        assert_eq!(args.len(), 2, "colored_find takes 2 arguments");
        let color: PredicateTerm<L> = args[0].clone().into();
        let term: Term<L> = args[1].clone().into();
        let black_id = EgglogBackend::get_eclass_id(egraph, &term).unwrap();

        let rep = with_lattice::<L, _, _>(|lattice| {
            let color_id = if color.is_true(egraph) {
                lattice.top
            } else {
                lattice.fact_node(color)
            };
            let canon_id =
                egraph.get_canonical_value(black_id.0, egraph.get_sort_by_name(L::name()).unwrap());
            lattice.colored_find(egraph, color_id, EClassId(canon_id))
        });

        let output = ColoredFindOutput { value: rep };

        Ok(Some(CommandOutput::UserDefined(Arc::new(output))))
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct ColoredFindOutput {
    pub value: EClassId,
}

impl Display for ColoredFindOutput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.value)
    }
}

impl FromStr for ColoredFindOutput {
    type Err = egglog::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // it should look like EClassId(Value(`actual_value`))
        let s = s.trim();
        if !s.starts_with("EClassId(Value(") || !s.ends_with("))") {
            return Err(egglog::Error::BackendError(format!(
                "Invalid ColoredFindOutput string: {}",
                s
            )));
        }

        let inner = &s["EClassId(Value(".len()..s.len() - 2];
        let value: egglog::Value = Value::new_const(inner.parse::<u32>().unwrap());
        Ok(ColoredFindOutput {
            value: EClassId(value),
        })
    }
}

#[cfg(test)]
pub mod tests {
    use std::{str::FromStr, sync::Arc};

    use egglog::{
        CommandOutput,
        prelude::{RustSpan, Span},
        span,
    };

    use super::ColoredFind;

    use crate::{
        bubbler::backend::{
            EgglogBackend,
            colors::{
                Lattice,
                commands::colored_find::ColoredFindOutput,
                context::{clear_lattice, set_lattice},
            },
        },
        language::{Language, Term},
        test_langs::llvm::{LLVMLang, LLVMLangOp},
    };

    #[test]
    fn colored_find_does_not_explode_upon_registration() {
        let mut backend: EgglogBackend<LLVMLang> = EgglogBackend::new();
        let lattice: Lattice<LLVMLang> = Lattice::new();

        set_lattice(&lattice);

        backend
            .egraph
            .add_command(
                "colored-find".to_string(),
                Arc::new(ColoredFind::<LLVMLang>::default()),
            )
            .unwrap();

        let term: Term<LLVMLang> = Term::Call(
            LLVMLangOp::Add,
            vec![Term::Var("x".into()), Term::Var("y".into())],
        );

        backend.add_term(term, false).unwrap();

        let result = backend
            .egraph
            .parse_and_run_program(
                None,
                // the BaseTerm (Var "true") is just a placeholder for the color stuff.
                // in a separate PR, we'll actually look up the color e-class.
                r#"(colored-find (BaseTerm (Var "true")) (Add (Var "x") (Var "y")))"#,
            )
            .expect("Failed to run colored-find command");

        assert_eq!(result.len(), 1);

        let result_val = match &result[0] {
            CommandOutput::UserDefined(output) => {
                let as_str = format!("{}", output);
                ColoredFindOutput::from_str(&as_str).unwrap().value
            }
            _ => panic!("Expected ColoredFindOutput"),
        };

        let msgs = backend
            .egraph
            .parse_and_run_program(None, r#"(extract (Add (Var "x") (Var "y")))"#)
            .unwrap();

        assert_eq!(msgs.len(), 1);

        let val = match &msgs[0] {
            egglog::CommandOutput::ExtractBest(termdag, _, term) => {
                let expr = termdag.term_to_expr(term, span!());
                backend.egraph.eval_expr(&expr).unwrap().1
            }
            _ => panic!("Expected extracted values"),
        };

        let sort = backend.egraph.get_sort_by_name(LLVMLang::name()).unwrap();
        let id = backend.egraph.get_canonical_value(val, sort);

        assert_eq!(id, result_val.0);

        clear_lattice();
    }
}
