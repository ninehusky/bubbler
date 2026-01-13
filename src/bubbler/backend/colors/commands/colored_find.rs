use std::{fmt::Display, sync::Arc};

use egglog::{CommandOutput, UserDefinedCommand};

use crate::{
    bubbler::backend::{
        EgglogBackend, colors::context::with_lattice, enodes::EClassId, uf::UFContext,
    },
    language::{Language, PredicateTerm},
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
        println!("debug: we're going to just assume the color is black.");
        let black_id = EgglogBackend::get_eclass_id(egraph, &color.term).unwrap();

        let rep = with_lattice::<L, _, _>(|lattice| {
            let uf = lattice
                .ufs
                .get(&lattice.top())
                .expect("Hey... where's the top UF?");

            let sort = egraph.get_sort_by_name(L::name()).unwrap();
            uf.peek(UFContext::EGraph { egraph, sort }, black_id.0)
        });

        let output = ColoredFindOutput {
            value: EClassId(rep),
        };

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
        write!(f, "ColoredFindOutput({:?})", self.value)
    }
}

#[cfg(test)]
pub mod tests {
    use std::sync::Arc;

    use egglog::{
        prelude::{RustSpan, Span},
        span,
    };

    use super::ColoredFind;

    use crate::{
        bubbler::backend::{
            EgglogBackend,
            colors::{
                Lattice,
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

        let result = backend.egraph.parse_and_run_program(
            None,
            // the BaseTerm (Var "true") is just a placeholder for the color stuff.
            // in a separate PR, we'll actually look up the color e-class.
            r#"(colored-find (BaseTerm (Var "true")) (Add (Var "x") (Var "y")))"#,
        );

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

        assert!(result.is_ok(), "colored-find command failed: {:?}", result);
        assert_eq!(id, val);

        clear_lattice();
    }
}
