use egglog::{
    UserDefinedCommand,
    ast::{GenericAction, GenericCommand},
    prelude::{RustSpan, Span, span},
};

use crate::{
    bubbler::backend::{EgglogBackend, colors::context::with_lattice},
    language::{Language, PredicateTerm, Term},
};

/// (colored-merge color1 t1 t2)
pub struct ColoredMerge<L: Language>(std::marker::PhantomData<fn() -> L>);

impl<L: Language> Default for ColoredMerge<L> {
    fn default() -> Self {
        ColoredMerge(std::marker::PhantomData)
    }
}

impl<L: Language> UserDefinedCommand for ColoredMerge<L> {
    fn update(
        &self,
        egraph: &mut egglog::EGraph,
        args: &[egglog::ast::Expr],
    ) -> Result<Option<egglog::CommandOutput>, egglog::Error> {
        if args.len() != 3 {
            return Err(egglog::Error::BackendError(format!(
                "colored-merge expects 3 arguments, got {}",
                args.len()
            )));
        }

        let a: Term<L> = args[1].clone().into();
        let b: Term<L> = args[2].clone().into();

        let color: PredicateTerm<L> = args[0].clone().into();
        if PredicateTerm::is_true(&color, egraph) {
            // do the union here
            egraph
                .run_program(vec![GenericCommand::Action(GenericAction::Union(
                    span!(),
                    a.into(),
                    b.into(),
                ))])
                .expect("Failed to run union action in colored-merge under top color.");
        } else {
            with_lattice(|lattice| {
                let a_id = EgglogBackend::get_eclass_id(egraph, &a).unwrap();
                let b_id = EgglogBackend::get_eclass_id(egraph, &b).unwrap();
                let fact_node = lattice.fact_node(color);
                lattice.colored_merge(egraph, fact_node, a_id, b_id);
            });
        }
        Ok(None)
    }
}

#[cfg(test)]
pub mod tests {
    use std::{sync::Arc, vec};

    use crate::{
        bubbler::backend::colors::{
            Lattice,
            commands::colored_find::ColoredFind,
            context::{clear_lattice, set_lattice},
        },
        test_langs::llvm::{LLVMLang, LLVMLangOp},
    };

    use super::*;

    #[test]
    fn colored_merge_basic() {
        let mut backend: EgglogBackend<LLVMLang> = EgglogBackend::new();
        let lattice = Lattice::<LLVMLang>::new();

        set_lattice(&lattice);

        backend
            .egraph
            .add_command(
                "colored-find".into(),
                Arc::new(ColoredFind::<LLVMLang>::default()),
            )
            .unwrap();

        backend
            .egraph
            .add_command(
                "colored-merge".into(),
                Arc::new(ColoredMerge::<LLVMLang>::default()),
            )
            .unwrap();

        backend
            .add_term(
                Term::Call(
                    LLVMLangOp::Div,
                    vec![Term::Var("x".into()), Term::Var("x".into())],
                ),
                false,
            )
            .unwrap();

        backend.add_term(Term::Const(1.into()), false).unwrap();

        let div_eclass_id = EgglogBackend::<LLVMLang>::get_eclass_id(
            &mut backend.egraph,
            &Term::Call(
                LLVMLangOp::Div,
                vec![Term::Var("x".into()), Term::Var("x".into())],
            ),
        )
        .unwrap();

        let one_eclass_id =
            EgglogBackend::<LLVMLang>::get_eclass_id(&mut backend.egraph, &Term::Const(1.into()))
                .unwrap();
        // at this point, we have two e-classes: one for (Div x x) and one for 1.
        // they should not be equal.

        with_lattice(|lattice: &mut Lattice<LLVMLang>| {
            let div_id = lattice.colored_find(&mut backend.egraph, lattice.top, div_eclass_id);
            let one_id = lattice.colored_find(&mut backend.egraph, lattice.top, one_eclass_id);

            assert!(div_id != one_id);
        });

        // now, do a colored-merge under the color "x != 0".
        with_lattice(|lattice: &mut Lattice<LLVMLang>| {
            let predicate: PredicateTerm<LLVMLang> = PredicateTerm::from_term(Term::Call(
                LLVMLangOp::Neq,
                vec![Term::Var("x".into()), Term::Const(0.into())],
            ));
            let fact_node = lattice.fact_node(predicate);
            lattice.colored_merge(&mut backend.egraph, fact_node, div_eclass_id, one_eclass_id);

            let div_id_in_color =
                lattice.colored_find(&mut backend.egraph, fact_node, div_eclass_id);
            let one_id_in_color =
                lattice.colored_find(&mut backend.egraph, fact_node, one_eclass_id);
            assert_eq!(div_id_in_color, one_id_in_color);

            let div_id_top = lattice.colored_find(&mut backend.egraph, lattice.top, div_eclass_id);
            let one_id_top = lattice.colored_find(&mut backend.egraph, lattice.top, one_eclass_id);
            assert!(div_id_top != one_id_top);
        });

        clear_lattice();
    }
}
