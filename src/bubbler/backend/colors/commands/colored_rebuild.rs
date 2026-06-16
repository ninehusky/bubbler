use crate::bubbler::backend::colors::context::with_lattice;
use crate::bubbler::backend::colors::Lattice;
use crate::language::Language;
use egglog::UserDefinedCommand;

/// (colored-rebuild)
///
/// Rebuilds the lattice to maintain the colored invariant. The colored
/// invariant is that for any two colors `p` and `q`, if `p ==> q`, then
/// for any terms `a`, `b` where `a ==_p b`, we also have to push
/// `a ==_q b`.
///
/// To ground this in an example: consider `p = (x > 0)`, `q = (x != 0)`.
/// We know that `(x > 0) ==> (x != 0)`, so if we have
/// done a colored-merge to insert the fact `(x / x) ==_q 1`, then `(colored-rebuild)` would also insert the fact `(x / x) ==_p 1`,
/// because `(x > 0)` implies `(x != 0)`.
///
/// Or something.
///
pub struct ColoredRebuild<L: Language>(std::marker::PhantomData<fn() -> L>);

impl<L: Language> Default for ColoredRebuild<L> {
    fn default() -> Self {
        ColoredRebuild(std::marker::PhantomData)
    }
}

impl<L: Language> UserDefinedCommand for ColoredRebuild<L> {
    fn update(
        &self,
        egraph: &mut egglog::EGraph,
        args: &[egglog::ast::Expr],
    ) -> Result<Option<egglog::CommandOutput>, egglog::Error> {
        if args.len() != 0 {
            return Err(egglog::Error::BackendError(format!(
                "colored-rebuild expects 0 arguments, got {}",
                args.len()
            )));
        }

        with_lattice(|lattice: &mut Lattice<'_, L>| {
            lattice.rebuild();
        });

        Ok(None)
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::{
        bubbler::{
            backend::{colors::context::set_lattice, EgglogBackend},
            Condition, Implication,
        },
        language::{PredicateTerm, Term},
        test_langs::llvm::{LLVMLang, LLVMLangOp},
    };

    #[test]
    pub fn rebuild_ok() {
        // first, construct a lattice with two predicates.
        let mut lattice: Lattice<LLVMLang> = Lattice::new();
        let p = PredicateTerm::from_term(Term::Call(
            LLVMLangOp::Gt,
            vec![Term::Var("x".into()), Term::Const(0)],
        ));
        let q = PredicateTerm::from_term(Term::Call(
            LLVMLangOp::Neq,
            vec![Term::Var("x".into()), Term::Const(0)],
        ));
        let p_cid = lattice.add_color(p.clone());
        let q_cid = lattice.add_color(q.clone());

        lattice
            .add_implication(Implication::new(Condition::Predicate(p.clone()), q.clone()).unwrap());

        set_lattice(&lattice);

        // set the lattice to this one.

        // then, add a colored equality under `q` but not `p`.
        let mut backend = crate::bubbler::backend::EgglogBackend::<LLVMLang>::new();
        let x_div_x = backend
            .add_term(
                Term::Call(
                    LLVMLangOp::Div,
                    vec![Term::Var("x".into()), Term::Var("x".into())],
                ),
                false,
            )
            .unwrap();
        let one = backend.add_term(Term::Const(1.into()), false).unwrap();

        backend.egraph.add_command(
            "colored-find".into(),
            std::sync::Arc::new(
                crate::bubbler::backend::colors::commands::colored_find::ColoredFind::<LLVMLang>::default(),
            ),
        ).unwrap();

        backend
            .egraph
            .add_command(
                "colored-merge".into(),
                std::sync::Arc::new(
                    crate::bubbler::backend::colors::commands::colored_merge::ColoredMerge::<
                        LLVMLang,
                    >::default(),
                ),
            )
            .unwrap();

        backend
            .egraph
            .add_command(
                "colored-rebuild".into(),
                std::sync::Arc::new(ColoredRebuild::<LLVMLang>::default()),
            )
            .unwrap();

        lattice.colored_merge(&mut backend.egraph, p_cid, x_div_x, one);

        assert!(
            lattice.colored_find(&mut backend.egraph, p_cid, x_div_x)
                == lattice.colored_find(&mut backend.egraph, p_cid, one)
        );

        // okay, so now the test can actually do something.
        backend
            .egraph
            .parse_and_run_program(
                None,
                r#"
            (let p (BaseTerm (Gt (Var "x") (Const 0))))
            (let q (BaseTerm (Neq (Var "x") (Const 0))))
            (colored-rebuild)

            ;;; should succeed now that we've rebuilt the lattice
            (check (= (colored-find q (Div (Var "x") (Var "x"))) (Const 1)))
            "#,
            )
            .unwrap();
    }
}
