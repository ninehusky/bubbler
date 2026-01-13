use std::{fmt::Display, sync::Arc};

use egglog::{CommandOutput, UserDefinedCommand, UserDefinedCommandOutput};

use crate::{
    bubbler::backend::union_find::UnionFindLike,
    bubbler::backend::{EgglogBackend, colors::Lattice, enodes::EClassId},
    language::{Language, PredicateTerm},
};

/// `(colored-find c t)`
/// A command that returns the canonical representation for a term `t`
/// in a given color `c`.
/// This almost always means finding the representative term for
/// a colored e-class in the lattice structure.
pub struct ColoredFind<'a, L: Language> {
    lattice: &'a Lattice<'a, L>,
}

impl<'a, L: Language> UserDefinedCommand for ColoredFind<'a, L> {
    fn update(
        &self,
        egraph: &mut egglog::EGraph,
        args: &[egglog::ast::Expr],
    ) -> Result<Option<egglog::CommandOutput>, egglog::Error> {
        assert_eq!(args.len(), 2, "colored_find takes 2 arguments");
        let color: PredicateTerm<L> = args[0].clone().into();
        let black_id = EgglogBackend::get_eclass_id(egraph, &color.term).unwrap();

        println!("debug: we're going to just assume the color is black.");

        let color = &self.lattice.top;

        // let Some(color) = self.lattice.facts.get(&color) else {
        //     return Err(egglog::Error::BackendError(format!(
        //         "Color {:?} not found in lattice",
        //         args[0]
        //     )));
        // };

        let res = self.lattice.ufs.get(color).ok_or_else(|| {
            egglog::Error::BackendError(format!(
                "No union-find found for color {:?} in lattice",
                args[0]
            ))
        })?;

        Ok(Some(CommandOutput::UserDefined(Arc::new(
            ColoredFindOutput {
                value: EClassId(res.peek(black_id.0)),
            },
        ))))
    }
}

#[derive(Debug)]
pub struct ColoredFindOutput {
    pub value: EClassId,
}

impl Display for ColoredFindOutput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ColoredFindOutput({:?})", self.value)
    }
}

#[cfg(test)]
pub mod tests {}
