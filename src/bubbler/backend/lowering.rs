use egglog::{CommandOutput, EGraph, ast::{Expr, GenericCommand, Literal}, prelude::{RustSpan, Span}, span};

pub(crate) struct EgglogLowering<'a> {
	egraph: &'a mut EGraph,
}

impl<'a> EgglogLowering<'a> {
	pub(crate) fn new(egraph: &'a mut EGraph) -> Self {
		Self { egraph }
	}

	pub(crate) fn extract_best(&mut self, expr: Expr) -> Result<CommandOutput, egglog::Error> {
		let outputs = self
			.egraph
			.run_program(vec![GenericCommand::Extract(
				span!(),
				expr,
				Expr::Lit(span!(), Literal::Int(0)),
			)])?;

		Ok(outputs
			.into_iter()
			.next()
			.expect("extract should emit exactly one output"))
	}

	#[allow(dead_code)]
	pub(crate) fn user_defined(
		&mut self,
		name: &str,
		args: Vec<Expr>,
	) -> Result<Vec<CommandOutput>, egglog::Error> {
		let outputs = self
			.egraph
			.run_program(vec![GenericCommand::UserDefined(span!(), name.to_string(), args)])?;

		Ok(outputs)
	}

}