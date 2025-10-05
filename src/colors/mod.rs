use crate::language::sexp::Sexp;
use egglog::{
    ast::{Command, GenericCommand, GenericNCommand},
    UserDefinedCommand,
};

/// A Color represents the view of the e-graph under some predicate. Two important things:
///
/// 1. Colors are _layers_ over the e-graph. A single color represents some assumption, under which
///    the e-graph may have more equivalences than the base (black) e-graph.
///    For example: consider the expression `x / x`. In the black e-graph, this expression
///    is not equivalent to `1`, because if `x == 0`, then `x / x` is not `1`.
///    But if we have additional assumptions, for example `x != 0`, then in the
///    color representing `x != 0`, `x / x` is equivalent to `1`.
///
/// 2. Colors represent predicates which can have relationships with each other. These relationships
///    often take the form of _implications_, which form an implication lattice between colors.
///    For example, let's consider `p`, `q` as two predicates. We can form _conjunctions_ (∧)
///    and _disjunctions_ (∨) of these predicates.
///    From discrete math, we should remember the following relationships:
///     (p ∧ q) → p
///     (p ∧ q) → q
///     p → (p ∨ q)
///     q → (p ∨ q)
///
///    We can model these relationships in a geometric way as a diamond lattice:
///    ```text
///              T
///              |
///              |
///            p ∧ q
///            /   \
///           /     \
///          p       q
///           \     /
///            \   /
///            p ∨ q
///              |
///              |
///              F
///   ```
///
///   Assume edges point downwards. Equalities flow downwards, but not upwards.
///   For example, if we know `x = y` in the color `T`, then it must also hold for every layer below it.
///   But if we know `x = y` in the color `p`, it does not necessarily hold in `T`.
///
///
/// With a `Color`, in Bubbler we can start to support the colored e-graph operations
/// presented in the Colored E-Graphs paper:
/// - `colored_find` should take a color and a term, and return the canonical term for that color:
///    in a color `c` representing `x != 0`, `colored_find(c, x / x)` should return `1`.
/// - `colored_union` should take a color and two terms, and merge the two terms in that color.
///    the "action" of the rule `if x != 0 then x / x ~> 1` should perform
///   `colored_union(c, x / x, 1)`.

pub struct Color {
    pub label: String,
    pub children: Vec<Color>,
    // canonical terms for each black e-class that this color considers equal.
    terms: Vec<Sexp>,
}

pub struct ColoredFind {}

impl UserDefinedCommand for ColoredFind {
    fn update(
        &self,
        egraph: &mut egglog::EGraph,
        args: &[egglog::ast::Expr],
    ) -> Result<Option<egglog::CommandOutput>, egglog::Error> {
        todo!()
    }
}

// impl ColoredFind {
//     fn name() -> &'static str {}
// }
