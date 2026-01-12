use crate::{
    bubbler::{Bubbler, BubblerConfig, Implication},
    language::{Language, Rewrite},
};

const ITERATION_LIMIT: usize = 5;

pub fn can_derive<L: Language>(
    rewrites: &[Rewrite<L>],
    implications: &[Implication<L>],
    target: &Rewrite<L>,
) -> bool {
    // 1. Create a temporary backend populated with the existing rewrites and implications.
    let mut bubbler: Bubbler<L> = Bubbler::new(BubblerConfig::new(vec![], vec![]));

    for rw in rewrites {
        bubbler
            .register_rewrite(rw)
            .expect("Failed to add rewrite to bubbler.");
    }

    for imp in implications {
        bubbler
            .register_implication(imp)
            .expect("Failed to add implication to bubbler.");
    }

    let mut backend = bubbler.new_backend();

    // 2. Add the target rewrite's LHS, RHS, and condition (if any) to the backend.
    backend.add_term(target.lhs_concrete(), false).unwrap();
    backend.add_term(target.rhs_concrete(), false).unwrap();

    if let Some(cond) = target.cond_concrete() {
        backend.add_predicate(cond, false).unwrap();
    }

    let mut is_derived = false;

    // 3. Run the inference engine.
    for i in 0..ITERATION_LIMIT {
        // NOTE: this weird schedule is a bit ad-hoc, maybe writing it as a egglog Schedule
        // or somewhere else would be better.
        // Basically, we need to run rewrites _first_ to get new predicate ammo, and then
        // implications to propagate those predicates, and then rewrites again to use
        // the newly derived predicates...
        backend.run_rewrites().unwrap();
        backend.run_implications().unwrap();
        backend.run_rewrites().unwrap();

        is_derived = match &target {
            Rewrite::Conditional { .. } => backend
                .is_conditionally_equal(
                    &target.cond_concrete().unwrap(),
                    &target.lhs_concrete(),
                    &target.rhs_concrete(),
                )
                .unwrap(),
            Rewrite::Unconditional { .. } => backend
                .is_equal(&target.lhs_concrete(), &target.rhs_concrete())
                .unwrap(),
        };

        if is_derived {
            println!("Derived after {} iterations.", i + 1);
            break;
        }
    }

    is_derived
}
