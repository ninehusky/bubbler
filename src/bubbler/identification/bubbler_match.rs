//! This file defines the invariant finding mechanism for Bubbler.
//! I'll do this later.

// use crate::{
//     bubbler::{backend::EgglogBackend, schedule::Identification, InferredFacts},
//     colors::{Condition, Implication},
//     language::Language,
// };

// pub struct InvariantFinder<L: Language> {
//     backend: EgglogBackend<L>,
// }

// impl<L: Language> Identification<L> for InvariantFinder<L> {
//     fn identify(&self, backend: &mut EgglogBackend<L>) -> Result<InferredFacts<L>, String> {
//         // 1. Find all pvecs which are just true.
//         let mut candidates = vec![];
//         let map = backend.get_pvec_map();
//         for (pvec, terms) in map.iter() {
//             if pvec.iter().all(|&b| b) {
//                 for term in terms {
//                     let implication = Implication {
//                         from: Condition::Term(term.term),
//                         to: term.clone(),
//                     };
//                     candidates.push(implication);
//                 }
//             }
//         }
//     }
// }
