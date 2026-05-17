//! This file contains the matching strategies used in Chompy's implementation.

use crate::{
    bubbler::{Implication, InferredFacts, backend::EgglogBackend, schedule::Identification},
    language::{CVec, Language, OpTrait, PVec, Term, rewrite::Rewrite},
};

use super::IdentificationConfig;

/// CvecMatch identifies rewrites by looking for terms which are
/// observationally equivalent in the e-graph. More or less a
/// 1:1 mapping from the Enumo repo's implementation.
pub struct CvecMatch<L: Language> {
    cfg: IdentificationConfig,
    _marker: std::marker::PhantomData<L>,
}

impl<L: Language> CvecMatch<L> {
    pub fn new(cfg: IdentificationConfig) -> CvecMatch<L> {
        Self {
            cfg,
            _marker: std::marker::PhantomData::<L>,
        }
    }
}

impl<L: Language> Identification<L> for CvecMatch<L> {
    fn identify(&self, backend: &mut EgglogBackend<L>) -> Result<InferredFacts<L>, String> {
        if self.cfg.mode != super::IdentificationMode::Rewrites {
            return Err("CvecMatch only supports rewrite identification.".into());
        }
        let cvec_map = backend.get_cvec_map();

        // Naively, a pairing of any two terms with identical
        // cvecs is a rewrite.
        let mut candidates = vec![];

        for (_cvec, terms) in cvec_map {
            if terms.len() < 2 {
                continue;
            }

            for i in 0..terms.len() {
                for j in (i + 1)..terms.len() {
                    let lhs = terms[i].clone();
                    let rhs = terms[j].clone();

                    // Forward direction.
                    if let Ok(rw) = Rewrite::new(None, lhs.clone(), rhs.clone()) {
                        if candidates.contains(&rw) {
                            continue;
                        }
                        candidates.push(rw.clone());
                    }

                    // Backward direction.
                    if let Ok(rw) = Rewrite::new(None, rhs, lhs) {
                        if candidates.contains(&rw) {
                            continue;
                        }
                        candidates.push(rw.clone());
                    }
                }
            }
        }

        Ok(InferredFacts::Rewrites(candidates))
    }
}

/// AxiomMatch finds predicates that are unconditionally true over the evaluation
/// environment — those whose PVec is all-true. These are shape-based axioms
/// like `abs(x) >= 0` that require no antecedent predicate.
pub struct AxiomMatch<L: Language> {
    _marker: std::marker::PhantomData<L>,
}

impl<L: Language> AxiomMatch<L> {
    pub fn new() -> Self {
        Self {
            _marker: std::marker::PhantomData,
        }
    }
}

impl<L: Language> Identification<L> for AxiomMatch<L> {
    fn identify(&self, backend: &mut EgglogBackend<L>) -> Result<InferredFacts<L>, String> {
        Ok(InferredFacts::Axioms(backend.get_axioms()))
    }
}

/// PvecMatch finds likely candidates for _implications_ through pvec matching.
/// Formally, two predicates with pvecs `p, q` are a match if
/// `\forall i. p[i] => q[i]`. Like in the real world, implications in
/// the forward directions do not guarantee implications in the reverse direction.
#[allow(dead_code)]
pub struct PvecMatch<L: Language> {
    cfg: IdentificationConfig,
    _marker: std::marker::PhantomData<L>,
}

impl<L: Language> PvecMatch<L> {
    pub fn new(cfg: IdentificationConfig) -> PvecMatch<L> {
        Self {
            cfg,
            _marker: std::marker::PhantomData::<L>,
        }
    }

    fn is_bad_pvec(pvec: PVec) -> bool {
        // A pvec which is all false is useless.
        if pvec.iter().all(|b| !*b) {
            return true;
        }

        // A pvec which is all true is useless.
        if pvec.iter().all(|b| *b) {
            return true;
        }
        false
    }
}

impl<L: Language> Identification<L> for PvecMatch<L> {
    fn identify(&self, backend: &mut EgglogBackend<L>) -> Result<InferredFacts<L>, String> {
        let pvec_map = backend.get_pvec_map();

        let pvecs = pvec_map.keys().cloned();

        let mut matching_pvecs: Vec<(PVec, PVec)> = vec![];

        for pvec1 in pvecs.clone() {
            if Self::is_bad_pvec(pvec1.clone()) {
                continue;
            }
            for pvec2 in pvecs.clone() {
                if Self::is_bad_pvec(pvec2.clone()) {
                    continue;
                }
                if pvec1 == pvec2 {
                    continue;
                }

                let mut implies = true;
                for (b1, b2) in pvec1.iter().zip(pvec2.iter()) {
                    if *b1 && !*b2 {
                        implies = false;
                        break;
                    }
                }

                if implies {
                    matching_pvecs.push((pvec1.clone(), pvec2.clone()));
                }
            }
        }

        // Build a reverse map from predicate terms to their PVecs for the
        // extended Or-weakening filter (step 2).
        let term_to_pvec: std::collections::HashMap<&crate::language::PredicateTerm<L>, &PVec> =
            pvec_map
                .iter()
                .flat_map(|(pvec, terms)| terms.iter().map(move |t| (t, pvec)))
                .collect();

        let mut candidates = vec![];
        for (from, to) in matching_pvecs {
            let from_terms = pvec_map.get(&from).unwrap();
            let to_terms = pvec_map.get(&to).unwrap();

            for from_term in from_terms {
                for to_term in to_terms {
                    // Skip trivially true implications:
                    //
                    // Syntactic: And(P,Q)→P/Q and P→Or(P,Q)/Q→Or(P,Q).
                    //
                    // PVec-lattice: P → Or(Q, R) when pvec(P) ⊆ pvec(Q) or
                    // pvec(P) ⊆ pvec(R). These are derivable through the lattice
                    // even when P is not literally a component of Or.
                    let syntactic_trivial = match (&from_term.term, &to_term.term) {
                        (Term::Call(f, args), _) if f.is_conjunction() => {
                            args.iter().any(|a| a == &to_term.term)
                        }
                        (_, Term::Call(t, args)) if t.is_disjunction() => {
                            args.iter().any(|a| a == &from_term.term)
                        }
                        _ => false,
                    };

                    let pvec_trivial = if let Term::Call(t, or_args) = &to_term.term {
                        if t.is_disjunction() && or_args.len() == 2 {
                            let p = crate::language::PredicateTerm::from_term(or_args[0].clone());
                            let q = crate::language::PredicateTerm::from_term(or_args[1].clone());
                            let pv_p = term_to_pvec.get(&p);
                            let pv_q = term_to_pvec.get(&q);
                            // P → Or(Q,R) is trivial if pvec(P) ⊆ pvec(Q) or pvec(P) ⊆ pvec(R)
                            pv_p.map_or(false, |pq| {
                                from.iter().zip(pq.iter()).all(|(a, b)| !*a || *b)
                            }) || pv_q.map_or(false, |pq| {
                                from.iter().zip(pq.iter()).all(|(a, b)| !*a || *b)
                            })
                        } else {
                            false
                        }
                    } else {
                        false
                    };

                    // Skip And(P,Q)→R when all of R's pattern holes are covered
                    // by P alone or Q alone — dominated by the single-antecedent P→R
                    // or Q→R, which the minimizer would already have or will find.
                    let and_dominated = if let Term::Call(f, and_args) = &from_term.term {
                        if f.is_conjunction() && and_args.len() == 2 {
                            let r_holes = to_term.term.holes();
                            let p_holes = and_args[0].holes();
                            let q_holes = and_args[1].holes();
                            let covered_by_p = r_holes.iter().all(|h| p_holes.contains(h));
                            let covered_by_q = r_holes.iter().all(|h| q_holes.contains(h));
                            covered_by_p || covered_by_q
                        } else {
                            false
                        }
                    } else {
                        false
                    };

                    if syntactic_trivial || pvec_trivial || and_dominated {
                        continue;
                    }

                    // Skip implications where the consequent introduces variables
                    // not bound by the antecedent — those are structurally
                    // unregisterable in egglog (need a universe relation).
                    if let Ok(implication) =
                        Implication::new(from_term.clone().into(), to_term.clone())
                    {
                        candidates.push(implication);
                    }
                }
            }
        }

        eprintln!("[pvec_match] {} implication candidates", candidates.len());
        Ok(InferredFacts::Implications(candidates))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        bubbler::{
            Bubbler, BubblerConfig,
            enumeration::{BasicEnumerate, EnumerationConfig, EnumerationMode},
            identification::{IdentificationConfig, IdentificationMode},
            schedule::{Enumeration, Identification},
        },
        test_langs::llvm::LLVMLang,
    };

    use super::*;
    #[test]
    fn cvec_match_finds_add_mul_rules() {
        let cfg: BubblerConfig<LLVMLang> =
            BubblerConfig::new(vec!["x".into(), "y".into()], vec![1, 2]);
        let bubbler = Bubbler::new(cfg);

        let enumeration_cfg: EnumerationConfig = EnumerationConfig {
            mode: EnumerationMode::Terms,
            evaluate: true,
        };

        let enumerate_act = BasicEnumerate::<LLVMLang>::new(enumeration_cfg);

        let mut backend = bubbler.new_backend();

        enumerate_act
            .enumerate_bubbler(
                &mut backend,
                ruler::enumo::Workload::new(vec![
                    "(Add x y)",
                    "(Mul x y)",
                    "(Add y x)",
                    "(Mul y x)",
                    "(Add 0 x)",
                    "x",
                ]),
            )
            .unwrap();

        let identification_cfg: IdentificationConfig = IdentificationConfig {
            mode: IdentificationMode::Rewrites,
            validate_now: false,
        };

        let identification_act = CvecMatch::<LLVMLang>::new(identification_cfg);
        let inferred = identification_act.identify(&mut backend).unwrap();
        let InferredFacts::Rewrites(inferred) = inferred else {
            panic!("Expected rewrites inferred.");
        };
        assert_eq!(inferred.len(), 3);
    }

    #[test]
    fn pvec_match_finds_implications() {
        let cfg: BubblerConfig<LLVMLang> =
            BubblerConfig::new(vec!["x".into(), "y".into()], vec![1, 2]);
        let bubbler = Bubbler::new(cfg);

        let enumeration_cfg: EnumerationConfig = EnumerationConfig {
            mode: EnumerationMode::Predicates,
            evaluate: true,
        };

        let mut backend: EgglogBackend<LLVMLang> = bubbler.new_backend();

        let enumerate_act = BasicEnumerate::<LLVMLang>::new(enumeration_cfg);

        enumerate_act
            .enumerate_bubbler(
                &mut backend,
                ruler::enumo::Workload::new(vec!["(Lt x y)", "(Gt x y)", "(Neq x y)"]),
            )
            .unwrap();

        let identification_cfg: IdentificationConfig = IdentificationConfig {
            mode: IdentificationMode::Implications,
            validate_now: false,
        };

        let identification_act = PvecMatch::<LLVMLang>::new(identification_cfg);
        let inferred = identification_act.identify(&mut backend).unwrap();
        let InferredFacts::Implications(inferred) = inferred else {
            panic!("Expected implications inferred.");
        };
        assert_eq!(inferred.len(), 2);
    }
}

#[derive(Debug, Clone)]
pub struct ConditionalCvecMatch<L: Language> {
    cfg: IdentificationConfig,
    _marker: std::marker::PhantomData<L>,
}

impl<L: Language> ConditionalCvecMatch<L> {
    pub fn new(cfg: IdentificationConfig) -> ConditionalCvecMatch<L> {
        Self {
            cfg,
            _marker: std::marker::PhantomData::<L>,
        }
    }

    fn pvec_implies_cvecs_equal(pvec: &PVec, cvec1: &CVec<L>, cvec2: &CVec<L>) -> bool {
        assert_eq!(pvec.len(), cvec1.len());
        assert_eq!(cvec1.len(), cvec2.len());

        for i in 0..pvec.len() {
            let p = pvec[i];
            let c1 = &cvec1[i];
            let c2 = &cvec2[i];

            // p[i] -> c[i] == c'[i]
            if p && c1 != c2 {
                return false;
            }
        }

        true
    }
}

impl<L: Language> Identification<L> for ConditionalCvecMatch<L> {
    fn identify(&self, backend: &mut EgglogBackend<L>) -> Result<InferredFacts<L>, String> {
        if self.cfg.mode != super::IdentificationMode::Rewrites {
            return Err("ConditionalCvecMatch only supports rewrite identification.".into());
        }
        let cvec_map = backend.get_cvec_map();
        let pvec_map = backend.get_pvec_map();

        let mut vector_matches: Vec<(PVec, CVec<L>, CVec<L>)> = vec![];
        for (i, cvec1) in cvec_map.keys().cloned().enumerate() {
            for cvec2 in cvec_map.keys().skip(i + 1) {
                // find the pvecs which make cvec1 = cvec2.
                for pvec in pvec_map.keys() {
                    if Self::pvec_implies_cvecs_equal(pvec, &cvec1, cvec2) {
                        vector_matches.push((pvec.clone(), cvec1.clone(), cvec2.clone()));
                    }
                }
            }
        }

        let mut candidates: Vec<Rewrite<L>> = vec![];

        // Now, materialize the matches into candidates.
        for (pvec, cvec1, cvec2) in vector_matches {
            let cond = pvec_map.get(&pvec).unwrap();
            let terms1 = cvec_map.get(&cvec1).unwrap();
            let terms2 = cvec_map.get(&cvec2).unwrap();

            for cond_term in cond {
                for term1 in terms1 {
                    for term2 in terms2 {
                        if backend.is_conditionally_equal(cond_term, term1, term2)? {
                            continue;
                        }
                        let fwds =
                            Rewrite::new(Some(cond_term.clone()), term1.clone(), term2.clone());
                        if let Ok(rw) = fwds {
                            candidates.push(rw);
                        }
                        let bwds =
                            Rewrite::new(Some(cond_term.clone()), term2.clone(), term1.clone());
                        if let Ok(rw) = bwds {
                            candidates.push(rw);
                        }
                    }
                }
            }
        }

        Ok(InferredFacts::Rewrites(candidates))
    }
}

#[cfg(test)]
pub mod cond_cvec_match_test {
    use crate::{
        bubbler::{Bubbler, BubblerConfig, identification::IdentificationMode},
        language::{PredicateTerm, Term},
        test_langs::llvm::{LLVMLang, LLVMLangOp},
    };

    use super::*;
    #[test]
    fn conditional_cvec_match_finds_conditional_rewrites() {
        let bubbler =
            Bubbler::<LLVMLang>::new(BubblerConfig::new(vec!["x".into()], vec![-1, 0, 1]));
        let mut backend = EgglogBackend::<LLVMLang>::new();
        backend.set_environment(bubbler.environment.clone());
        backend
            .register(
                &Rewrite::new(
                    Some(PredicateTerm::from_term(Term::Call(
                        LLVMLangOp::Lt,
                        vec![Term::Var("x".into()), Term::Const(0)],
                    ))),
                    Term::Call(
                        LLVMLangOp::Div,
                        vec![Term::Var("x".into()), Term::Var("x".into())],
                    ),
                    Term::Const(1),
                )
                .unwrap(),
            )
            .unwrap();

        backend
            .register_implication(&Implication {
                from: PredicateTerm::from_term(Term::Call(
                    LLVMLangOp::Lt,
                    vec![Term::Var("x".into()), Term::Var("y".into())],
                ))
                .into(),
                to: PredicateTerm::from_term(Term::Call(
                    LLVMLangOp::Neq,
                    vec![Term::Var("x".into()), Term::Var("y".into())],
                )),
            })
            .unwrap();

        let x_div_x = Term::Call(
            LLVMLangOp::Div,
            vec![Term::Var("x".into()), Term::Var("x".into())],
        );

        backend.add_term(Term::Const(1), true).unwrap();

        backend.add_term(x_div_x.clone(), true).unwrap();

        let x_gt_0 = Term::Call(LLVMLangOp::Gt, vec![Term::Var("x".into()), Term::Const(0)]);
        backend
            .add_predicate(PredicateTerm::from_term(x_gt_0.clone()), true)
            .unwrap();

        let x_lt_0 = Term::Call(LLVMLangOp::Lt, vec![Term::Var("x".into()), Term::Const(0)]);
        backend
            .add_predicate(PredicateTerm::from_term(x_lt_0.clone()), true)
            .unwrap();

        let x_neq_0 = Term::Call(LLVMLangOp::Neq, vec![Term::Var("x".into()), Term::Const(0)]);
        backend
            .add_predicate(PredicateTerm::from_term(x_neq_0.clone()), true)
            .unwrap();

        // NOTE: the order here is pretty important. If you run implications first, then
        // rewrites, then the backend doesn't discover the `cond-equal` fact needed to prune
        // away redundant rewrites.
        backend.run_rewrites().unwrap();
        backend.run_implications().unwrap();

        let identifier = ConditionalCvecMatch::<LLVMLang>::new(IdentificationConfig {
            mode: IdentificationMode::Rewrites,
            validate_now: false,
        });

        let candidates = identifier.identify(&mut backend).unwrap();

        let InferredFacts::Rewrites(candidates) = candidates else {
            panic!("Expected rewrites inferred.");
        };

        assert_eq!(candidates.len(), 2);
        assert!(
            candidates.contains(
                &Rewrite::new(
                    Some(PredicateTerm::from_term(x_gt_0)),
                    x_div_x.clone(),
                    Term::Const(1),
                )
                .unwrap()
            )
        );

        assert!(
            candidates.contains(
                &Rewrite::new(
                    Some(PredicateTerm::from_term(x_neq_0)),
                    x_div_x.clone(),
                    Term::Const(1),
                )
                .unwrap()
            )
        );
    }
}
