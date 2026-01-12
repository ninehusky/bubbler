use serde::Serialize;

use crate::{
    bubbler::Implication,
    language::{Language, Rewrite},
};

use super::{derivability::can_derive, stats::EvalStats};

#[derive(Debug, Clone, Serialize)]
pub struct NightlyReport {
    pub summary: Summary,
    pub inferred: InferredSection,
    pub subsumptions: SubsumptionSection,
    pub stats: StatsSection,
}

impl NightlyReport {
    pub fn from_run<L: Language>(
        inferred_rewrites: &[Rewrite<L>],
        inferred_implications: &[Implication<L>],
        handwritten_rules: &[Rewrite<L>],
        stats: &EvalStats,
    ) -> Self {
        // --- Subsumption records ---
        let mut records = Vec::new();
        let mut subsumed_count = 0;

        for rule in handwritten_rules {
            let subsumed = can_derive(inferred_rewrites, inferred_implications, rule);
            if subsumed {
                subsumed_count += 1;
            }

            records.push(SubsumptionRecord {
                rule: rule.to_string(),
                subsumed,
            });
        }

        // --- Stats (stable ordering) ---
        let mut timings: Vec<(String, u128)> = stats
            .times
            .iter()
            .map(|(k, v)| (k.clone(), v.as_millis()))
            .collect();

        timings.sort_by(|a, b| a.0.cmp(&b.0));

        NightlyReport {
            summary: Summary {
                rules_checked: handwritten_rules.len(),
                rules_subsumed: subsumed_count,
            },
            inferred: InferredSection {
                rewrites: inferred_rewrites.iter().map(|r| r.to_string()).collect(),
                implications: inferred_implications
                    .iter()
                    .map(|i| i.to_string())
                    .collect(),
            },
            subsumptions: SubsumptionSection { records },
            stats: StatsSection {
                timings_ms: timings,
            },
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Summary {
    pub rules_checked: usize,
    pub rules_subsumed: usize,
}

#[derive(Debug, Clone, Serialize)]
pub struct InferredSection {
    pub rewrites: Vec<String>,
    pub implications: Vec<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct SubsumptionSection {
    pub records: Vec<SubsumptionRecord>,
}

#[derive(Debug, Clone, Serialize)]
pub struct SubsumptionRecord {
    pub rule: String,
    pub subsumed: bool,
}

#[derive(Debug, Clone, Serialize)]
pub struct StatsSection {
    pub timings_ms: Vec<(String, u128)>,
}
