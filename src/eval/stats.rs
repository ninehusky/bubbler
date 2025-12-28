use std::collections::BTreeMap;
use std::time::Duration;

#[derive(Default)]
pub struct EvalStats {
    pub times: BTreeMap<String, Duration>,
}
