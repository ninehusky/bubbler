#[macro_export]
macro_rules! record {
    ($stats:expr, $label:expr, $expr:expr) => {{
        let __start = std::time::Instant::now();
        let __result = $expr;
        let __elapsed = __start.elapsed();
        $stats.times.insert($label.to_string(), __elapsed);
        __result
    }};
}
