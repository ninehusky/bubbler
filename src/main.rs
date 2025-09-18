use bubbler::language::{BubbleLang, Language};

fn main() {
    let egglog_name_defn = BubbleLang::to_egglog_src();
    println!("Egglog definition:\n{}", egglog_name_defn);
}
