use sqlo2::*;

fn run_load() {
    let mut db = backend_memory::MemoryBackend::new();
    db.eval_query(
        "CREATE TABLE people (id INT, name TEXT);"
            .to_owned()
            .as_str(),
    )
    .unwrap();
    for i in 0..1000000 {
        db.eval_query(&format!("INSERT INTO people VALUES ({i}, 'Baam{i}');"))
            .unwrap();
    }
    for _ in 0..100 {
        db.eval_query("SELECT * FROM people WHERE id = 999999;")
            .unwrap();
    }
}

fn main() {
    run_load();
}
