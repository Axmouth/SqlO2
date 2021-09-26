use criterion::{black_box, criterion_group, criterion_main, Benchmark, Criterion};

use instant::Instant;
use sqlo2::*;

fn lex_benchmark(c: &mut Criterion) {
    let lexer = lexer::Lexer::new();
    c.bench_function("lex", |b| b.iter(|| lexer.lex(black_box("
        CREATE TABLE people (id INT PRIMARY KEY, name TEXT); INSERT INTO people VALUES (1, 'Baam'); INSERT INTO people VALUES (2, 'Rachel'); INSERT INTO people VALUES (3, 'Rak WraithKaiser'); INSERT INTO people VALUES (4, 'Khun Aguero Agnes');
        SELECT id, name FROM people;
        SELECT id, name FROM people where id != 3;
        SELECT id, name FROM people where name = 'Rachel';"))));
}

fn lex_select_benchmark(c: &mut Criterion) {
    let lexer = lexer::Lexer::new();
    c.bench_function("lex selects", |b| b.iter(|| lexer.lex(black_box("
        SELECT id, name FROM people;
        SELECT id, name FROM people where id != 3;
        SELECT id, name FROM people where name = 'Rachel';
        SELECT id, age, role, job, position, country, address from people WHERE country = 'GR' AND age > 17
        SELECT id, age, role, job, position, country, address from people WHERE country = 'GR' AND age > 17 INNER LEFT JOIN ON jobs"))));
}

fn parse_benchmark(c: &mut Criterion) {
    let parser = parser::Parser::new();

    c.bench_function("parse", |b| b.iter(|| parser.parse(black_box("
    CREATE TABLE people (id INT PRIMARY KEY, name TEXT); INSERT INTO people VALUES (1, 'Baam'); INSERT INTO people VALUES (2, 'Rachel'); INSERT INTO people VALUES (3, 'Rak WraithKaiser'); INSERT INTO people VALUES (4, 'Khun Aguero Agnes');
    SELECT id, name FROM people;
    SELECT id, name FROM people where id != 3;
    SELECT id, name FROM people where name = 'Rachel';".to_owned().as_str()))));
}

fn parse_select_benchmark(c: &mut Criterion) {
    let parser = parser::Parser::new();

    c.bench_function("parse", |b| b.iter(|| parser.parse(
        black_box("SELECT id, age, role, job, position, country, address from people WHERE country = 'GR' AND age > 17 INNER LEFT JOIN ON jobs".to_owned().as_str()))));
}

fn create_benchmark(c: &mut Criterion) {
    // Bench here
    c.bench(
        "create",
        Benchmark::new("create in", move |b| {
            b.iter(|| {
                for _ in 0..1 {
                    let mut db = backend_memory::MemoryBackend::new();
                    db.eval_query("CREATE TABLE people (id INT PRIMARY KEY, name TEXT);")
                        .unwrap();
                }
            })
        }),
    );
}

fn insert_benchmark(c: &mut Criterion) {
    let mut db = backend_memory::MemoryBackend::new();
    db.eval_query("CREATE TABLE people (id INT, name TEXT);")
        .unwrap();

    // Bench here
    c.bench(
        "insert",
        Benchmark::new("insert in", move |b| {
            b.iter(|| {
                for _ in 0..10 {
                    db.eval_query(black_box("INSERT INTO people VALUES (1, 'Baam');"))
                        .unwrap();
                }
            })
        }),
    );
}

fn single_insert_benchmark(c: &mut Criterion) {
    let mut db = backend_memory::MemoryBackend::new();
    db.eval_query("CREATE TABLE people (id INT, name TEXT);")
        .unwrap();

    // Bench here
    c.bench(
        "single_insert",
        Benchmark::new("insert in", move |b| {
            b.iter(|| {
                for _ in 0..1 {
                    db.eval_query(black_box("INSERT INTO people VALUES (1, 'Baam');"))
                        .unwrap();
                }
            })
        }),
    );
}

fn select_benchmark(c: &mut Criterion) {
    let mut db = backend_memory::MemoryBackend::new();
    db.eval_query(
        "CREATE TABLE people (id INT PRIMARY KEY, name TEXT);"
            .to_owned()
            .as_str(),
    )
    .unwrap();
    for i in 0..10000 {
        db.eval_query(&format!("INSERT INTO people VALUES ({}, 'Baam{}');", i, i))
            .unwrap();
    }
    let temp = db.eval_query("SELECT * FROM people;").unwrap();
    match &temp[0] {
        backend::EvalResult::Select {
            results: _select_results,
            time: _time,
        } => {}
        _ => {
            eprintln!("Derp..");
            return;
        }
    }

    // Bench here
    c.bench(
        "select",
        Benchmark::new("select_from_10000", move |b| {
            b.iter(|| {
                db.eval_query(black_box("SELECT * FROM people;")).unwrap();
            })
        }),
    );
}

pub fn million_row_benchmark(_c: &mut Criterion) {
    println!("Million Row Benchmark");

    // Insert benchmark
    let before = Instant::now();
    let mut db = backend_memory::MemoryBackend::new();
    db.eval_query("CREATE TABLE people (id INT, name TEXT);")
        .unwrap();
    for i in 0..1000000 {
        db.eval_query(
            black_box(format!("INSERT INTO people VALUES ({}, 'Baam{}');", i, i)).as_str(),
        )
        .unwrap();
    }
    println!(
        "Elapsed time to insert 1000000 rows: {:.2?}",
        before.elapsed()
    );

    // Select benchmark 1
    let before = Instant::now();
    for i in 0..100 {
        let result = db.eval_query(black_box(
            format!("SELECT * FROM people WHERE id = {};", i * 10000).as_str(),
        ));
        if result.is_err() {
            println!("{}", result.err().unwrap());
        } else {
            result.unwrap();
        }
    }
    println!(
        "Elapsed time to select single rows, 100 times: {:.2?}",
        before.elapsed()
    );

    // Select benchmark 2
    let before = Instant::now();
    for _ in 0..100 {
        db.eval_query(black_box(
            format!("SELECT * FROM people WHERE id = 999999;").as_str(),
        ))
        .unwrap();
    }
    println!(
        "Elapsed time to select single last row, 100 times: {:.2?}",
        before.elapsed()
    );

    // Select benchmark 3
    let before = Instant::now();
    for _ in 0..1 {
        db.eval_query(black_box(
            format!("SELECT * FROM people WHERE id = 999999;").as_str(),
        ))
        .unwrap();
    }
    println!(
        "Elapsed time to select single last row, 1 time: {:.2?}",
        before.elapsed()
    );

    // Select benchmark 4
    let before = Instant::now();
    for _ in 0..1 {
        db.eval_query(black_box("SELECT * FROM people;")).unwrap();
    }
    println!(
        "Elapsed time to select 1000000 rows, 1 time: {:.2?}",
        before.elapsed()
    );

    // Select benchmark 4
    let before = Instant::now();
    for _ in 0..100 {
        db.eval_query(black_box("SELECT * FROM people;")).unwrap();
    }
    println!(
        "Elapsed time to select 1000000 rows, 100 times: {:.2?}",
        before.elapsed()
    );

    db.eval_query(black_box(
        "Create unique index \"fdfddf\" on \"people\" (id);",
    ))
    .unwrap();

    // Select benchmark 5
    let before = Instant::now();
    for _ in 0..1 {
        db.eval_query(black_box(
            format!("SELECT * FROM people WHERE id = 999999;").as_str(),
        ))
        .unwrap();
    }
    println!(
        "Elapsed time to select single last row, 1 time, with an index: {:.2?}",
        before.elapsed()
    );
}

criterion_group!(
    benches,
    lex_benchmark,
    lex_select_benchmark,
    parse_benchmark,
    // parse_select_benchmark,
    create_benchmark,
    single_insert_benchmark,
    insert_benchmark,
    select_benchmark,
    million_row_benchmark,
);
criterion_main!(benches);
