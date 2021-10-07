use criterion::black_box;
use instant::Instant;
use sqlo2::lexer;

fn lex_benchmark() {
    let lexer = lexer::Lexer::new();
    lexer.lex(black_box("
        CREATE TABLE people (id INT PRIMARY KEY, name TEXT); INSERT INTO people VALUES (1, 'Baam'); INSERT INTO people VALUES (2, 'Rachel'); INSERT INTO people VALUES (3, 'Rak WraithKaiser'); INSERT INTO people VALUES (4, 'Khun Aguero Agnes');
        SELECT id, name FROM people;
        SELECT id, name FROM people where id != 3;
        SELECT id, name FROM people where name = 'Rachel';".to_owned().as_str())).unwrap();
}

fn lex_select_benchmark() {
    let lexer = lexer::Lexer::new();
    lexer.lex(black_box("
        SELECT id, name FROM people;
        SELECT id, name FROM people where id != 3;
        SELECT id, name FROM people where name = 'Rachel';
        SELECT id, age, role, job, position, country, address from people WHERE country = 'GR' AND age > 17
        SELECT id, age, role, job, position, country, address from people WHERE country = 'GR' AND age > 17 INNER LEFT JOIN ON jobs".to_owned().as_str())).unwrap();
}

fn main() {
    let before = Instant::now();
    for _ in 0..100000 {
        lex_benchmark();
    }
    for _ in 0..100000 {
        lex_select_benchmark();
    }
    let after = before.elapsed();
    let avg = after / 200000;
    println!("Average time: {:.2?}", avg);
}
