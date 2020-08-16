use postgrustql::*;

fn main() {
    let mut db = backend_memory::MemoryBackend::new();
    db.eval_query(
        "CREATE TABLE people (id INT, name TEXT);"
            .to_owned()
            .as_str(),
    )
    .unwrap();
    for i in 0..1000000 {
        let parsed =
            parser::parse(&format!("INSERT INTO people VALUES ({}, 'Baam{}');", i, i)).unwrap();
        let statement;
        match &parsed.statements[0] {
            ast::Statement::InsertStatement(insert_statement) => {
                statement = insert_statement;
            }
            _ => {
                eprintln!("Derp..");
                return;
            }
        }
        db.insert(statement.clone()).unwrap();
    }
    for _ in 0..100 {
        db.eval_query(
            format!("SELECT * FROM people WHERE id = 999999;")
                .to_owned()
                .as_str(),
        )
        .unwrap();
    }
}
