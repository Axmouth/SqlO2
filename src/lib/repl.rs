use super::backend_memory::*;
use super::parser::*;

use std::io::{stdin, stdout, Write};

pub fn run_repl() {
    let mut mb = MemoryBackend::new();

    loop {
        print!("PostgRustQL #: ");
        match stdout().flush() {
            Ok(_) => {}
            Err(err) => {
                eprintln!("An error occured: {}", err);
            }
        }

        let mut input = String::new();
        match stdin().read_line(&mut input) {
            Ok(_) => {}
            Err(err) => {
                eprintln!("An error occured: {}", err);
                continue;
            }
        }

        let text = input.trim().replace("\n", "");

        let ast;
        match parse(text.as_str()) {
            Ok(result) => {
                ast = result;
            }
            Err(err) => {
                eprintln!("{}", err);
                continue;
            }
        }

        for statement in ast.statements {
            match statement {
                Statement::CreateTableStatement(create_table_statement) => {
                    mb.create_table(create_table_statement).unwrap();
                    println!("Ok!");
                }
                Statement::InsertStatement(insert_statement) => {
                    mb.insert(insert_statement).unwrap();
                    println!("Ok!");
                }
                Statement::SelectStatement(select_statement) => {
                    let results = mb.select(select_statement).unwrap();

                    for col in &results.columns {
                        print!("| {}", col.name);
                    }
                    println!("|");

                    for _ in 0..20 {
                        print!("=");
                    }
                    println!();

                    for result in results.rows {
                        print!("|");

                        for i in 0..result.len() {
                            let cell = &result[i];
                            let typ = results.columns[i].col_type.clone();
                            let s;

                            match typ {
                                super::backend::ColumnType::IntType => {
                                    s = format!("{}", cell.as_int().unwrap());
                                }
                                super::backend::ColumnType::TextType => {
                                    s = format!("{}", cell.as_text().unwrap());
                                }
                            }
                            print!(" {} | ", s);
                        }

                        println!();
                    }
                    println!("Ok!");
                }
            }
        }
    }
}
