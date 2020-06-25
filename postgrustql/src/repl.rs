use prettytable;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use super::backend::{Cell, EvalResult};
use super::backend_memory::*;

use std::io::{stdout, Write};
use std::time::Duration;

pub fn run_repl() {
    let mut mb = MemoryBackend::new();
    let mut rl = Editor::<()>::new();

    loop {
        // print!("PostgRustQL #: ");
        match stdout().flush() {
            Ok(_) => {}
            Err(err) => {
                eprintln!("An error occured: {}", err);
            }
        }

        let readline = rl.readline("PostgRustQL #: ");
        match readline {
            Ok(_) => {}
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
        let input = readline.unwrap();

        let text = input.trim().replace("\n", "");

        let mut total_time: Duration = Duration::from_millis(0);
        let mut multiple_results = false;

        match mb.eval_query(&text) {
            Ok(eval_results) => {
                if eval_results.len() > 1 {
                    multiple_results = true;
                }
                for eval_result in eval_results {
                    match eval_result {
                        EvalResult::Select { results, time } => {
                            let mut titles = vec![];
                            let mut table = prettytable::Table::new();
                            for col in &results.columns {
                                let title = format!("{}({:?})", col.name, col.col_type);
                                let title_cell = prettytable::Cell::new(&title)
                                    .with_style(prettytable::Attr::Bold)
                                    .with_style(prettytable::Attr::ForegroundColor(
                                        prettytable::color::GREEN,
                                    ));
                                titles.push(title_cell);
                            }

                            table.set_titles(prettytable::Row::new(titles));

                            for result in &results.rows {
                                let mut table_row = vec![];
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
                                        super::backend::ColumnType::BoolType => {
                                            s = format!("{}", cell.as_bool().unwrap());
                                        }
                                    }
                                    table_row.push(prettytable::Cell::new(&s));
                                }
                                table.add_row(prettytable::Row::new(table_row));
                            }
                            table.set_format(
                                *prettytable::format::consts::FORMAT_NO_LINESEP_WITH_TITLE,
                            );
                            if results.rows.len() > 0 {
                                table.printstd();
                            }
                            println!("({} Results)", results.rows.len());

                            println!("Ok!");
                            if multiple_results {
                                total_time += time;
                            }
                            println!("Elapsed time : {:.2?}", time);
                        }
                        EvalResult::CreateTable { success: _, time } => {
                            println!("Ok!");
                            if multiple_results {
                                total_time += time;
                            }
                            println!("Elapsed time : {:.2?}", time);
                        }
                        EvalResult::Insert { success: _, time } => {
                            println!("Ok!");
                            if multiple_results {
                                total_time += time;
                            }
                            println!("Elapsed time : {:.2?}", time);
                        }
                    }
                }

                if multiple_results {
                    println!("Total time : {:.2?}", total_time);
                }
            }

            Err(err) => {
                eprintln!("{}", err);
                continue;
            }
        }
    }
}
