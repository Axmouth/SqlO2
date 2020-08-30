use postgrustql;

use postgrustql::backend::{Cell, EvalResult};
use postgrustql::backend_memory::*;

use rustyline::{error::ReadlineError, Editor};
use std::io::{stdout, Write};
use std::time::Duration;
use tokio;

fn main() {
    let mut mb = MemoryBackend::new();
    let mut rl = Editor::<()>::new();

    loop {
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

        println!("{}", repl_eval(&mut mb, text));
    }
}
pub fn repl_eval(mb: &mut MemoryBackend, cmd: String) -> String {
    let mut output_text = String::from("");

    let mut total_time: Duration = Duration::from_millis(0);
    let mut multiple_results = false;

    match mb.eval_query(&cmd) {
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
                                    postgrustql::backend::ColumnType::IntType => {
                                        s = format!("{}", cell.as_int().unwrap());
                                    }
                                    postgrustql::backend::ColumnType::TextType => {
                                        s = format!("{}", cell.as_text().unwrap());
                                    }
                                    postgrustql::backend::ColumnType::BoolType => {
                                        s = format!("{}", cell.as_bool().unwrap());
                                    }
                                }
                                table_row.push(prettytable::Cell::new(&s));
                            }
                            table.add_row(prettytable::Row::new(table_row));
                        }
                        table
                            .set_format(*prettytable::format::consts::FORMAT_NO_LINESEP_WITH_TITLE);
                        if results.rows.len() > 0 {
                            // table.printstd();

                            output_text.push_str(table.to_string().as_str());
                        }
                        output_text.push_str(format!("({} Results)", results.rows.len()).as_str());

                        output_text.push_str("Ok!");
                        if multiple_results {
                            total_time += time;
                        }
                        output_text.push_str(format!("Elapsed time : {:.2?}", time).as_str());
                    }
                    EvalResult::CreateTable { success: _, time } => {
                        output_text.push_str("Ok!");
                        if multiple_results {
                            total_time += time;
                        }
                        output_text.push_str(format!("Elapsed time : {:.2?}", time).as_str());
                    }
                    EvalResult::Insert { success: _, time } => {
                        output_text.push_str("Ok!");
                        if multiple_results {
                            total_time += time;
                        }
                        output_text.push_str(format!("Elapsed time : {:.2?}", time).as_str());
                    }
                }
            }

            if multiple_results {
                output_text.push_str(format!("Total time : {:.2?}", total_time).as_str());
            }
        }

        Err(err) => {
            return err;
            // output_text.push_str(err.as_str());
            // continue;
        }
    }

    output_text
}
