use sqlo2::{self};

use sqlo2::backend::EvalResult;
use sqlo2::backend_memory::*;

extern crate rustc_version_runtime;
use rustc_version_runtime::version;

use rustyline::{error::ReadlineError, Editor};
use std::io::{stdout, Write};
use std::time::Duration;

use sysinfo::{get_current_pid, ProcessExt, System, SystemExt};

fn main() {
    const VERSION: &str = env!("CARGO_PKG_VERSION");

    let mut mb = MemoryBackend::new();
    let mut rl = Editor::<()>::new();

    if rl.load_history("history.txt").is_ok() {}

    let mut system = System::new();
    system.refresh_all();
    let rust_info = version();

    println!();
    println!("SqlO2 {} Repl", VERSION);
    println!();

    // Display system information:
    println!(
        "System:            {} {}",
        system.name().unwrap_or_else(|| "Unknown".to_string()),
        system.os_version().unwrap_or_else(|| "unknown".to_string())
    );
    println!(
        "Kernel             {}",
        system
            .kernel_version()
            .unwrap_or_else(|| "Unknown".to_string())
    );
    println!(
        "Rust version:      {}.{}.{}",
        rust_info.major, rust_info.minor, rust_info.patch
    );
    if let Ok(current_pid) = get_current_pid() {
        let current_process_opt = system.process(current_pid);
        if let Some(current_process) = current_process_opt {
            println!("Memory usage(kb):  {}", current_process.memory());
        }
    }

    println!();

    loop {
        match stdout().flush() {
            Ok(_) => {}
            Err(err) => {
                eprintln!("An error occured: {}", err);
            }
        }

        let readline = rl.readline("SqlO2 #: ");
        let input = match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                line
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                match rl.save_history("history.txt") {
                    Ok(_) => {}
                    Err(err) => {
                        eprintln!("An error occured: {}", err);
                    }
                }
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                match rl.save_history("history.txt") {
                    Ok(_) => {}
                    Err(err) => {
                        eprintln!("An error occured: {}", err);
                    }
                }
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                match rl.save_history("history.txt") {
                    Ok(_) => {}
                    Err(err) => {
                        eprintln!("An error occured: {}", err);
                    }
                }
                break;
            }
        };

        let cmd = input.trim_end().replace("\n", "");
        match cmd.as_str() {
            "quit" | "exit" | "\\q" => {
                break;
            }
            _ => {
                println!("{}", repl_eval(&mut mb, cmd));
            }
        }
    }
    rl.save_history("history.txt").unwrap();
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
                        let mut titles = Vec::with_capacity(10);
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
                            let mut table_row = Vec::with_capacity(10);
                            for cell in result.iter() {
                                let s = cell.to_string();
                                table_row.push(prettytable::Cell::new(&s));
                            }
                            table.add_row(prettytable::Row::new(table_row));
                        }
                        table
                            .set_format(*prettytable::format::consts::FORMAT_NO_LINESEP_WITH_TITLE);
                        if !results.rows.is_empty() {
                            output_text.push_str(table.to_string().as_str());
                        }
                        output_text
                            .push_str(format!("({} Results)\n", results.rows.len()).as_str());

                        output_text.push_str("Ok!\n");
                        if multiple_results {
                            total_time += time;
                        }
                        output_text.push_str(format!("Elapsed time : {:.2?}\n", time).as_str());
                    }
                    EvalResult::CreateTable { success: _, time } => {
                        output_text.push_str("Ok!\n");
                        if multiple_results {
                            total_time += time;
                        }
                        output_text.push_str(format!("Elapsed time : {:.2?}\n", time).as_str());
                    }
                    EvalResult::Insert { success: _, time } => {
                        output_text.push_str("Ok!\n");
                        if multiple_results {
                            total_time += time;
                        }
                        output_text.push_str(format!("Elapsed time : {:.2?}\n", time).as_str());
                    }
                    EvalResult::DropTable { success: _, time } => {
                        output_text.push_str("Ok!\n");
                        if multiple_results {
                            total_time += time;
                        }
                        output_text.push_str(format!("Elapsed time : {:.2?}\n", time).as_str());
                    }
                }
            }

            if multiple_results {
                output_text.push_str(format!("Total time : {:.2?}", total_time).as_str());
            }
        }

        Err(err) => {
            return err;
        }
    }

    output_text
}
