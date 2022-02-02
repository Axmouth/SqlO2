use crate::{ast::Ast, backend::EvalResult, parser::ParsingError};
use std::fmt::Write;
use test_util::{TestResultExt, TestResultType, TestStringify};

pub struct VecContainer<T>(Vec<T>);

pub trait IntoVecContainer<T> {
    fn into_vec_container(self) -> VecContainer<T>;
}

impl<T> IntoVecContainer<T> for Vec<T>
where
    T: TestStringify,
{
    fn into_vec_container(self) -> VecContainer<T> {
        VecContainer(self)
    }
}

impl TestStringify for Ast {
    fn stringify(&self) -> String {
        format!("{:#?}", self)
    }
}

impl TestStringify for ParsingError {
    fn stringify(&self) -> String {
        format!("{:#?}", self)
    }
}

impl<T> TestStringify for VecContainer<T>
where
    T: TestStringify,
{
    fn stringify(&self) -> String {
        let mut out = String::new();
        for item in self.0.iter() {
            writeln!(out, "{}", item.stringify()).ok();
        }
        out
    }
}

impl<T> TestResultExt for VecContainer<T>
where
    T: TestStringify,
{
    fn result_type(&self) -> TestResultType {
        TestResultType::Unknown
    }

    fn stringified(&self) -> String {
        self.stringify()
    }
}

impl<C: std::fmt::Display> TestStringify for EvalResult<C> {
    fn stringify(&self) -> String {
        match self {
            EvalResult::CreateTable { success, .. } => {
                if *success {
                    "CreateTable: success\n".to_string()
                } else {
                    "CreateTable: failure\n".to_string()
                }
            }
            EvalResult::Select { results, .. } => {
                let mut out = String::new();
                let mut col_lengths: Vec<usize> = results
                    .columns
                    .iter()
                    .map(|c| c.col_type.to_string().len() + c.name.len() + 2)
                    .collect();
                results.rows.iter().for_each(|r| {
                    r.iter()
                        .zip(col_lengths.iter_mut())
                        .for_each(|(col, col_len)| {
                            let len = col.to_string().len();
                            if len > *col_len {
                                *col_len = len;
                            }
                        });
                });
                let columns = results
                    .columns
                    .iter()
                    .zip(col_lengths.iter())
                    .map(|(c, col_len)| {
                        format!(
                            "{:width$}",
                            format!("{}({})", c.name, c.col_type),
                            width = col_len
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(" | ");

                writeln!(out, "Select:\n| {} |", columns).ok();

                let columns_sep = col_lengths
                    .iter()
                    .map(|col_len| "-".repeat(*col_len))
                    .collect::<Vec<_>>()
                    .join("-|-");
                writeln!(out, "|-{}-|", columns_sep).ok();
                for result in results.rows.iter() {
                    let column: String = result
                        .iter()
                        .zip(col_lengths.iter())
                        .map(|(col, col_len)| {
                            format!("{:width$}", col.to_string(), width = col_len)
                        })
                        .collect::<Vec<String>>()
                        .join(" | ");
                    writeln!(out, "| {} |", column).ok();
                }
                out
            }
            EvalResult::Insert { success, .. } => {
                if *success {
                    "Insert: success\n".to_string()
                } else {
                    "Insert: failure\n".to_string()
                }
            }
            EvalResult::DropTable { success, .. } => {
                if *success {
                    "DropTable: success\n".to_string()
                } else {
                    "DropTable: failure\n".to_string()
                }
            }
        }
    }
}
