extern crate byteorder;

use super::backend::*;
use super::lexer::*;
use super::parser::*;

use byteorder::{BigEndian, ReadBytesExt};
use std::collections::HashMap;
use std::io::Cursor;
use std::io::Read;

#[derive(Clone, Eq, PartialEq)]
pub struct MemoryCell {
    bytes: Vec<u8>,
}

impl Cell for MemoryCell {
    fn as_int(&self) -> Result<i32, &'static str> {
        let mut rdr = Cursor::new(self.bytes.clone());
        match rdr.read_i32::<BigEndian>() {
            Ok(result) => {
                return Ok(result);
            }
            Err(_err) => {
                return Err("Failed to parse bytes to int32.");
            }
        }
    }

    fn as_text(&self) -> Result<String, &'static str> {
        let mut rdr = Cursor::new(self.bytes.clone());

        let mut text = "".to_owned();
        match rdr.read_to_string(&mut text) {
            Ok(_) => {
                return Ok(text);
            }
            Err(_err) => {
                return Err("Failed to parse bytes to String.");
            }
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
struct Table {
    columns: Vec<String>,
    column_types: Vec<ColumnType>,
    rows: Vec<Vec<MemoryCell>>,
}

#[derive(Clone, Eq, PartialEq)]
pub struct MemoryBackend {
    tables: HashMap<String, Table>,
}

impl MemoryBackend {
    pub fn new() -> MemoryBackend {
        return Self {
            tables: HashMap::new(),
        };
    }

    pub fn create_table(&mut self, create_statement: CreateTableStatement) -> Result<bool, String> {
        let mut new_table = Table {
            columns: vec![],
            column_types: vec![],
            rows: vec![],
        };

        if create_statement.cols.len() == 0 {
            return Err("No Table Columns.".to_owned());
        }

        match self.tables.get(&create_statement.name.value) {
            Some(_) => {
                return Err(format!(
                    "Table \"{}\" already exists.",
                    create_statement.name.value
                ));
            }
            _ => {}
        }

        for col in create_statement.cols {
            new_table.columns.push(col.name.value);

            let data_type;
            if col.data_type.value == "int".to_owned() {
                data_type = ColumnType::IntType;
            } else if col.data_type.value == "text".to_owned() {
                data_type = ColumnType::TextType;
            } else {
                return Err(ERR_INVALID_DATA_TYPE.to_owned());
            }

            new_table.column_types.push(data_type);
        }

        self.tables.insert(create_statement.name.value, new_table);

        return Ok(true);
    }

    pub fn insert(&mut self, insert_statement: InsertStatement) -> Result<bool, String> {
        let table = self.tables.get(&insert_statement.table.value);

        match table {
            Some(table) => {
                if insert_statement.values.len() == 0 {
                    return Err(ERR_MISSING_VALUES.to_owned());
                }

                let mut table: Table = table.clone();

                let mut row: Vec<MemoryCell> = vec![];

                for value in insert_statement.values {
                    if value.kind != ExpressionKind::LiteralKind {
                        println!("Skipping non-literal.");
                        continue;
                    }
                    let cell = MemoryBackend::token_to_cell(value.literal);
                    row.push(cell);
                }

                table.rows.push(row);

                self.tables.insert(insert_statement.table.value, table);

                return Ok(true);
            }
            None => {
                return Err(ERR_TABLE_DOES_NOT_EXIST.to_owned());
            }
        }
    }

    pub fn token_to_cell(token: LexToken) -> MemoryCell {
        match token.kind {
            LexTokenKind::NumericKind => {
                let i = token.value.parse::<i32>();
                match i {
                    Ok(i) => {
                        return MemoryCell {
                            bytes: i.to_be_bytes().into(),
                        };
                    }
                    Err(err) => {
                        eprint!("{}", err);
                        return MemoryCell { bytes: vec![] };
                    }
                }
            }
            LexTokenKind::StringKind => {
                return MemoryCell {
                    bytes: token.value.as_bytes().into(),
                };
            }
            _ => {
                return MemoryCell { bytes: vec![] };
            }
        }
    }

    pub fn select(&self, select_statement: SelectStatement) -> Result<QueryResults, String> {
        let table = self.tables.get(&select_statement.from.value);

        match table {
            None => {
                return Err(ERR_TABLE_DOES_NOT_EXIST.to_owned());
            }
            Some(table) => {
                let mut results: Vec<Vec<Box<dyn Cell>>> = vec![];

                let mut columns: ResultColumns = vec![];

                for i in 0..table.rows.len() {
                    let row = table.rows[i].clone();

                    let mut result: Vec<Box<dyn Cell>> = vec![];
                    let is_first_row = i == 0;

                    for expression in select_statement.item.clone() {
                        if expression.kind != ExpressionKind::LiteralKind {
                            // Unsupported, doesn't currently exist, ignore.
                            println!("Skipping non-literal expression.");
                            continue;
                        }

                        let literal = expression.literal;
                        if literal.kind == LexTokenKind::IdentifierKind {
                            let mut found = false;
                            for i in 0..table.columns.len() {
                                let table_column = table.columns[i].clone();
                                if table_column == literal.value {
                                    if is_first_row {
                                        columns.push(ResultColumn {
                                            col_type: table.column_types[i].clone(),
                                            name: literal.value,
                                        });
                                    }

                                    result.push(Box::new(row[i].clone()));
                                    found = true;
                                    break;
                                }
                            }

                            if !found {
                                return Err(ERR_COLUMN_DOES_NOT_EXIST.to_owned());
                            }

                            continue;
                        }

                        return Err(ERR_COLUMN_DOES_NOT_EXIST.to_owned());
                    }
                    results.push(result);
                }

                return Ok(QueryResults {
                    columns: columns,
                    rows: results,
                });
            }
        }
    }
}
