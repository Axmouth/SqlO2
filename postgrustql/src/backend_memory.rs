extern crate byteorder;

use super::ast::*;
use super::backend::*;
use super::lexer::*;
use super::parser::parse;

use byteorder::{BigEndian, ReadBytesExt};
use std::collections::HashMap;
use std::io::Cursor;
use std::io::Read;
use std::time::Instant;

const ERR_INVALID_CELL: &str = "Invalid Cell";
const ERR_INVALID_OPERANDS: &str = "Invalid Operands";

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct MemoryCell {
    bytes: Vec<u8>,
}

impl Cell for MemoryCell {
    fn as_int(&self) -> Result<i32, &'static str> {
        let mut rdr = Cursor::new(&self.bytes);
        match rdr.read_i32::<BigEndian>() {
            Ok(result) => {
                return Ok(result);
            }
            Err(_err) => {
                return Err("Failed to parse bytes to int32.");
            }
        }
    }

    fn as_bool(&self) -> Result<bool, &'static str> {
        return Ok(self.bytes.len() > 0);
    }

    fn as_text(&self) -> Result<String, &'static str> {
        let mut rdr = Cursor::new(&self.bytes);

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

    fn equals(&self, other: Self) -> bool {
        return self.bytes == other.bytes;
    }
}

#[derive(Clone, Eq, PartialEq)]
struct Table {
    columns: Vec<String>,
    column_types: Vec<ColumnType>,
    rows: Vec<Vec<MemoryCell>>,
}

impl Table {
    pub fn evaluate_literal_cell(
        &self,
        row_index: u32,
        expression: &Expression,
    ) -> Result<(MemoryCell, &str, ColumnType), String> {
        match expression {
            Expression::Literal(literal_expression) => {
                let literal = &literal_expression.literal;

                match &literal.token {
                    Token::IdentifierValue { value } => {
                        for (i, table_col) in self.columns.iter().enumerate() {
                            if table_col == value.as_str() {
                                return Ok((
                                    self.rows[row_index as usize][i as usize].clone(),
                                    table_col,
                                    self.column_types[i].clone(),
                                ));
                            }
                        }

                        return Err(format!("{}: {}", value, ERR_COLUMN_DOES_NOT_EXIST).to_string());
                    }
                    Token::BoolValue { value } => {
                        if *value {
                            return Ok((get_true_mem_cell(), "?column?", ColumnType::BoolType));
                        } else {
                            return Ok((get_false_mem_cell(), "?column?", ColumnType::BoolType));
                        }
                    } /*
                    LexTokenKind::NumericKind => {
                    return Err(format!("{}", ERR_INVALID_CELL).to_string());
                    }*/
                    _ => {
                        let mut column_type = ColumnType::IntType;
                        match literal.token {
                            Token::StringValue { value: _ } => {
                                column_type = ColumnType::TextType;
                            }
                            Token::BoolValue { value: _ } => {
                                column_type = ColumnType::BoolType;
                            }
                            _ => {}
                        }
                        let mem_cell = match literal_to_memory_cell(&literal) {
                            Err(err) => {
                                return Err(err);
                            }
                            Ok(value) => value,
                        };

                        return Ok((mem_cell, "?column?", column_type));
                    }
                }
            }
            _ => return Err(ERR_INVALID_CELL.to_string()),
        }
    }

    pub fn evaluate_binary_cell(
        &self,
        row_index: u32,
        expression: &Expression,
    ) -> Result<(MemoryCell, &str, ColumnType), String> {
        match expression {
            Expression::Binary(binary_expression) => {
                let (first_mem_cell, _, first_col_type) =
                    self.evaluate_cell(row_index, &binary_expression.first)?;

                let (second_mem_cell, _, second_col_type) =
                    self.evaluate_cell(row_index, &binary_expression.second)?;

                match binary_expression.operand.token {
                    Token::Equal => {
                        let equal = first_mem_cell.equals(second_mem_cell);
                        if first_col_type == ColumnType::TextType
                            && second_col_type == ColumnType::TextType
                            && equal
                        {
                            return Ok((get_true_mem_cell(), "?column?", ColumnType::BoolType));
                        }
                        if first_col_type == ColumnType::IntType
                            && second_col_type == ColumnType::IntType
                            && equal
                        {
                            return Ok((get_true_mem_cell(), "?column?", ColumnType::BoolType));
                        }
                        if first_col_type == ColumnType::BoolType
                            && second_col_type == ColumnType::BoolType
                            && equal
                        {
                            return Ok((get_true_mem_cell(), "?column?", ColumnType::BoolType));
                        }
                        return Ok((get_false_mem_cell(), "?column?", ColumnType::BoolType));
                    }
                    Token::NotEqual => {
                        if first_col_type != second_col_type
                            || !&first_mem_cell.equals(second_mem_cell)
                        {
                            return Ok((get_true_mem_cell(), "?column?", ColumnType::BoolType));
                        }
                        return Ok((get_false_mem_cell(), "?column?", ColumnType::BoolType));
                    }
                    Token::GreaterThan => {
                        let greater_than = first_mem_cell.bytes > second_mem_cell.bytes;
                        if first_col_type == ColumnType::TextType
                            && second_col_type == ColumnType::TextType
                            && greater_than
                        {
                            return Ok((get_true_mem_cell(), "?column?", ColumnType::BoolType));
                        }
                        if first_col_type == ColumnType::IntType
                            && second_col_type == ColumnType::IntType
                            && greater_than
                        {
                            return Ok((get_true_mem_cell(), "?column?", ColumnType::BoolType));
                        }
                        if first_col_type == ColumnType::BoolType
                            && second_col_type == ColumnType::BoolType
                            && greater_than
                        {
                            return Ok((get_true_mem_cell(), "?column?", ColumnType::BoolType));
                        }
                        return Ok((get_false_mem_cell(), "?column?", ColumnType::BoolType));
                    }
                    Token::GreaterThanOrEqual => {
                        let greater_than_or_equal = first_mem_cell.bytes >= second_mem_cell.bytes;
                        if first_col_type == ColumnType::TextType
                            && second_col_type == ColumnType::TextType
                            && greater_than_or_equal
                        {
                            return Ok((get_true_mem_cell(), "?column?", ColumnType::BoolType));
                        }
                        if first_col_type == ColumnType::IntType
                            && second_col_type == ColumnType::IntType
                            && greater_than_or_equal
                        {
                            return Ok((get_true_mem_cell(), "?column?", ColumnType::BoolType));
                        }
                        if first_col_type == ColumnType::BoolType
                            && second_col_type == ColumnType::BoolType
                            && greater_than_or_equal
                        {
                            return Ok((get_true_mem_cell(), "?column?", ColumnType::BoolType));
                        }
                        return Ok((get_false_mem_cell(), "?column?", ColumnType::BoolType));
                    }
                    Token::LessThan => {
                        let less_than = first_mem_cell.bytes < second_mem_cell.bytes;
                        if first_col_type == ColumnType::TextType
                            && second_col_type == ColumnType::TextType
                            && less_than
                        {
                            return Ok((get_true_mem_cell(), "?column?", ColumnType::BoolType));
                        }
                        if first_col_type == ColumnType::IntType
                            && second_col_type == ColumnType::IntType
                            && less_than
                        {
                            return Ok((get_true_mem_cell(), "?column?", ColumnType::BoolType));
                        }
                        if first_col_type == ColumnType::BoolType
                            && second_col_type == ColumnType::BoolType
                            && less_than
                        {
                            return Ok((get_true_mem_cell(), "?column?", ColumnType::BoolType));
                        }
                        return Ok((get_false_mem_cell(), "?column?", ColumnType::BoolType));
                    }
                    Token::LessThanOrEqual => {
                        let less_than_or_equal = first_mem_cell.bytes <= second_mem_cell.bytes;
                        if first_col_type == ColumnType::TextType
                            && second_col_type == ColumnType::TextType
                            && less_than_or_equal
                        {
                            return Ok((get_true_mem_cell(), "?column?", ColumnType::BoolType));
                        }
                        if first_col_type == ColumnType::IntType
                            && second_col_type == ColumnType::IntType
                            && less_than_or_equal
                        {
                            return Ok((get_true_mem_cell(), "?column?", ColumnType::BoolType));
                        }
                        if first_col_type == ColumnType::BoolType
                            && second_col_type == ColumnType::BoolType
                            && less_than_or_equal
                        {
                            return Ok((get_true_mem_cell(), "?column?", ColumnType::BoolType));
                        }
                        return Ok((get_false_mem_cell(), "?column?", ColumnType::BoolType));
                    }
                    Token::Concat => {
                        if first_col_type != ColumnType::TextType
                            || second_col_type != ColumnType::TextType
                        {
                            return Err(ERR_INVALID_OPERANDS.to_string());
                        }
                        let mut token = TokenContainer::new();
                        let mut first_value = first_mem_cell.as_text()?;
                        let second_value = second_mem_cell.as_text()?;

                        first_value.push_str(&second_value);
                        token.token = Token::StringValue { value: first_value };

                        return Ok((
                            literal_to_memory_cell(&token)?,
                            "?column?",
                            ColumnType::TextType,
                        ));
                    }
                    Token::Plus => {
                        if first_col_type != ColumnType::IntType
                            || second_col_type != ColumnType::IntType
                        {
                            return Err(ERR_INVALID_OPERANDS.to_string());
                        }

                        let int_value = first_mem_cell.as_int()? + second_mem_cell.as_int()?;
                        let mut token = TokenContainer::new();
                        token.token = Token::NumericValue {
                            value: int_value.to_string(),
                        };
                        return Ok((
                            literal_to_memory_cell(&token)?,
                            "?column?",
                            ColumnType::IntType,
                        ));
                    }
                    Token::Minus => {
                        if first_col_type != ColumnType::IntType
                            || second_col_type != ColumnType::IntType
                        {
                            return Err(ERR_INVALID_OPERANDS.to_string());
                        }

                        let int_value = first_mem_cell.as_int()? - second_mem_cell.as_int()?;
                        let mut token = TokenContainer::new();
                        token.token = Token::NumericValue {
                            value: int_value.to_string(),
                        };
                        return Ok((
                            literal_to_memory_cell(&token)?,
                            "?column?",
                            ColumnType::IntType,
                        ));
                    }
                    Token::And => {
                        if first_col_type != ColumnType::BoolType
                            || second_col_type != ColumnType::BoolType
                        {
                            return Err(ERR_INVALID_OPERANDS.to_string());
                        }

                        let mut result_cell = get_false_mem_cell();
                        if first_mem_cell.as_bool()? && second_mem_cell.as_bool()? {
                            result_cell = get_true_mem_cell();
                        }

                        return Ok((result_cell, "?column?", ColumnType::BoolType));
                    }
                    Token::Or => {
                        if first_col_type != ColumnType::BoolType
                            || second_col_type != ColumnType::BoolType
                        {
                            return Err(ERR_INVALID_OPERANDS.to_string());
                        }

                        let mut result_cell = get_false_mem_cell();
                        if first_mem_cell.as_bool()? || second_mem_cell.as_bool()? {
                            result_cell = get_true_mem_cell();
                        }

                        return Ok((result_cell, "?column?", ColumnType::BoolType));
                    }
                    _ => {
                        return Err(ERR_INVALID_CELL.to_string());
                    }
                }
            }
            _ => return Err(ERR_INVALID_CELL.to_string()),
        }
    }

    pub fn evaluate_cell(
        &self,
        row_index: u32,
        expression: &Expression,
    ) -> Result<(MemoryCell, &str, ColumnType), String> {
        match expression {
            Expression::Literal(_) => {
                return self.evaluate_literal_cell(row_index, expression);
            }
            Expression::Binary(_) => {
                return self.evaluate_binary_cell(row_index, expression);
            }
            _ => return Err(ERR_INVALID_CELL.to_string()),
        }
    }
}

#[derive(Eq, PartialEq)]
pub struct MemoryBackend {
    tables: HashMap<String, Table>,
}

pub fn get_true_mem_cell() -> MemoryCell {
    MemoryCell { bytes: vec![1] }
}
pub fn get_false_mem_cell() -> MemoryCell {
    MemoryCell { bytes: vec![] }
}
pub fn get_true_lex_token() -> TokenContainer {
    TokenContainer {
        token: Token::BoolValue { value: true },
        loc: TokenLocation { line: 0, col: 0 },
    }
}
pub fn get_false_lex_token() -> TokenContainer {
    TokenContainer {
        token: Token::BoolValue { value: false },
        loc: TokenLocation { line: 0, col: 0 },
    }
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

        match self.tables.get(&create_statement.name) {
            Some(_) => {
                return Err(format!(
                    "Table \"{}\" already exists.",
                    create_statement.name
                ));
            }
            _ => {}
        }

        for col in create_statement.cols {
            new_table.columns.push(col.name);

            let data_type;
            if col.data_type.token == Token::Int {
                data_type = ColumnType::IntType;
            } else if col.data_type.token == Token::Text {
                data_type = ColumnType::TextType;
            } else {
                return Err(ERR_INVALID_DATA_TYPE.to_owned());
            }

            new_table.column_types.push(data_type);
        }

        self.tables.insert(create_statement.name, new_table);

        return Ok(true);
    }

    pub fn insert(&mut self, insert_statement: InsertStatement) -> Result<bool, String> {
        let &table = &match self.tables.get(&insert_statement.table) {
            Some(value) => value,
            None => {
                return Err(ERR_TABLE_DOES_NOT_EXIST.to_owned());
            }
        };

        if insert_statement.values.len() != table.columns.len() {
            return Err(ERR_MISSING_VALUES.to_owned());
        }

        let mut row: Vec<MemoryCell> = vec![];

        for value in insert_statement.values {
            match value {
                Expression::Literal(value) => {
                    let cell = literal_to_memory_cell(&value.literal)?;
                    row.push(cell);
                }
                _ => {
                    println!("Skipping non-literal.");
                    continue;
                }
            }
        }

        let table = match self.tables.get_mut(&insert_statement.table) {
            Some(value) => value,
            None => {
                return Err(ERR_TABLE_DOES_NOT_EXIST.to_owned());
            }
        };

        table.rows.push(row);

        return Ok(true);
    }

    pub fn select(
        &self,
        select_statement: SelectStatement,
    ) -> Result<QueryResults<MemoryCell>, String> {
        let table: Option<&Table> = match select_statement.from {
            Some(from_name) => match self.tables.get(&from_name) {
                None => {
                    return Err(ERR_TABLE_DOES_NOT_EXIST.to_string());
                }
                Some(value) => Some(value),
            },
            None => None,
        };
        let mut results: Vec<Vec<MemoryCell>> = vec![];

        let mut columns: ResultColumns = vec![];
        if let Some(table) = table {
            if select_statement.items.len() == 0 {
                return Ok(QueryResults {
                    columns: vec![],
                    rows: vec![],
                });
            }

            for row_index in 0..table.rows.len() {
                let mut result: Vec<MemoryCell> = vec![];
                let is_first_row = results.len() == 0;

                match &select_statement.where_clause {
                    Expression::Empty => {}
                    _ => {
                        let (mem_cell, _, _) = table
                            .evaluate_cell(row_index as u32, &select_statement.where_clause)?;

                        if !mem_cell.as_bool()? {
                            continue;
                        }
                    }
                }

                for select_item in &select_statement.items {
                    if select_item.asterisk {
                        for (col_name, _) in table.columns.iter().zip(table.column_types.iter()) {
                            let (mem_cell, col_name, col_type) = table.evaluate_cell(
                                row_index as u32,
                                &Expression::Literal(LiteralExpression {
                                    literal: TokenContainer::new_with_kind_and_value(
                                        Token::IdentifierValue {
                                            value: col_name.clone(),
                                        },
                                        col_name.clone(),
                                    ),
                                }),
                            )?;
                            if is_first_row {
                                columns.push(ResultColumn {
                                    col_type: col_type.clone(),
                                    name: col_name.to_string(),
                                });
                            }
                            result.push(mem_cell);
                        }
                        continue;
                    }

                    let (mem_cell, col_name, col_type) =
                        table.evaluate_cell(row_index as u32, &select_item.expression)?;

                    if is_first_row {
                        match &select_item.as_clause {
                            Some(as_name) => {
                                columns.push(ResultColumn {
                                    col_type: col_type.clone(),
                                    name: as_name.clone(),
                                });
                            }
                            None => {
                                columns.push(ResultColumn {
                                    col_type: col_type.clone(),
                                    name: col_name.to_string(),
                                });
                            }
                        }
                    }

                    result.push(mem_cell);
                }

                results.push(result);
            }
        }

        return Ok(QueryResults {
            columns,
            rows: results,
        });
    }

    pub fn eval_query(&mut self, query: &str) -> Result<Vec<EvalResult<MemoryCell>>, String> {
        let mut before = Instant::now();
        let ast = parse(query)?;

        let mut eval_results = vec![];

        for statement in ast.statements {
            match statement {
                Statement::CreateTableStatement(create_table_statement) => {
                    let result = self.create_table(create_table_statement)?;

                    eval_results.push(EvalResult::CreateTable {
                        success: result,
                        time: before.elapsed(),
                    });
                }
                Statement::InsertStatement(insert_statement) => {
                    let result = self.insert(insert_statement)?;
                    eval_results.push(EvalResult::Insert {
                        success: result,
                        time: before.elapsed(),
                    });
                    before = Instant::now();
                }
                Statement::SelectStatement(select_statement) => {
                    let results = self.select(select_statement)?;
                    eval_results.push(EvalResult::Select {
                        results,
                        time: before.elapsed(),
                    });
                    before = Instant::now();
                }
            }
        }

        return Ok(eval_results);
    }
}

pub fn literal_to_memory_cell(token: &TokenContainer) -> Result<MemoryCell, String> {
    match &token.token {
        Token::NumericValue { value } => {
            let num_result = value.parse::<i32>();

            if num_result.is_err() {
                return Err("Failed to parse number".to_string());
            }
            let bytes = match num_result {
                Ok(value) => value.to_be_bytes().into(),
                Err(err) => {
                    return Err(err.to_string());
                }
            };
            return Ok(MemoryCell { bytes });
        }
        Token::StringValue { value } => {
            return Ok(MemoryCell {
                bytes: value.as_bytes().into(),
            });
        }
        Token::BoolValue { value } => {
            if *value {
                return Ok(MemoryCell { bytes: vec![1] });
            }
            return Ok(MemoryCell { bytes: vec![] });
        }
        _ => {
            return Ok(MemoryCell { bytes: vec![] });
        }
    }
}
