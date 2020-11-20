extern crate byteorder;

use super::ast::*;
use super::backend::*;
use super::lexer::*;
use super::parser::parse;

use crate::{
    backend::{MemoryCell, MemoryCellData},
    sql_types::{SqlNumeric, SqlType, SqlValue},
};
use instant::Instant;
use std::collections::HashMap;

const ERR_INVALID_CELL: &str = "Invalid Cell";
const ERR_INVALID_OPERANDS: &str = "Invalid Operands";

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Index {
    name: String,
    expression: Expression,
    unique: bool,
    primary_key: bool,
    typ: String,
    tree: std::collections::btree_map::BTreeMap<Vec<u8>, Vec<usize>>,
}

impl Index {
    pub fn add_row(&mut self, table: &Table, row_index: usize) -> Result<(), String> {
        let (index_value, _, _) = table.evaluate_cell(row_index, &self.expression)?;

        if index_value.is_null() {
            return Err("Violates NOT NULL Constraint".to_string());
        }

        if self.unique {
            if let Some(_) = self.tree.get(&index_value.encode().bytes) {
                return Err("Violates UNIQUE Constraint".to_string());
            }
        }

        match self.tree.get_mut(&index_value.encode().bytes) {
            Some(row_indexes) => {
                row_indexes.push(row_index);
            }
            None => {
                self.tree
                    .insert(index_value.encode().bytes, vec![row_index]);
            }
        }

        Ok(())
    }

    pub fn applicable_value(&self, exp: &Expression) -> Result<Option<Expression>, String> {
        if let Expression::Binary(bin_exp) = exp {
            // Find the column and the value in the boolean expression
            let mut column_exp = *bin_exp.first.clone();
            let mut value_exp = *bin_exp.second.clone();
            if column_exp != self.expression {
                column_exp = *bin_exp.second.clone();
                value_exp = *bin_exp.first.clone();
            }

            // Neither side is applicable, return None
            if column_exp != self.expression {
                return Ok(None);
            }

            let supported_checks = vec![
                Token::Equal,
                Token::NotEqual,
                Token::GreaterThan,
                Token::GreaterThanOrEqual,
                Token::LessThan,
                Token::LessThanOrEqual,
            ];
            let is_supported = supported_checks.contains(&bin_exp.operand.token);

            if is_supported == false {
                return Ok(None);
            }
            if let Expression::Literal(lit_exp) = value_exp {
                return Ok(Some(Expression::Literal(lit_exp)));
            } else {
                eprintln!("Only index checks on literals supported");
                return Ok(None);
            }
        } else {
            return Ok(None);
        }
    }

    pub fn new_table_from_subset(
        &self,
        table: &Table,
        bin_exp: BinaryExpression,
    ) -> Result<Table, String> {
        let value_exp = match self.applicable_value(&Expression::Binary(bin_exp.clone()))? {
            Some(value) => value,
            None => {
                return Ok(table.clone());
            }
        };

        let mut new_table = Table {
            column_types: table.column_types.clone(),
            columns: table.columns.clone(),
            indexes: vec![],
            name: table.name.clone(),
            rows: vec![],
        };

        let (value, _, _) = match new_table.evaluate_cell(0, &value_exp) {
            Ok(value) => value,
            Err(err) => {
                eprintln!("{}", err);
                return Ok(table.clone());
            }
        };

        let mut row_indexes: Vec<usize> = vec![];

        match bin_exp.operand.token {
            Token::Equal => {
                match self.tree.get(&value.encode().bytes) {
                    Some(indexes) => {
                        row_indexes.append(&mut indexes.clone());
                    }
                    None => {}
                };
            }
            Token::NotEqual => {
                for (key, indexes) in &self.tree {
                    if *key == value.encode().bytes {
                        continue;
                    }
                    row_indexes.append(&mut indexes.clone());
                }
            }
            Token::LessThan => {
                for (key, indexes) in &self.tree {
                    if key >= &value.encode().bytes {
                        break;
                    }
                    row_indexes.append(&mut indexes.clone());
                }
            }
            Token::LessThanOrEqual => {
                for (key, indexes) in &self.tree {
                    if key > &value.encode().bytes {
                        break;
                    }
                    row_indexes.append(&mut indexes.clone());
                }
            }
            Token::GreaterThan => {
                for (_, ref mut indexes) in self.tree.clone().split_off(&value.encode().bytes) {
                    row_indexes.append(indexes);
                }
            }
            Token::GreaterThanOrEqual => {
                match self.tree.get(&value.encode().bytes) {
                    Some(indexes) => {
                        row_indexes.append(&mut indexes.clone());
                    }
                    None => {}
                };
                for (_, ref mut indexes) in self.tree.clone().split_off(&value.encode().bytes) {
                    row_indexes.append(indexes);
                }
            }
            _ => {}
        }

        for index in row_indexes {
            if let Some(row) = table.rows.get(index) {
                new_table.rows.push(row.clone());
            }
        }

        return Ok(table.clone());
    }
}

#[derive(Clone, PartialEq)]
pub struct Table {
    name: String,
    columns: Vec<String>,
    column_types: Vec<SqlType>,
    rows: Vec<Vec<SqlValue>>,
    indexes: Vec<Index>,
}

impl Table {
    pub fn evaluate_literal_cell(
        &self,
        row_index: usize,
        expression: &Expression,
    ) -> Result<(SqlValue, &str, SqlType), String> {
        match expression {
            Expression::Literal(literal_expression) => {
                let literal = &literal_expression.literal;

                match &literal.token {
                    Token::IdentifierValue { value } => {
                        for (i, table_col) in self.columns.iter().enumerate() {
                            if table_col == value.as_str() {
                                let typ =
                                    self.column_types.get(i).ok_or("Error accesing column")?;
                                let val = self
                                    .rows
                                    .get(row_index as usize)
                                    .ok_or("Error accesing row")?
                                    .get(i as usize)
                                    .ok_or("Error accesing row's column")?;
                                return Ok((val.clone(), table_col, *typ));
                            }
                        }

                        return Err(format!("{}: {}", value, ERR_COLUMN_DOES_NOT_EXIST).to_string());
                    }
                    _ => {
                        let val = SqlValue::from_token(&literal.token)?;
                        let typ = val.get_type();
                        return Ok((val, "?column?", typ));
                    } /*
                      Token::BoolValue { value } => {
                          if *value {
                              return Ok((get_true_mem_cell(), "?column?", SqlType::Boolean));
                          } else {
                              return Ok((get_false_mem_cell(), "?column?", SqlType::Boolean));
                          }
                      }
                      LexTokenKind::NumericKind => {
                      return Err(format!("{}", ERR_INVALID_CELL).to_string());
                      }
                      _ => {
                          let mut column_type = SqlType::Int;
                          match literal.token {
                              Token::StringValue { value: _ } => {
                                  column_type = SqlType::Text;
                              }
                              Token::BoolValue { value: _ } => {
                                  column_type = SqlType::Boolean;
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
                      }*/
                }
            }
            _ => return Err(ERR_INVALID_CELL.to_string()),
        }
    }

    pub fn evaluate_binary_cell(
        &self,
        row_index: usize,
        expression: &Expression,
    ) -> Result<(SqlValue, &str, SqlType), String> {
        match expression {
            Expression::Binary(binary_expression) => {
                let (first_val, _, first_col_type) =
                    self.evaluate_cell(row_index, &binary_expression.first)?;

                let (second_val, _, second_col_type) =
                    self.evaluate_cell(row_index, &binary_expression.second)?;

                match binary_expression.operand.token {
                    Token::Equal => {
                        let result = SqlValue::equals(first_val, second_val)?;
                        let typ = result.get_type();
                        return Ok((result, "?column?", typ));
                    }
                    Token::NotEqual => {
                        let result = SqlValue::not_equal(first_val, second_val)?;
                        let typ = result.get_type();
                        return Ok((result, "?column?", typ));
                    }
                    Token::GreaterThan => {
                        let result = SqlValue::greater_than(first_val, second_val)?;
                        let typ = result.get_type();
                        return Ok((result, "?column?", typ));
                    }
                    Token::GreaterThanOrEqual => {
                        let result = SqlValue::greater_than_or_equals(first_val, second_val)?;
                        let typ = result.get_type();
                        return Ok((result, "?column?", typ));
                    }
                    Token::LessThan => {
                        let result = SqlValue::less_than(first_val, second_val)?;
                        let typ = result.get_type();
                        return Ok((result, "?column?", typ));
                    }
                    Token::LessThanOrEqual => {
                        let result = SqlValue::less_than_or_equals(first_val, second_val)?;
                        let typ = result.get_type();
                        return Ok((result, "?column?", typ));
                    }
                    Token::Concat => {
                        let result = SqlValue::concat(first_val, second_val)?;
                        let typ = result.get_type();
                        return Ok((result, "?column?", typ));
                    }
                    Token::Plus => {
                        let result = SqlValue::add(first_val, second_val)?;
                        let typ = result.get_type();
                        return Ok((result, "?column?", typ));
                    }
                    Token::Minus => {
                        let result = SqlValue::subtract(first_val, second_val)?;
                        let typ = result.get_type();
                        return Ok((result, "?column?", typ));
                    }
                    Token::Asterisk => {
                        let result = SqlValue::multiply(first_val, second_val)?;
                        let typ = result.get_type();
                        return Ok((result, "?column?", typ));
                    }
                    Token::Slash => {
                        let result = SqlValue::divide(first_val, second_val)?;
                        let typ = result.get_type();
                        return Ok((result, "?column?", typ));
                    }
                    Token::Modulo => {
                        let result = SqlValue::modulo(first_val, second_val)?;
                        let typ = result.get_type();
                        return Ok((result, "?column?", typ));
                    }
                    Token::And => {
                        let result = SqlValue::and(first_val, second_val)?;
                        let typ = result.get_type();
                        return Ok((result, "?column?", typ));
                    }
                    Token::Or => {
                        let result = SqlValue::or(first_val, second_val)?;
                        let typ = result.get_type();
                        return Ok((result, "?column?", typ));
                    }
                    Token::Exponentiation => {
                        let result = SqlValue::exponentiation(first_val, second_val)?;
                        let typ = result.get_type();
                        return Ok((result, "?column?", typ));
                    }
                    _ => {
                        return Err(ERR_INVALID_CELL.to_string());
                    }
                }
            }
            Expression::Unary(unary_expression) => {
                let (val, _, first_col_type) =
                    self.evaluate_cell(row_index, &unary_expression.first)?;
                match unary_expression.operand.token {
                    Token::Minus => {
                        let result = SqlValue::subtract(
                            SqlValue::Numeric(SqlNumeric::SmallInt { value: 0 }),
                            val,
                        )?;
                        let typ = result.get_type();
                        return Ok((result, "?column?", typ));
                    }
                    Token::SquareRoot => {
                        let result = SqlValue::multiply(val.clone(), val)?;
                        let typ = result.get_type();
                        return Ok((result, "?column?", typ));
                    }
                    _ => return Err(ERR_INVALID_CELL.to_string()),
                }
            }
            _ => return Err(ERR_INVALID_CELL.to_string()),
        }
    }

    pub fn evaluate_cell(
        &self,
        row_index: usize,
        expression: &Expression,
    ) -> Result<(SqlValue, &str, SqlType), String> {
        match expression {
            Expression::Literal(_) => {
                return self.evaluate_literal_cell(row_index, expression);
            }
            Expression::Binary(_) | Expression::Unary(_) => {
                return self.evaluate_binary_cell(row_index, expression);
            }
            _ => return Err(ERR_INVALID_CELL.to_string()),
        }
    }

    pub fn get_applicable_indexes(
        &self,
        where_clause: Option<&Expression>,
    ) -> Result<Vec<(Index, Expression)>, String> {
        let exps = linearize_expressions(where_clause.map(|v| v.clone()), vec![]);

        let mut indexes_and_expressions = vec![];
        for exp in &exps {
            for index in &self.indexes {
                if let Some(_) = index.applicable_value(exp)? {
                    let index_and_expression = (index.clone(), exp.clone());
                    indexes_and_expressions.push(index_and_expression);
                }
            }
        }

        Ok(indexes_and_expressions)
    }
}

#[derive(PartialEq)]
pub struct MemoryBackend {
    tables: HashMap<String, Table>,
}

pub fn get_true_mem_cell() -> MemoryCell {
    MemoryCell { bytes: vec![1] }
}
pub fn get_false_mem_cell() -> MemoryCell {
    MemoryCell { bytes: vec![0] }
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
            name: create_statement.name.clone(),
            columns: vec![],
            column_types: vec![],
            rows: vec![],
            indexes: vec![],
        };

        if create_statement.cols.len() == 0 {
            return Err("No Table Columns.".to_owned());
        }

        match self.tables.get(&create_statement.name) {
            Some(_) => {
                return Err(format!(
                    "Table \"{}\" already exists.",
                    create_statement.name.clone()
                ));
            }
            _ => {}
        }

        let mut primary_key: Option<Expression> = None;

        for col in create_statement.cols {
            new_table.columns.push(col.name.clone());

            let data_type;
            if let Ok(typ) = SqlType::from_token(col.data_type.token) {
                data_type = typ;
            } else {
                self.tables.remove(&new_table.name);
                return Err(ERR_INVALID_DATA_TYPE.to_string());
            }

            if col.is_primary_key {
                if primary_key != None {
                    self.tables.remove(&new_table.name);
                    return Err("Primary Key already exists".to_string());
                }
                primary_key = Some(Expression::new_literal_id(col.name));
            }

            new_table.column_types.push(data_type);
        }

        self.tables.insert(create_statement.name.clone(), new_table);

        if let Some(primary_key) = primary_key {
            match self.create_index(CreateIndexStatement {
                table: create_statement.name.clone(),
                name: format!("{}_pkey", create_statement.name),
                is_unique: true,
                is_primary_key: true,
                expression: primary_key,
            }) {
                Err(err) => {
                    self.tables.remove(&create_statement.name);
                    return Err(err);
                }
                _ => {}
            }
        }

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

        let mut row: Vec<SqlValue> = vec![];

        for (i, value) in insert_statement.values.iter().enumerate() {
            match value {
                Expression::Literal(value) => {
                    let typ = match table.column_types.get(i) {
                        Some(val) => val,
                        None => {
                            return Err("Column type not found".to_owned());
                        }
                    };

                    let cell = literal_to_memory_cell(&value.literal)?.to_type(*typ)?;
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
        let row_index = table.rows.len() - 1;
        for i in 0..table.indexes.len() {
            let index = match table.indexes.get(i) {
                None => {
                    return Err("Index not found".to_string());
                }
                Some(value) => value,
            };
            let (index_value, _, _) = table.evaluate_cell(row_index, &index.expression)?;

            if index_value.is_null() {
                return Err("Violates NOT NULL Constraint".to_string());
            }

            if index.unique {
                if let Some(_) = index.tree.get(&index_value.encode().bytes) {
                    return Err("Violates UNIQUE Constraint".to_string());
                }
            }
            let index = match table.indexes.get_mut(i) {
                None => {
                    return Err("Index not found".to_string());
                }
                Some(value) => value,
            };

            match index.tree.get_mut(&index_value.encode().bytes) {
                Some(row_indexes) => {
                    row_indexes.push(row_index);
                }
                None => {
                    table.indexes[i]
                        .tree
                        .insert(index_value.encode().bytes, vec![row_index]);
                }
            }
        }

        return Ok(true);
    }

    pub fn select(
        &self,
        select_statement: SelectStatement,
    ) -> Result<QueryResults<SqlValue>, String> {
        let mut new_table = Table {
            column_types: vec![],
            columns: vec![],
            indexes: vec![],
            name: "".to_string(),
            rows: vec![],
        };

        let mut table: &Table = match select_statement.from {
            Some(from_name) => match self.tables.get(&from_name) {
                None => {
                    return Err(ERR_TABLE_DOES_NOT_EXIST.to_string());
                }
                Some(value) => value,
            },
            None => {
                new_table.rows.push(vec![]);
                &new_table
            }
        };
        if select_statement.items.len() == 0 {}

        let mut results: Vec<Vec<SqlValue>> = vec![];

        let mut columns: ResultColumns = vec![];
        if select_statement.items.len() == 0 {
            return Ok(QueryResults {
                columns: vec![],
                rows: vec![],
            });
        }

        for (index, exp) in table.get_applicable_indexes(Some(&select_statement.where_clause))? {
            if let Expression::Binary(bin_exp) = exp {
                new_table = index.new_table_from_subset(table, bin_exp)?;
                table = &new_table;
            }
        }

        for row_index in 0..table.rows.len() {
            let mut result: Vec<SqlValue> = vec![];
            let is_first_row = results.len() == 0;

            match &select_statement.where_clause {
                Expression::Empty => {}
                _ => {
                    let (cell_val, _, _) =
                        table.evaluate_cell(row_index, &select_statement.where_clause)?;

                    if let SqlValue::Boolean(true) = cell_val {
                    } else {
                        continue;
                    }
                }
            }

            for select_item in &select_statement.items {
                if select_item.asterisk {
                    for (col_name, _) in table.columns.iter().zip(table.column_types.iter()) {
                        let (cell_val, col_name, col_type) = table.evaluate_cell(
                            row_index,
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
                        result.push(cell_val);
                    }
                    continue;
                }

                let (cell_val, col_name, col_type) =
                    table.evaluate_cell(row_index, &select_item.expression)?;

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

                result.push(cell_val);
            }

            results.push(result);
        }

        return Ok(QueryResults {
            columns,
            rows: results,
        });
    }

    pub fn eval_query(&mut self, query: &str) -> Result<Vec<EvalResult<SqlValue>>, String> {
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
                Statement::CreateIndexStatement(create_index_statement) => {
                    let result = self.create_index(create_index_statement)?;

                    eval_results.push(EvalResult::CreateTable {
                        success: true,
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

        Ok(eval_results)
    }

    pub fn create_index(
        &mut self,
        create_index_statement: CreateIndexStatement,
    ) -> Result<(), String> {
        let table = match self.tables.get_mut(&create_index_statement.table) {
            Some(value) => value,
            None => return Err("Table not found".to_string()),
        };

        for index in &table.indexes {
            if index.name == create_index_statement.name {
                return Err("Index name already exists".to_string());
            }
        }

        let index = Index {
            expression: create_index_statement.expression,
            unique: create_index_statement.is_unique,
            primary_key: create_index_statement.is_primary_key,
            name: create_index_statement.name,
            tree: std::collections::btree_map::BTreeMap::new(),
            typ: "btreemap".to_string(),
        };

        table.indexes.push(index);

        Ok(())
    }
}

pub fn linearize_expressions(
    where_clause: Option<Expression>,
    expressions: Vec<Expression>,
) -> Vec<Expression> {
    if where_clause == None {
        return expressions.clone();
    }
    if let Some(Expression::Binary(ref bin_exp)) = where_clause {
        if bin_exp.operand.token == Token::Or {
            return expressions.clone();
        }
        if (bin_exp.operand.token) == Token::And {
            let exps = linearize_expressions(Some(*bin_exp.first.clone()), expressions);
            return linearize_expressions(Some(*bin_exp.second.clone()), exps);
        }

        let mut expressions = expressions.clone();
        if let Some(ref exp) = where_clause {
            expressions.push(exp.clone());
        }
        return expressions;
    } else {
        return expressions.clone();
    }
}

pub fn literal_to_memory_cell(token: &TokenContainer) -> Result<SqlValue, String> {
    match SqlValue::from_token(&token.token) {
        Ok(value) => Ok(value),
        Err(err) => Err(err.to_string()),
    }
}
