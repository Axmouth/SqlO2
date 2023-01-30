extern crate byteorder;

pub mod memory_store;

use super::ast::*;
use super::backend::*;
use super::lexer::*;
use super::parser::Parser;

use crate::parser::ParsingError;
use crate::sql_types::SqlNumeric;
use crate::sql_types::SqlText;
use crate::{
    backend::MemoryCell,
    sql_types::{SqlType, SqlValue},
};
use instant::Instant;
use std::borrow::Cow;
use std::collections::HashMap;
use test_util::TestSubjectExt;

const ERR_INVALID_CELL: &str = "Invalid Cell";
//TODO:
// const ERR_INVALID_OPERANDS: &str = "Invalid Operands";
const ANONYMOUS_COL_NAME: &str = "?column?";

type SqlRowIter = Box<dyn Iterator<Item = Vec<SqlValue>>>;

pub trait Selectable {
    fn select(&self) -> Result<(SqlRowIter, Vec<SqlType>), String>;
}

pub struct InnerJoinIter {
    
}

pub struct OuterJoinIter {
    
}

pub struct LeftJoinIter {
    
}

pub struct RightJoinIter {
    
}

pub struct RightOuterJoinIter {
    
}

pub struct TableRowsIter {

}

pub struct IndexedTableRowsIter {

}

pub struct SubQueryIter {

}

#[derive(Clone, PartialEq, Eq)]
pub struct ExecutionContext {}

type Thunk = Box<dyn Fn(ExecutionContext) -> Result<(SqlValue, Box<str>), String>>;

#[derive(Clone, PartialEq, Eq)]
pub enum TableContainer<'a> {
    Temp(Box<Table>),
    Concrete(&'a Table),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Index {
    name: String,
    expression: Expression,
    unique: bool,
    primary_key: bool,
    typ: String,
    tree: std::collections::btree_map::BTreeMap<SqlValue, Vec<usize>>,
}

impl Index {
    pub fn add_row(&mut self, table: &Table, row_index: usize) -> Result<(), String> {
        let (thunk, _) = table.compile_cell(row_index, &self.expression)?;
        let (index_value, _) = thunk(ExecutionContext {})?;

        if index_value.is_null() {
            return Err("Violates NOT NULL Constraint".to_string());
        }

        if self.unique && self.tree.get(&index_value).is_some() {
            return Err("Duplicate Value violates UNIQUE Constraint".to_string());
        }

        match self.tree.get_mut(&index_value) {
            Some(row_indexes) => {
                row_indexes.push(row_index);
            }
            None => {
                self.tree.insert(index_value, vec![row_index]);
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
                std::mem::swap(&mut column_exp, &mut value_exp);
            }

            // Neither side is applicable, return None
            if column_exp != self.expression {
                return Ok(None);
            }

            let supported_checks = vec![
                BinaryOperand::Equal,
                BinaryOperand::NotEqual,
                BinaryOperand::GreaterThan,
                BinaryOperand::GreaterThanOrEqual,
                BinaryOperand::LessThan,
                BinaryOperand::LessThanOrEqual,
            ];
            let is_supported = supported_checks.contains(&bin_exp.operand);

            if !is_supported {
                return Ok(None);
            }
            if let Expression::Literal(lit_exp) = value_exp {
                Ok(Some(Expression::Literal(lit_exp)))
            } else {
                eprintln!("Only index checks on literals supported");
                Ok(None)
            }
        } else {
            Ok(None)
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
            rows: Vec::with_capacity(100),
        };

        let (thunk, _) = match new_table.compile_cell(0, &value_exp) {
            Ok(value) => value,
            Err(err) => {
                eprintln!("{err}");
                return Ok(table.clone());
            }
        };

        let (value, _) = thunk(ExecutionContext {})?;

        let mut row_indexes: Vec<usize> = Vec::with_capacity(100);

        match bin_exp.operand {
            BinaryOperand::Equal => {
                if let Some(indexes) = self.tree.get(&value) {
                    row_indexes.append(&mut indexes.clone());
                }
            }
            BinaryOperand::NotEqual => {
                for (key, indexes) in &self.tree {
                    if *key == value {
                        continue;
                    }
                    row_indexes.append(&mut indexes.clone());
                }
            }
            BinaryOperand::LessThan => {
                for (key, indexes) in &self.tree {
                    if key >= &value {
                        break;
                    }
                    row_indexes.append(&mut indexes.clone());
                }
            }
            BinaryOperand::LessThanOrEqual => {
                for (key, indexes) in &self.tree {
                    if key > &value {
                        break;
                    }
                    row_indexes.append(&mut indexes.clone());
                }
            }
            BinaryOperand::GreaterThan => {
                for (_, ref mut indexes) in self.tree.clone().split_off(&value) {
                    row_indexes.append(indexes);
                }
            }
            BinaryOperand::GreaterThanOrEqual => {
                if let Some(indexes) = self.tree.get(&value) {
                    row_indexes.append(&mut indexes.clone());
                }
                for (_, ref mut indexes) in self.tree.clone().split_off(&value) {
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

        Ok(table.clone())
    }
}

#[derive(Clone, PartialEq, Debug, Eq)]
pub struct Table {
    name: String,
    columns: Vec<String>,
    column_types: Vec<SqlType>,
    rows: Vec<Vec<SqlValue>>,
    indexes: Vec<Index>,
}

impl From<QueryResults<SqlValue>> for Table {
    fn from(results: QueryResults<SqlValue>) -> Self {
        Self {
            column_types: results.columns.iter().map(|c| c.col_type).collect(),
            name: String::from(""),
            columns: results.columns.iter().map(|c| c.name.clone()).collect(),
            indexes: vec![],
            rows: results.rows,
        }
    }
}

impl Table {
    pub fn evaluate_literal_cell<'a>(
        &'a self,
        row_index: usize,
        expression: &'a Expression,
    ) -> Result<(Thunk, SqlType), String> {
        match expression {
            Expression::Literal(literal_expression) => match &literal_expression {
                LiteralExpression::Identifier(value) => {
                    for (i, table_col) in self.columns.iter().enumerate() {
                        if table_col == value.as_str() {
                            let table_col = table_col.clone();
                            let typ = self.column_types.get(i).ok_or("Error accesing column")?;
                            let thunk = Box::new(move |_| {
                                let val = self
                                    .rows
                                    .get(row_index as usize)
                                    .ok_or("Error accesing row")?
                                    .get(i as usize)
                                    .ok_or("Error accesing row's column")?;

                                Ok((val.clone(), table_col.clone().into_boxed_str()))
                            });
                            return Ok((thunk, *typ));
                        }
                    }

                    Err(format!("{value}: {ERR_COLUMN_DOES_NOT_EXIST}"))
                }
                LiteralExpression::Numeric(value) => {
                    let typ = SqlType::DoublePrecision;
                    let val = SqlValue::Numeric(SqlNumeric::DoublePrecision {
                        value: value.parse::<f64>().map_err(|e| e.to_string())?,
                    });
                    let thunk: Thunk = Box::new(move |_| Ok((val.clone(), "".into())));
                    Ok((thunk, typ))
                }
                LiteralExpression::String(value) => {
                    let typ = SqlType::Text;
                    let val = SqlValue::Text(SqlText::Text { value: value.clone() });

                    let thunk: Thunk = Box::new(move |_| Ok((val.clone(), "".into())));
                    Ok((thunk, typ))
                }
                LiteralExpression::Bool(value) => {
                    let typ = SqlType::Boolean;
                    let val = SqlValue::Boolean(*value);
                    let thunk: Thunk = Box::new(move |_| Ok((val.clone(), "".into())));
                    Ok((thunk, typ))
                }
                LiteralExpression::Null => {
                    let typ = SqlType::Null;
                    let val = SqlValue::Null;
                    let thunk: Thunk = Box::new(move |_| Ok((val.clone(), "".into())));
                    Ok((thunk, typ))
                }
            },
            Expression::TableColumn(table_column) => {
                for (i, table_col) in self.columns.iter().cloned().enumerate() {
                    if table_col == table_column.col_name {
                        let typ = self.column_types.get(i).ok_or("Error accesing column")?;
                        let thunk: Thunk = Box::new(move |_| {
                            let val = self
                                .rows
                                .get(row_index)
                                .ok_or("Error accesing row")?
                                .get(i)
                                .ok_or("Error accesing row's column")?;

                            Ok((val.clone(), table_col.clone().into_boxed_str()))
                        });
                        return Ok((thunk, *typ));
                    }
                }

                Err(format!(
                    "{}: {ERR_COLUMN_DOES_NOT_EXIST}",
                    table_column.col_name
                ))
            }
            Expression::ProcessedTableColumn(table_column) => {
                let table_col = self
                    .columns
                    .get(table_column.col_idx)
                    .ok_or(ERR_COLUMN_DOES_NOT_EXIST)?.clone();
                let typ = self
                    .column_types
                    .get(table_column.col_idx)
                    .ok_or(ERR_COLUMN_DOES_NOT_EXIST)?;
                let thunk = Box::new(move |_| {
                let val = self
                    .rows
                    .get(row_index)
                    .ok_or("Error accesing row")?
                    .get(table_column.col_idx)
                    .ok_or("Error accesing row's column")?;

                    Ok((val.clone(), table_col.clone().into_boxed_str()))
                });

                Ok((thunk, *typ))
            }
            _ => Err(ERR_INVALID_CELL.to_string()),
        }
    }

    pub fn evaluate_binary_cell<'a>(
        &'a self,
        row_index: usize,
        expression: &'a Expression,
    ) -> Result<(Thunk, SqlType), String> {
        match expression {
            Expression::Binary(binary_expression) => {

                macro_rules! bin_thunk {
                    ($first_thunk:ident, $second_thunk:ident, $func:ident) => {
                        Box::new(move |ctx| {
                            let (first_val, _) = $first_thunk(ctx.clone())?;
                            let (second_val, _) = $second_thunk(ctx)?;

                            Ok((SqlValue::$func(&first_val, &second_val)?, ANONYMOUS_COL_NAME.into()))
                        })
                    };
                }

                let (first_thunk, typ1) = self.compile_cell(row_index, &binary_expression.first)?;

                let (second_thunk, typ2) = self.compile_cell(row_index, &binary_expression.second)?;

                let thunk: Thunk = match binary_expression.operand {
                    BinaryOperand::Equal => {
                        bin_thunk!(first_thunk, second_thunk, equals)
                    }
                    BinaryOperand::NotEqual => {
                        bin_thunk!(first_thunk, second_thunk, not_equal)
                    }
                    BinaryOperand::GreaterThan => {
                        bin_thunk!(first_thunk, second_thunk, greater_than)
                    }
                    BinaryOperand::GreaterThanOrEqual => {
                        bin_thunk!(first_thunk, second_thunk, greater_than_or_equals)
                    }
                    BinaryOperand::LessThan => {
                        bin_thunk!(first_thunk, second_thunk, less_than)
                    }
                    BinaryOperand::LessThanOrEqual => {
                        bin_thunk!(first_thunk, second_thunk, less_than_or_equals)
                    }
                    BinaryOperand::Concat => {
                        bin_thunk!(first_thunk, second_thunk, concat)
                    }
                    BinaryOperand::Add => {
                        bin_thunk!(first_thunk, second_thunk, add)
                    }
                    BinaryOperand::Subtract => {
                        bin_thunk!(first_thunk, second_thunk, subtract)
                    }
                    BinaryOperand::Multiply => {
                        bin_thunk!(first_thunk, second_thunk, multiply)
                    }
                    BinaryOperand::Divide => {
                        bin_thunk!(first_thunk, second_thunk, divide)
                    }
                    BinaryOperand::Modulo => {
                        bin_thunk!(first_thunk, second_thunk, modulo)
                    }
                    BinaryOperand::And => {
                        bin_thunk!(first_thunk, second_thunk, and)
                    }
                    BinaryOperand::Or => {
                        bin_thunk!(first_thunk, second_thunk, or)
                    }
                    BinaryOperand::Exponentiation => {
                        bin_thunk!(first_thunk, second_thunk, exponentiation)
                    }
                    BinaryOperand::BitwiseAnd => {
                        bin_thunk!(first_thunk, second_thunk, bitwise_and)
                    }
                    BinaryOperand::BitwiseOr => {
                        bin_thunk!(first_thunk, second_thunk, bitwise_or)
                    }
                    BinaryOperand::BitwiseXor => {
                        bin_thunk!(first_thunk, second_thunk, bitwise_xor)
                    }
                    BinaryOperand::BitwiseShiftLeft => {
                        bin_thunk!(first_thunk, second_thunk, bitwise_shift_left)
                    }
                    BinaryOperand::BitwiseShiftRight => {
                        bin_thunk!(first_thunk, second_thunk, bitwise_shift_right)
                    }
                    BinaryOperand::In => todo!(),
                    BinaryOperand::NotIn => todo!(),
                    BinaryOperand::Like => todo!(),
                    BinaryOperand::NotLike => todo!(),
                    BinaryOperand::Between => todo!(),
                    BinaryOperand::NotBetween => todo!(),
                    BinaryOperand::BitwiseShiftRightZeroFill => todo!(),
                };
                
                let typ = typ1;
                Ok((thunk, typ))
            }
            Expression::Unary(unary_expression) => {
                let (thunk, typ) = self.compile_cell(row_index, &unary_expression.first)?;

                macro_rules! un_thunk {
                    ($thunk:ident, $func:ident) => {
                        Box::new(move |ctx| {
                            let (val, _) = $thunk(ctx)?;
                            Ok((SqlValue::$func(&val)?, ANONYMOUS_COL_NAME.into()))
                        })
                    };
                }

                let thunk: Thunk = match unary_expression.operand {
                    UnaryOperand::Minus => {
                        un_thunk!(thunk, minus)
                    }
                    UnaryOperand::SquareRoot => {
                        un_thunk!(thunk, square_root)
                    }
                    UnaryOperand::CubeRoot => {
                        un_thunk!(thunk, cube_root)
                    }
                    UnaryOperand::Factorial | UnaryOperand::FactorialPrefix => {
                        un_thunk!(thunk, factorial)
                    }
                    UnaryOperand::Not => {
                        un_thunk!(thunk, not)
                    }
                    UnaryOperand::AbsoluteValue => {
                        un_thunk!(thunk, abs)
                    }
                    UnaryOperand::BitwiseNot => {
                        un_thunk!(thunk, bitwise_not)
                    }
                    UnaryOperand::Exists => todo!(),
                    UnaryOperand::NotExists => todo!(),
                    UnaryOperand::IsNull => todo!(),
                    UnaryOperand::IsNotNull => todo!(),
                };
                Ok((thunk, typ))
            }
            Expression::Cast { data, typ } => {
                let (thunk, _) = self.compile_cell(row_index, data)?;
                let thunk = Box::new(move |ctx| {
                    let (val, _) = thunk(ctx)?;
                    Ok((val.explicit_cast_to_type(*typ)?, ANONYMOUS_COL_NAME.into()))
                });
                Ok((thunk, *typ))
            }
            _ => Err(ERR_INVALID_CELL.to_string()),
        }
    }

    pub fn compile_cell(
        &self,
        row_index: usize,
        expression: &Expression,
    ) -> Result<(Thunk, SqlType), String> {
        match expression {
            Expression::Literal(_)
            | Expression::TableColumn(_)
            | Expression::ProcessedTableColumn(_) => {
                self.evaluate_literal_cell(row_index, expression)
            }
            Expression::Binary(_) | Expression::Unary(_) | Expression::Cast { data: _, typ: _ } => {
                self.evaluate_binary_cell(row_index, expression)
            }
            Expression::SubSelect(select_statement) => {
                if select_statement.items.len() != 1 {
                    return Err("Subquery must return only one column".to_string());
                }

                if let Some(item) = select_statement.items.get(0) {
                    let (result, typ) = self.compile_cell(row_index, &item.expression)?;
                    Ok((result, ANONYMOUS_COL_NAME, typ))
                } else {
                    Err("Subquery must return only one column".to_string())
                };
                let (where_thunk, where_typ): (Thunk, SqlType) = if Expression::Empty != select_statement.where_clause {
                    self.compile_cell(row_index, &select_statement.where_clause)?
                } else {
                    (Box::new(move |ctx| {
                        Ok((SqlValue::Boolean(true), ANONYMOUS_COL_NAME.into()))
                    }), SqlType::Boolean)
                };
                if where_typ != SqlType::Boolean {
                    return Err("Subquery WHERE clause must return a boolean".to_string());
                }

                if let Some(item) = select_statement.items.get(0) {
                    let (thunk, typ) = self.compile_cell(row_index, &item.expression)?;
                    let thunk = Box::new(move |ctx: ExecutionContext| {
                        let (where_val, _) = where_thunk(ctx.clone())?;
                        if let SqlValue::Boolean(true) = where_val {
                            thunk(ctx)
                        } else if let SqlValue::Boolean(true) = where_val {
                            Ok((SqlValue::Null, ANONYMOUS_COL_NAME.into()))
                        } else {
                            Err(ERR_INVALID_CELL.to_string())
                        }
                    });
                    Ok((thunk, typ))
                } else {
                    Err("Subquery must return only one column".to_string())
                }
            }
            Expression::Empty => Err(ERR_INVALID_CELL.to_string()),
        }
    }

    pub fn get_applicable_indexes(
        &self,
        where_clause: Option<&Expression>,
    ) -> Result<Vec<(&Index, Expression)>, String> {
        let exps = linearize_expressions(where_clause.cloned(), vec![]);

        let mut indexes_and_expressions = vec![];
        for exp in &exps {
            for index in &self.indexes {
                if index.applicable_value(exp)?.is_some() {
                    let index_and_expression = (index, exp.clone());
                    indexes_and_expressions.push(index_and_expression);
                }
            }
        }

        Ok(indexes_and_expressions)
    }
}

#[derive(PartialEq, Eq, Default)]
pub struct MemoryBackend {
    tables: HashMap<String, Table>,
    parser: Parser,
}

impl TestSubjectExt for MemoryBackend {
    fn init() -> Self {
        Self::new()
    }
}

pub fn get_true_mem_cell() -> MemoryCell {
    MemoryCell { bytes: vec![1] }
}
pub fn get_false_mem_cell() -> MemoryCell {
    MemoryCell { bytes: vec![0] }
}
pub fn get_true_lex_token() -> TokenContainer<'static> {
    TokenContainer {
        token: Token::BoolValue { value: true },
        loc: TokenLocation { line: 0, col: 0 },
    }
}
pub fn get_false_lex_token() -> TokenContainer<'static> {
    TokenContainer {
        token: Token::BoolValue { value: false },
        loc: TokenLocation { line: 0, col: 0 },
    }
}

impl MemoryBackend {
    pub fn new() -> MemoryBackend {
        Self {
            tables: HashMap::new(),
            parser: Parser::new(),
        }
    }
    pub fn parse<'a>(&'a self, sql: &'a str) -> Result<Ast, ParsingError> {
        self.parser.parse(sql)
    }

    pub fn create_table(&mut self, create_statement: CreateTableStatement) -> Result<bool, String> {
        let mut new_table = Table {
            name: create_statement.name.clone(),
            columns: vec![],
            column_types: vec![],
            rows: vec![],
            indexes: vec![],
        };

        if create_statement.cols.is_empty() {
            return Err("No Table Columns.".to_owned());
        }

        if self.tables.get(&create_statement.name).is_some() {
            return Err(format!(
                "Table \"{}\" already exists.",
                create_statement.name
            ));
        }

        let mut primary_key: Option<Expression> = None;

        for col in create_statement.cols {
            new_table.columns.push(col.name.clone());

            let data_type = col.data_type;

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
            if let Err(err) = self.create_index(CreateIndexStatement {
                table: create_statement.name.clone(),
                name: format!("{}_pkey", create_statement.name),
                is_unique: true,
                is_primary_key: true,
                expression: primary_key,
            }) {
                self.tables.remove(&create_statement.name);
                return Err(err);
            }
        }

        Ok(true)
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

                    let cell = literal_to_memory_cell(value)?.to_type(*typ)?;
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
            let (index_thunk, _) = table.compile_cell(row_index, &index.expression)?;

            let (index_value, _) = index_thunk(ExecutionContext {  })?;

            if index_value.is_null() {
                table.rows.remove(row_index);
                return Err("Violates NOT NULL Constraint".to_string());
            }

            if index.unique && index.tree.get(&index_value).is_some() {
                table.rows.remove(row_index);
                return Err("Duplicate Value violates UNIQUE Constraint".to_string());
            }
            let index = match table.indexes.get_mut(i) {
                None => {
                    return Err("Index not found".to_string());
                }
                Some(value) => value,
            };

            match index.tree.get_mut(&index_value) {
                Some(row_indexes) => {
                    row_indexes.push(row_index);
                }
                None => {
                    table.indexes[i].tree.insert(index_value, vec![row_index]);
                }
            }
        }

        Ok(true)
    }

    pub fn select(
        &self,
        select_statement: SelectStatement,
    ) -> Result<QueryResults<SqlValue>, String> {
        let mut tables: HashMap<String, TableContainer> = HashMap::new();

        let mut results: Vec<Vec<SqlValue>> = Vec::with_capacity(100);
        let mut results_order: Vec<SqlValue> = Vec::with_capacity(100);
        let mut offset = 0;

        let mut columns: ResultColumns = Vec::with_capacity(10);

        if select_statement.items.is_empty() {
            return Ok(QueryResults {
                columns,
                rows: results,
            });
        }

        let mut table_joins = &vec![];
        let (table_name, mut table) = match select_statement.from.get(0) {
            Some(RowDataSource::Table {
                as_clause,
                table_name: ref from_name,
                joins,
            }) => match self.tables.get(from_name) {
                // TODO
                None => {
                    return Err(ERR_TABLE_DOES_NOT_EXIST.to_string());
                }
                Some(table) => {
                    let from_name = if let Some(from_name) = as_clause {
                        from_name.clone()
                    } else {
                        from_name.clone()
                    };

                    let mut new_table = TableContainer::Concrete(table);
                    for (index, exp) in
                        table.get_applicable_indexes(Some(&select_statement.where_clause))?
                    {
                        if let Expression::Binary(bin_exp) = exp {
                            new_table = TableContainer::Temp(Box::new(
                                index.new_table_from_subset(table, bin_exp)?,
                            ));
                        }
                    }
                    table_joins = joins;
                    (from_name, new_table)
                }
            },
            Some(RowDataSource::SubSelect {
                as_clause,
                select,
                joins,
            }) => {
                // TODO
                let result = self.select(select.clone())?;
                let new_table = Table::from(result);
                table_joins = joins;
                (as_clause.clone(), TableContainer::Temp(Box::new(new_table)))
            }
            None => {
                let mut new_table = Table {
                    column_types: Vec::with_capacity(10),
                    columns: Vec::with_capacity(10),
                    indexes: Vec::with_capacity(10),
                    name: "".to_string(),
                    rows: Vec::with_capacity(1),
                };
                new_table.rows.push(vec![]);
                ("".to_string(), TableContainer::Temp(Box::new(new_table)))
            }
        };
        if let Some(JoinClause { on, source, kind }) = table_joins.get(0) {
            let (_, source_table) = match source {
                RowDataSource::Table {
                    as_clause,
                    table_name: ref from_name,
                    joins: _,
                } => match self.tables.get(from_name) {
                    // TODO
                    None => {
                        return Err(ERR_TABLE_DOES_NOT_EXIST.to_string());
                    }
                    Some(table) => {
                        let from_name = if let Some(from_name) = as_clause {
                            from_name.clone()
                        } else {
                            from_name.clone()
                        };

                        let mut new_table = TableContainer::Concrete(table);
                        for (index, exp) in
                            table.get_applicable_indexes(Some(&select_statement.where_clause))?
                        {
                            if let Expression::Binary(bin_exp) = exp {
                                new_table = TableContainer::Temp(Box::new(
                                    index.new_table_from_subset(table, bin_exp)?,
                                ));
                            }
                        }
                        // table_joins = joins;
                        (from_name, new_table)
                    }
                },
                RowDataSource::SubSelect {
                    as_clause,
                    select,
                    joins: _,
                } => {
                    // TODO
                    let result = self.select(select.clone())?;
                    let new_table = Table::from(result);
                    // table_joins = joins;
                    (as_clause.clone(), TableContainer::Temp(Box::new(new_table)))
                }
            };
            let &mut rows;
            let temp;
            let (columns, column_types) = match table {
                TableContainer::Concrete(table) => {
                    rows = &table.rows;
                    (table.columns.clone(), table.column_types.clone())
                }
                TableContainer::Temp(table) => {
                    temp = table.rows;
                    rows = &temp;
                    (table.columns.clone(), table.column_types.clone())
                }
            };

            let &mut source_rows;
            let temp;
            let (mut source_columns, mut source_column_types) = match source_table {
                TableContainer::Concrete(table) => {
                    source_rows = &table.rows;
                    (table.columns.clone(), table.column_types.clone())
                }
                TableContainer::Temp(table) => {
                    temp = table.rows;
                    source_rows = &temp;
                    (table.columns.clone(), table.column_types.clone())
                }
            };
            let source_columns_num = source_columns.len();
            let on_columns_num = columns.len();
            let mut full_derp_table = Table {
                column_types,
                columns,
                indexes: vec![],
                name: "".to_string(),
                rows: Vec::with_capacity(100),
            };
            full_derp_table.columns.append(&mut source_columns);
            full_derp_table
                .column_types
                .append(&mut source_column_types);
            let mut temp_table = full_derp_table.clone();
            // TODO nested loop through tables, temp table with only the current row for each loop, run expression, rename cols if needed

            let mut used_source_indices = vec![];
            let mut used_on_indices = vec![];
            for (source_index, source_row) in source_rows.iter().enumerate() {
                for (on_index, row) in rows.iter().enumerate() {
                    let mut new_row = row.clone();
                    new_row.append(&mut source_row.clone());
                    temp_table.rows = vec![new_row.clone()];
                    let (thunk, _) = temp_table.compile_cell(0, on)?;
                    let (result, _) = thunk(ExecutionContext {  })?;

                    if let SqlValue::Boolean(true) = result {
                        used_source_indices.push(source_index);
                        used_on_indices.push(on_index);
                        full_derp_table.rows.push(new_row);
                    } else if let SqlValue::Boolean(false) = result {
                        continue;
                    } else {
                        return Err("Invalid Join Expression".to_string());
                    }
                }
            }

            if kind == &JoinKind::RightOuter || kind == &JoinKind::FullOuter {
                let start = 0;
                let end = source_columns_num;
                for (source_index, source_row) in source_rows.iter().enumerate() {
                    if !used_source_indices.contains(&source_index) {
                        let mut new_row = vec![];
                        for _ in start..end {
                            new_row.push(SqlValue::Null);
                        }
                        new_row.append(&mut source_row.clone());
                        full_derp_table.rows.push(new_row);
                    }
                }
            }
            if kind == &JoinKind::LeftOuter || kind == &JoinKind::FullOuter {
                let start = source_columns_num;
                let end = source_columns_num + on_columns_num;
                for (on_index, on_row) in rows.iter().enumerate() {
                    if !used_on_indices.contains(&on_index) {
                        let mut new_row = on_row.clone();
                        for _ in start..end {
                            new_row.push(SqlValue::Null);
                        }
                        full_derp_table.rows.push(new_row);
                    }
                }
            }

            table = TableContainer::Temp(Box::new(full_derp_table));
        }
        tables.insert(table_name, table);

        let mut final_select_items: Vec<SelectItem> = Vec::with_capacity(10);
        for item in select_statement.items {
            // TODO when multiple tables in join have same column name, rename them to table.column
            if item.asterisk {
                for table in tables.values() {
                    let table = match table {
                        TableContainer::Concrete(table) => table,
                        TableContainer::Temp(table) => table.as_ref(),
                    };
                    let mut new_select_items: Vec<SelectItem> = Vec::with_capacity(10);
                    for (idx, column) in table.columns.iter().enumerate() {
                        let new_select_item = SelectItem {
                            expression: Expression::ProcessedTableColumn(ProcessedTableColumn {
                                col_name: Some(column.to_string()),
                                col_idx: idx,
                            }),
                            as_clause: None,
                            asterisk: false,
                        };
                        new_select_items.push(new_select_item);
                    }
                    final_select_items.append(&mut new_select_items);
                }
            } else {
                for table in tables.values() {
                    let table = match table {
                        TableContainer::Concrete(table) => table,
                        TableContainer::Temp(table) => table.as_ref(),
                    };
                    if let Expression::TableColumn(TableColumn {
                        ref col_name,
                        table_name: Some(ref table_name),
                    }) = item.expression
                    {
                        if table_name != &table.name {
                            continue;
                        }
                        for (idx, column) in table.columns.iter().enumerate() {
                            if column != col_name {
                                continue;
                            }
                            let new_select_item = SelectItem {
                                expression: Expression::ProcessedTableColumn(
                                    ProcessedTableColumn {
                                        col_name: Some(column.to_string()),
                                        col_idx: idx,
                                    },
                                ),
                                as_clause: None,
                                asterisk: false,
                            };
                            final_select_items.push(new_select_item.clone());
                            break;
                        }
                        break;
                    } else {
                        final_select_items.push(item.clone());
                    }
                }
            }
        }

        let table_name = if let Some(RowDataSource::Table {
            as_clause: _,
            table_name,
            joins: _,
        }) = select_statement.from.get(0)
        {
            // TODO
            table_name.clone()
        } else if let Some(RowDataSource::SubSelect {
            as_clause,
            select: _,
            joins: _,
        }) = select_statement.from.get(0)
        {
            // TODO
            as_clause.clone()
        // return Err("Not implemented subquery for FROM, yet..".to_string());
        } else {
            "".to_string()
        };
        let table = match &tables.get(&table_name) {
            Some(TableContainer::Concrete(table)) => table,
            Some(TableContainer::Temp(table)) => table.as_ref(),
            None => return Err(format!("Table {table_name} not found")),
        };

        for row_index in 0..table.rows.len() {
            if let Some(limit) = select_statement.limit {
                if results.len() >= limit {
                    break;
                }
            }
            let mut result: Vec<SqlValue> = vec![];
            let is_first_row = results.is_empty();

            match &select_statement.where_clause {
                Expression::Empty => {}
                _ => {
                    let (cell_thunk, _) =
                        table.compile_cell(row_index, &select_statement.where_clause)?;
                    
                    let (cell_val, _) = cell_thunk(ExecutionContext {  })?;

                    if let SqlValue::Boolean(true) = cell_val {
                    } else {
                        continue;
                    }
                }
            }
            if let Some(target_offset) = select_statement.offset {
                offset += 1;
                if offset <= target_offset {
                    continue;
                }
            }

            for select_item in &final_select_items {
                let (cell_thunk, col_type) =
                    table.compile_cell(row_index, &select_item.expression)?;
                
                let (cell_val, col_name) = cell_thunk(ExecutionContext {  })?;

                if is_first_row {
                    match &select_item.as_clause {
                        Some(as_name) => {
                            columns.push(ResultColumn {
                                col_type,
                                name: as_name.clone(),
                            });
                        }
                        None => {
                            columns.push(ResultColumn {
                                col_type,
                                name: col_name.to_string(),
                            });
                        }
                    }
                }

                result.push(cell_val);
            }

            if select_statement.is_distinct && results.contains(&result) {
                continue;
            }

            if let Some(ref order_by) = select_statement.order_by {
                let (new_ord_thunk, _) = table.compile_cell(row_index, &order_by.exp)?;
                let (new_ord_val, _) = new_ord_thunk(ExecutionContext {  })?;

                let mut index = results_order.len();
                for (i, val) in results_order.iter().enumerate() {
                    if (order_by.asc && new_ord_val < *val) || (!order_by.asc && new_ord_val > *val)
                    {
                        index = i;
                        break;
                    }
                }
                results_order.insert(index, new_ord_val);
                results.insert(index, result);
                continue;
            }

            results.push(result);
        }

        Ok(QueryResults {
            columns,
            rows: results,
        })
    }

    pub fn drop_table(&mut self, drop_table_statement: DropTableStatement) -> Result<bool, String> {
        match self.tables.get(&drop_table_statement.name) {
            None => Err(format!(
                "Table \"{}\" doesn't exist.",
                drop_table_statement.name.clone()
            )),
            Some(_) => {
                self.tables.remove(&drop_table_statement.name);
                Ok(true)
            }
        }
    }

    pub fn eval_query(&mut self, query: &str) -> Result<Vec<EvalResult<SqlValue>>, String> {
        let mut before = Instant::now();
        let ast = match self.parser.parse(query) {
            Ok(val) => val,
            Err(err) => return Err(err.to_string()),
        };

        let mut eval_results = Vec::new();

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
                    self.create_index(create_index_statement)?;

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
                Statement::DropTableStatement(drop_table_statement) => {
                    let result = self.drop_table(drop_table_statement)?;
                    eval_results.push(EvalResult::DropTable {
                        success: result,
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
        return expressions;
    }
    if let Some(Expression::Binary(ref bin_exp)) = where_clause {
        if bin_exp.operand == BinaryOperand::Or {
            return expressions;
        }
        if (bin_exp.operand) == BinaryOperand::And {
            let exps = linearize_expressions(Some(*bin_exp.first.clone()), expressions);
            return linearize_expressions(Some(*bin_exp.second.clone()), exps);
        }

        let mut expressions = expressions;
        if let Some(ref exp) = where_clause {
            expressions.push(exp.clone());
        }
        expressions
    } else {
        expressions
    }
}

pub fn literal_to_memory_cell(literal: &LiteralExpression) -> Result<SqlValue, String> {
    match literal {
        LiteralExpression::Numeric(value) => {
            let val = SqlValue::Numeric(SqlNumeric::DoublePrecision {
                value: value.parse::<f64>().map_err(|e| e.to_string())?,
            });
            Ok(val)
        }
        LiteralExpression::String(value) => {
            let val = SqlValue::Text(SqlText::Text {
                value: value.clone(),
            });
            Ok(val)
        }
        LiteralExpression::Bool(value) => {
            let val = SqlValue::Boolean(*value);
            Ok(val)
        }
        LiteralExpression::Null => {
            let val = SqlValue::Null;
            Ok(val)
        }
        _ => Err("Unsupported literal type".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod eval_query {
        use super::*;
        use instant::Duration;
        use pretty_assertions::assert_eq;
        use test_macros::test_case;

        #[test_case(
            "Select 1",
            Ok(vec![EvalResult::Select {
                results: QueryResults {
                    columns: vec![ResultColumn {
                        col_type: SqlType::DoublePrecision,
                        name: "".to_string(),
                    }],
                    rows: vec![vec![SqlValue::Numeric(SqlNumeric::DoublePrecision {
                        value: 1.0,
                    })]]
                },
                time: std::time::Duration::new(0, 0),
            }])
        )]
        fn eval_query_test(query: &str, expected: Result<Vec<EvalResult<SqlValue>>, String>) {
            let mut db = MemoryBackend::new();

            let result = db.eval_query(query);

            assert_eq!(result, expected);
        }

        #[test_case(
            &[
            "CREATE TABLE test (id INT, name TEXT, age INT);",
            "INSERT INTO test VALUES (1, 'John', 20);"
            ],
            vec![
                Ok(
                    vec![
                        EvalResult::CreateTable {
                            success: true,
                            time: Duration::new(0, 0),
                        },
                    ],
                ),
                Ok(
                    vec![
                        EvalResult::Insert {
                            success: true,
                            time: Duration::new(0, 0),
                        },
                    ],
                ),]
        )]
        #[test_case(
            &[
            "CREATE TABLE test (id INT, name TEXT, age INT);",
            "INSERT INTO test VALUES (1, 'John', 20);",
            "SELECT * FROM test WHERE id = 1;"
            ],
            vec![
                Ok(
                    vec![
                        EvalResult::CreateTable {
                            success: true,
                            time: Duration::new(0, 0),
                        },
                    ],
                ),
                Ok(
                    vec![
                        EvalResult::Insert {
                            success: true,
                            time: Duration::new(0, 0),
                        },
                    ],
                ),
                Ok(
                    vec![
                        EvalResult::Select {
                            results: QueryResults {
                                columns: vec![
                                    ResultColumn {
                                        col_type: SqlType::Int,
                                        name: "id".to_string(),
                                    },
                                    ResultColumn {
                                        col_type: SqlType::Text,
                                        name: "name".to_string(),
                                    },
                                    ResultColumn {
                                        col_type: SqlType::Int,
                                        name: "age".to_string(),
                                    },
                                ],
                                rows: vec![
                                    vec![
                                        SqlValue::Numeric(
                                            SqlNumeric::Int {
                                                value: 1,
                                            },
                                        ),
                                        SqlValue::Text(
                                            SqlText::Text {
                                                value: "John".to_string(),
                                            },
                                        ),
                                        SqlValue::Numeric(
                                            SqlNumeric::Int {
                                                value: 20,
                                            },
                                        ),
                                    ],
                                ],
                            },
                            time: Duration::new(0, 0),
                        },
                    ],
                ),
            ]
        )]
        fn eval_queries_test(
            queries: &[&str],
            expected: Vec<Result<Vec<EvalResult<SqlValue>>, String>>,
        ) {
            let mut db = MemoryBackend::new();

            let results = queries
                .iter()
                .map(|query| db.eval_query(query))
                .collect::<Vec<_>>();

            assert_eq!(results, expected);
        }
    }
}
