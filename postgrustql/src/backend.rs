use super::ast::*;
use std::time::Duration;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum ColumnType {
    TextType,
    IntType,
    BoolType,
}

pub trait Cell {
    fn as_text(&self) -> Result<String, &str>;
    fn as_int(&self) -> Result<i32, &str>;
    fn as_bool(&self) -> Result<bool, &str>;
    fn equals(&self, other: Self) -> bool;
}

#[derive(Debug, Clone)]
pub struct ResultColumn {
    pub col_type: ColumnType,
    pub name: String,
}

impl From<ColumnType> for String {
    fn from(col_type: ColumnType) -> Self {
        match col_type {
            ColumnType::TextType => "Text".to_string(),
            ColumnType::IntType => "Int".to_string(),
            ColumnType::BoolType => "Bool".to_string(),
        }
    }
}

impl From<&ColumnType> for String {
    fn from(col_type: &ColumnType) -> Self {
        match col_type {
            ColumnType::TextType => "Text".to_string(),
            ColumnType::IntType => "Int".to_string(),
            ColumnType::BoolType => "Bool".to_string(),
        }
    }
}

impl std::fmt::Display for ColumnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(String::from(self).as_str())
    }
}

#[derive(Debug, Clone)]
pub enum EvalResult<C> {
    Select {
        results: QueryResults<C>,
        time: Duration,
    },
    Insert {
        success: bool,
        time: Duration,
    },
    CreateTable {
        success: bool,
        time: Duration,
    },
}

pub type ResultColumns = Vec<ResultColumn>;

#[derive(Debug, Clone)]
pub struct QueryResults<C> {
    pub columns: ResultColumns,
    pub rows: Vec<Vec<C>>,
}

pub const ERR_TABLE_DOES_NOT_EXIST: &'static str = "Table does not exist.";
pub const ERR_COLUMN_DOES_NOT_EXIST: &'static str = "Column does not exist.";
pub const ERR_INVALID_SELECT_ITEM: &'static str = "Select item is not valid.";
pub const ERR_INVALID_DATA_TYPE: &'static str = "Invalid data type.";
pub const ERR_MISSING_VALUES: &'static str = "Missing values.";

pub trait Backend<C> {
    fn create_table(_: CreateTableStatement) -> Result<bool, String>;
    fn insert(_: InsertStatement) -> Result<bool, String>;
    fn select(_: SelectStatement) -> Result<QueryResults<C>, String>;
    fn eval_query(query: String) -> Result<Vec<EvalResult<C>>, String>;
}
