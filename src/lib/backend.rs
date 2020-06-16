use super::parser::*;

#[derive(Clone, Eq, PartialEq)]
pub enum ColumnType {
    TextType,
    IntType,
}

pub trait Cell {
    fn as_text(&self) -> Result<String, &str>;
    fn as_int(&self) -> Result<i32, &str>;
}

pub struct ResultColumn {
    pub col_type: ColumnType,
    pub name: String,
}

pub type ResultColumns = Vec<ResultColumn>;

pub struct QueryResults {
    pub columns: ResultColumns,
    pub rows: Vec<Vec<Box<dyn Cell>>>,
}

pub const ERR_TABLE_DOES_NOT_EXIST: &'static str = "Table does not exist.";
pub const ERR_COLUMN_DOES_NOT_EXIST: &'static str = "Column does not exist.";
pub const ERR_INVALID_SELECT_ITEM: &'static str = "Select item is not valid.";
pub const ERR_INVALID_DATA_TYPE: &'static str = "Invalid data type.";
pub const ERR_MISSING_VALUES: &'static str = "Missing values.";

trait Backend {
    fn create_table(_: CreateTableStatement) -> Result<bool, String>;
    fn insert(_: InsertStatement) -> Result<bool, String>;
    fn select(_: SelectStatement) -> Result<QueryResults, String>;
}
