use byteorder::{BigEndian, ReadBytesExt};

use crate::sql_types::{SqlType, SqlValue};

use super::ast::*;
use serde::{Deserialize, Serialize};
use std::{io::Read, time::Duration};

pub trait Cell {
    fn as_text(&self) -> Result<String, &str>;
    fn as_int(&self) -> Result<i32, &str>;
    fn as_num(&self, typ: SqlType) -> Result<f64, &str>;
    fn as_bool(&self) -> Result<bool, &str>;
    fn equals(&self, other: Self) -> bool;
}
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct ResultColumn {
    pub col_type: SqlType,
    pub name: String,
}

impl From<SqlType> for String {
    fn from(col_type: SqlType) -> Self {
        match col_type {
            SqlType::Char => "Char".to_string(),
            SqlType::Text => "Text".to_string(),
            SqlType::VarChar => "Varchar".to_string(),
            SqlType::SmallInt => "Smallint".to_string(),
            SqlType::Int => "Int".to_string(),
            SqlType::BigInt => "Bigint".to_string(),
            SqlType::Real => "Real".to_string(),
            SqlType::DoublePrecision => "Double Precision".to_string(),
            SqlType::Boolean => "Bool".to_string(),
            SqlType::Null => "Null".to_string(),
            SqlType::Type => "Type".to_string(),
        }
    }
}

impl From<&SqlType> for String {
    fn from(col_type: &SqlType) -> Self {
        match col_type {
            SqlType::Char => "Char".to_string(),
            SqlType::Text => "Text".to_string(),
            SqlType::VarChar => "Varchar".to_string(),
            SqlType::SmallInt => "Smallint".to_string(),
            SqlType::Int => "Int".to_string(),
            SqlType::BigInt => "Bigint".to_string(),
            SqlType::Real => "Real".to_string(),
            SqlType::DoublePrecision => "Double Precision".to_string(),
            SqlType::Boolean => "Bool".to_string(),
            SqlType::Null => "Null".to_string(),
            SqlType::Type => "Type".to_string(),
        }
    }
}

impl std::fmt::Display for SqlType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(String::from(self).as_str())
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
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
    DropTable {
        success: bool,
        time: Duration,
    },
}

pub type ResultColumns = Vec<ResultColumn>;

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize, Default)]
pub struct QueryResults<C> {
    pub columns: ResultColumns,
    pub rows: Vec<Vec<C>>,
}

pub const ERR_TABLE_DOES_NOT_EXIST: &str = "Table does not exist.";
pub const ERR_COLUMN_DOES_NOT_EXIST: &str = "Column does not exist.";
pub const ERR_INVALID_SELECT_ITEM: &str = "Select item is not valid.";
pub const ERR_INVALID_DATA_TYPE: &str = "Invalid data type.";
pub const ERR_MISSING_VALUES: &str = "Missing values.";

pub trait Backend<C> {
    fn create_table(_: CreateTableStatement) -> Result<bool, String>;
    fn insert(_: InsertStatement) -> Result<bool, String>;
    fn select(_: SelectStatement) -> Result<QueryResults<C>, String>;
    fn eval_query(query: String) -> Result<Vec<EvalResult<C>>, String>;
}

pub type MemoryCellData = Vec<u8>;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct MemoryCell {
    pub bytes: MemoryCellData,
}

impl Cell for MemoryCell {
    fn as_int(&self) -> Result<i32, &'static str> {
        let mut rdr = std::io::Cursor::new(&self.bytes);
        match rdr.read_i32::<BigEndian>() {
            Ok(result) => Ok(result),
            Err(_err) => Err("Failed to parse bytes to int32."),
        }
    }

    fn as_num(&self, typ: SqlType) -> Result<f64, &'static str> {
        let text = match SqlValue::decode_type(self, typ) {
            Ok(val) => val.to_string(),
            Err(_) => {
                return Err("Failed to parse bytes to double precision.");
            }
        };
        match text.parse::<f64>() {
            Ok(val) => Ok(val),
            Err(_) => Err("Failed to parse bytes to double precision."),
        }
    }

    fn as_bool(&self) -> Result<bool, &'static str> {
        Ok(self.bytes != vec![0])
    }

    fn as_text(&self) -> Result<String, &'static str> {
        let mut rdr = std::io::Cursor::new(&self.bytes);

        let mut text = "".to_owned();
        match rdr.read_to_string(&mut text) {
            Ok(_) => Ok(text),
            Err(_err) => Err("Failed to parse bytes to String."),
        }
    }

    fn equals(&self, other: Self) -> bool {
        self.bytes == other.bytes
    }
}
