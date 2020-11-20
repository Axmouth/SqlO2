use byteorder::{BigEndian, ReadBytesExt};
use std::convert::{TryFrom, TryInto};
use std::io::Read;

use crate::{
    backend::{Cell, MemoryCell, MemoryCellData, ERR_INVALID_DATA_TYPE},
    lexer::Token,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum SqlType {
    SmallInt,
    Int,
    BigInt,
    Real,
    DoublePrecision,
    Text,
    Char,
    VarChar,
    Boolean,
    Null,
}

impl SqlType {
    pub fn from_token(token: Token) -> Result<Self, SqlTypeError> {
        match token {
            Token::Int => Ok(SqlType::Int),
            Token::Text => Ok(SqlType::Text),
            Token::Bool => Ok(SqlType::Boolean),
            _ => Err(SqlTypeError::ConversionError(
                ERR_INVALID_DATA_TYPE.to_string(),
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum SqlTypeError {
    ConversionError(String),
    ParseError(String),
    DecodeError(String),
    TypeMismatchError(String),
    OverflowError(String),
    OperationError(String),
    Infallible,
}

impl ToString for SqlTypeError {
    fn to_string(&self) -> String {
        match self {
            SqlTypeError::ConversionError(msg) => msg.clone(),
            SqlTypeError::ParseError(msg) => msg.clone(),
            SqlTypeError::DecodeError(msg) => msg.clone(),
            SqlTypeError::TypeMismatchError(msg) => msg.clone(),
            SqlTypeError::OverflowError(msg) => msg.clone(),
            SqlTypeError::OperationError(msg) => msg.clone(),
            SqlTypeError::Infallible => "wut".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum SqlValue {
    Null,
    Text(SqlText),
    Numeric(SqlNumeric),
    Boolean(bool),
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum SqlNumeric {
    SmallInt { value: i16 },
    Int { value: i32 },
    BigInt { value: i64 },
    Real { value: f32 },
    DoublePrecision { value: f64 },
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum SqlText {
    Char {
        value: String,
        len: usize,
    },
    VarChar {
        value: String,
        maxlen: usize,
        len: usize,
    },
    Text {
        value: String,
    },
}

impl SqlValue {
    pub fn is_numeric(&self) -> bool {
        if let SqlValue::Numeric(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_null(&self) -> bool {
        if let SqlValue::Null = self {
            true
        } else {
            false
        }
    }

    pub fn is_text(&self) -> bool {
        if let SqlValue::Text(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_bool(&self) -> bool {
        if let SqlValue::Boolean(_) = self {
            true
        } else {
            false
        }
    }

    pub fn convert_to_matching_types(
        a: SqlValue,
        b: SqlValue,
    ) -> Result<(SqlValue, SqlValue), SqlTypeError> {
        match (a, b) {
            (SqlValue::Numeric(num1), SqlValue::Numeric(num2)) => match (num1, num2) {
                (SqlNumeric::Int { value: _ }, SqlNumeric::Int { value: _ })
                | (SqlNumeric::SmallInt { value: _ }, SqlNumeric::SmallInt { value: _ })
                | (SqlNumeric::BigInt { value: _ }, SqlNumeric::BigInt { value: _ })
                | (SqlNumeric::Real { value: _ }, SqlNumeric::Real { value: _ })
                | (
                    SqlNumeric::DoublePrecision { value: _ },
                    SqlNumeric::DoublePrecision { value: _ },
                ) => Ok((SqlValue::Numeric(num1), SqlValue::Numeric(num2))),
                (SqlNumeric::Real { value: v1 }, SqlNumeric::DoublePrecision { value: _ }) => Ok((
                    SqlValue::Numeric(SqlNumeric::DoublePrecision { value: v1 as f64 }),
                    SqlValue::Numeric(num2),
                )),
                (SqlNumeric::DoublePrecision { value: _ }, SqlNumeric::Real { value: v2 }) => Ok((
                    SqlValue::Numeric(num1),
                    SqlValue::Numeric(SqlNumeric::DoublePrecision { value: v2 as f64 }),
                )),
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::DoublePrecision { value: _ }) => {
                    Ok((
                        SqlValue::Numeric(SqlNumeric::DoublePrecision { value: v1 as f64 }),
                        SqlValue::Numeric(num2),
                    ))
                }
                (SqlNumeric::DoublePrecision { value: _ }, SqlNumeric::SmallInt { value: v2 }) => {
                    Ok((
                        SqlValue::Numeric(num1),
                        SqlValue::Numeric(SqlNumeric::DoublePrecision { value: v2 as f64 }),
                    ))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::DoublePrecision { value: _ }) => Ok((
                    SqlValue::Numeric(SqlNumeric::DoublePrecision { value: v1 as f64 }),
                    SqlValue::Numeric(num2),
                )),
                (SqlNumeric::DoublePrecision { value: _ }, SqlNumeric::Int { value: v2 }) => Ok((
                    SqlValue::Numeric(num1),
                    SqlValue::Numeric(SqlNumeric::DoublePrecision { value: v2 as f64 }),
                )),
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::DoublePrecision { value: _ }) => {
                    Ok((
                        SqlValue::Numeric(SqlNumeric::DoublePrecision { value: v1 as f64 }),
                        SqlValue::Numeric(num2),
                    ))
                }
                (SqlNumeric::DoublePrecision { value: _ }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok((
                        SqlValue::Numeric(num1),
                        SqlValue::Numeric(SqlNumeric::DoublePrecision { value: v2 as f64 }),
                    ))
                }
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::Real { value: _ }) => Ok((
                    SqlValue::Numeric(SqlNumeric::Real { value: v1 as f32 }),
                    SqlValue::Numeric(num2),
                )),
                (SqlNumeric::Real { value: _ }, SqlNumeric::SmallInt { value: v2 }) => Ok((
                    SqlValue::Numeric(num1),
                    SqlValue::Numeric(SqlNumeric::Real { value: v2 as f32 }),
                )),
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Real { value: _ }) => Ok((
                    SqlValue::Numeric(SqlNumeric::Real { value: v1 as f32 }),
                    SqlValue::Numeric(num2),
                )),
                (SqlNumeric::Real { value: _ }, SqlNumeric::Int { value: v2 }) => Ok((
                    SqlValue::Numeric(num1),
                    SqlValue::Numeric(SqlNumeric::Real { value: v2 as f32 }),
                )),
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::Real { value: _ }) => Ok((
                    SqlValue::Numeric(SqlNumeric::Real { value: v1 as f32 }),
                    SqlValue::Numeric(num2),
                )),
                (SqlNumeric::Real { value: _ }, SqlNumeric::BigInt { value: v2 }) => Ok((
                    SqlValue::Numeric(num1),
                    SqlValue::Numeric(SqlNumeric::Real { value: v2 as f32 }),
                )),
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::BigInt { value: _ }) => Ok((
                    SqlValue::Numeric(SqlNumeric::BigInt { value: v1 as i64 }),
                    SqlValue::Numeric(num2),
                )),
                (SqlNumeric::BigInt { value: _ }, SqlNumeric::SmallInt { value: v2 }) => Ok((
                    SqlValue::Numeric(num1),
                    SqlValue::Numeric(SqlNumeric::BigInt { value: v2 as i64 }),
                )),
                (SqlNumeric::Int { value: v1 }, SqlNumeric::BigInt { value: _ }) => Ok((
                    SqlValue::Numeric(SqlNumeric::BigInt { value: v1 as i64 }),
                    SqlValue::Numeric(num2),
                )),
                (SqlNumeric::BigInt { value: _ }, SqlNumeric::Int { value: v2 }) => Ok((
                    SqlValue::Numeric(num1),
                    SqlValue::Numeric(SqlNumeric::BigInt { value: v2 as i64 }),
                )),
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::Int { value: _ }) => Ok((
                    SqlValue::Numeric(SqlNumeric::Int { value: v1 as i32 }),
                    SqlValue::Numeric(num2),
                )),
                (SqlNumeric::Int { value: _ }, SqlNumeric::SmallInt { value: v2 }) => Ok((
                    SqlValue::Numeric(num1),
                    SqlValue::Numeric(SqlNumeric::Int { value: v2 as i32 }),
                )),
                _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
            },
            (SqlValue::Text(text1), SqlValue::Text(text2)) => match (&text1, &text2) {
                (&SqlText::Text { value: _ }, &SqlText::Text { value: _ })
                | (
                    &SqlText::VarChar {
                        value: _,
                        maxlen: _,
                        len: _,
                    },
                    &SqlText::VarChar {
                        value: _,
                        maxlen: _,
                        len: _,
                    },
                )
                | (&SqlText::Char { value: _, len: _ }, &SqlText::Char { value: _, len: _ }) => {
                    Ok((SqlValue::Text(text1), SqlValue::Text(text2)))
                }
                (
                    &SqlText::VarChar {
                        value: ref v1,
                        maxlen: _,
                        len: _,
                    },
                    &SqlText::Text { value: _ },
                ) => Ok((
                    SqlValue::Text(SqlText::Text { value: v1.clone() }),
                    SqlValue::Text(text2),
                )),
                (
                    &SqlText::Text { value: _ },
                    &SqlText::VarChar {
                        value: ref v2,
                        maxlen: _,
                        len: _,
                    },
                ) => Ok((
                    SqlValue::Text(text1),
                    SqlValue::Text(SqlText::Text { value: v2.clone() }),
                )),
                _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
            },
            _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
        }
    }

    pub fn decode_type(data: &MemoryCell, typ: SqlType) -> Result<Self, SqlTypeError> {
        if data.bytes.len() == 0 {
            return Ok(SqlValue::Null);
        }
        match typ {
            SqlType::Text => Ok(SqlValue::Text(SqlText::decode_text(data)?)),
            SqlType::VarChar => Ok(SqlValue::Text(SqlText::decode_varchar(data)?)),
            SqlType::Char => Ok(SqlValue::Text(SqlText::decode_char(data)?)),
            SqlType::SmallInt => Ok(SqlValue::Numeric(SqlNumeric::decode_small_int(data)?)),
            SqlType::Int => Ok(SqlValue::Numeric(SqlNumeric::decode_int(data)?)),
            SqlType::BigInt => Ok(SqlValue::Numeric(SqlNumeric::decode_big_int(data)?)),
            SqlType::Real => Ok(SqlValue::Numeric(SqlNumeric::decode_real(data)?)),
            SqlType::DoublePrecision => Ok(SqlValue::Numeric(SqlNumeric::decode_double_precision(
                data,
            )?)),
            SqlType::Boolean => Ok(SqlValue::Boolean(data.as_bool()?)),
            SqlType::Null => Ok(SqlValue::Null),
        }
    }

    pub fn from_token(token: &Token) -> Result<Self, SqlTypeError> {
        match token {
            Token::StringValue { value } => Ok(SqlValue::Text(SqlText::Text {
                value: value.clone(),
            })),
            Token::NumericValue { value } => Ok(SqlValue::Numeric(SqlNumeric::parse(value)?)),
            Token::BoolValue { value } => Ok(SqlValue::Boolean(*value)),
            Token::Null => Ok(SqlValue::Null),
            _ => Err(SqlTypeError::ParseError(
                "Invalid token type to convert to value".to_string(),
            )),
        }
    }

    pub fn encode(&self) -> MemoryCell {
        match self {
            SqlValue::Null => MemoryCell { bytes: vec![] },
            SqlValue::Text(text) => match &text {
                SqlText::Text { value } => MemoryCell {
                    bytes: value.as_bytes().into(),
                },
                SqlText::VarChar {
                    value,
                    maxlen: _,
                    len: _,
                } => MemoryCell {
                    bytes: value.as_bytes().into(),
                },
                SqlText::Char { value, len: _ } => MemoryCell {
                    bytes: value.as_bytes().into(),
                },
            },
            SqlValue::Numeric(num) => match &num {
                SqlNumeric::SmallInt { value } => MemoryCell {
                    bytes: value.to_be_bytes().into(),
                },
                SqlNumeric::Int { value } => MemoryCell {
                    bytes: value.to_be_bytes().into(),
                },
                SqlNumeric::BigInt { value } => MemoryCell {
                    bytes: value.to_be_bytes().into(),
                },
                SqlNumeric::Real { value } => MemoryCell {
                    bytes: value.to_be_bytes().into(),
                },
                SqlNumeric::DoublePrecision { value } => MemoryCell {
                    bytes: value.to_be_bytes().into(),
                },
            },
            SqlValue::Boolean(val) => match val {
                true => MemoryCell { bytes: vec![1] },
                false => MemoryCell { bytes: vec![0] },
            },
        }
    }

    pub fn subtract(a: Self, b: Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::convert_to_matching_types(a, b)?;
        match (&a, &b) {
            (SqlValue::Numeric(ref num1), SqlValue::Numeric(ref num2)) => match (num1, num2) {
                (
                    SqlNumeric::DoublePrecision { value: v1 },
                    SqlNumeric::DoublePrecision { value: v2 },
                ) => Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision {
                    value: v1 - v2,
                })),
                (SqlNumeric::Real { value: v1 }, SqlNumeric::Real { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Real { value: v1 - v2 }))
                }
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::SmallInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::SmallInt {
                        value: match v1.checked_sub(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::subtract(a.to_type(SqlType::Int)?, b);
                            }
                        },
                    }))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Int { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: match v1.checked_sub(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::subtract(a.to_type(SqlType::BigInt)?, b);
                            }
                        },
                    }))
                }
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: match v1.checked_sub(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::subtract(a.to_type(SqlType::DoublePrecision)?, b);
                            }
                        },
                    }))
                }
                _ => Err(SqlTypeError::TypeMismatchError(
                    "Type mismatch for subtraction".to_string(),
                )),
            },
            _ => Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for subtraction".to_string(),
            )),
        }
    }

    pub fn add(a: Self, b: Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::convert_to_matching_types(a, b)?;
        match (&a, &b) {
            (SqlValue::Numeric(ref num1), SqlValue::Numeric(ref num2)) => match (num1, num2) {
                (
                    SqlNumeric::DoublePrecision { value: v1 },
                    SqlNumeric::DoublePrecision { value: v2 },
                ) => Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision {
                    value: v1 + v2,
                })),
                (SqlNumeric::Real { value: v1 }, SqlNumeric::Real { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Real { value: v1 + v2 }))
                }
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::SmallInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::SmallInt {
                        value: match v1.checked_add(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::add(a.to_type(SqlType::Int)?, b);
                            }
                        },
                    }))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Int { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: match v1.checked_add(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::add(a.to_type(SqlType::BigInt)?, b);
                            }
                        },
                    }))
                }
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: match v1.checked_add(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::add(a.to_type(SqlType::DoublePrecision)?, b);
                            }
                        },
                    }))
                }
                _ => Err(SqlTypeError::TypeMismatchError(
                    "Type mismatch for addition".to_string(),
                )),
            },
            _ => Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for additon".to_string(),
            )),
        }
    }

    pub fn multiply(a: Self, b: Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::convert_to_matching_types(a, b)?;
        match (&a, &b) {
            (SqlValue::Numeric(ref num1), SqlValue::Numeric(ref num2)) => match (num1, num2) {
                (
                    SqlNumeric::DoublePrecision { value: v1 },
                    SqlNumeric::DoublePrecision { value: v2 },
                ) => Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision {
                    value: v1 * v2,
                })),
                (SqlNumeric::Real { value: v1 }, SqlNumeric::Real { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Real { value: v1 * v2 }))
                }
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::SmallInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::SmallInt {
                        value: match v1.checked_mul(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::multiply(a.to_type(SqlType::Int)?, b);
                            }
                        },
                    }))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Int { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: match v1.checked_mul(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::multiply(a.to_type(SqlType::BigInt)?, b);
                            }
                        },
                    }))
                }
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: match v1.checked_mul(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::multiply(a.to_type(SqlType::DoublePrecision)?, b);
                            }
                        },
                    }))
                }
                _ => Err(SqlTypeError::TypeMismatchError(
                    "Type mismatch for multiplication".to_string(),
                )),
            },
            _ => Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for multiplication".to_string(),
            )),
        }
    }

    pub fn divide(a: Self, b: Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::convert_to_matching_types(a, b)?;

        if let SqlValue::Numeric(SqlNumeric::DoublePrecision { value }) =
            b.to_type(SqlType::DoublePrecision)?
        {
            if value == 0.0 {
                return Err(SqlTypeError::OperationError("Division by zero".to_string()));
            }
        }
        match (&a, &b) {
            (SqlValue::Numeric(ref num1), SqlValue::Numeric(ref num2)) => match (num1, num2) {
                (
                    SqlNumeric::DoublePrecision { value: v1 },
                    SqlNumeric::DoublePrecision { value: v2 },
                ) => Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision {
                    value: v1 / v2,
                })),
                (SqlNumeric::Real { value: v1 }, SqlNumeric::Real { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Real { value: v1 / v2 }))
                }
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::SmallInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::SmallInt {
                        value: match v1.checked_div(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::divide(a.to_type(SqlType::Int)?, b);
                            }
                        },
                    }))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Int { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: match v1.checked_div(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::divide(a.to_type(SqlType::BigInt)?, b);
                            }
                        },
                    }))
                }
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: match v1.checked_div(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::divide(a.to_type(SqlType::DoublePrecision)?, b);
                            }
                        },
                    }))
                }
                _ => Err(SqlTypeError::TypeMismatchError(
                    "Type mismatch for division".to_string(),
                )),
            },
            _ => Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for division".to_string(),
            )),
        }
    }

    pub fn modulo(a: Self, b: Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::convert_to_matching_types(a, b)?;

        if let SqlValue::Numeric(SqlNumeric::DoublePrecision { value }) =
            b.to_type(SqlType::DoublePrecision)?
        {
            if value == 0.0 {
                return Err(SqlTypeError::OperationError("Division by zero".to_string()));
            }
        }
        match (&a, &b) {
            (SqlValue::Numeric(ref num1), SqlValue::Numeric(ref num2)) => match (num1, num2) {
                (
                    SqlNumeric::DoublePrecision { value: v1 },
                    SqlNumeric::DoublePrecision { value: v2 },
                ) => Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision {
                    value: v1 % v2,
                })),
                (SqlNumeric::Real { value: v1 }, SqlNumeric::Real { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Real { value: v1 % v2 }))
                }
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::SmallInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::SmallInt {
                        value: match v1.checked_rem(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::modulo(a.to_type(SqlType::Int)?, b);
                            }
                        },
                    }))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Int { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: match v1.checked_rem(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::modulo(a.to_type(SqlType::BigInt)?, b);
                            }
                        },
                    }))
                }
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: match v1.checked_rem(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::modulo(a.to_type(SqlType::DoublePrecision)?, b);
                            }
                        },
                    }))
                }
                _ => Err(SqlTypeError::TypeMismatchError(
                    "Type mismatch for modulo".to_string(),
                )),
            },
            _ => Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for modulo".to_string(),
            )),
        }
    }

    pub fn exponentiation(a: Self, b: Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::convert_to_matching_types(a, b)?;

        if let SqlValue::Numeric(SqlNumeric::DoublePrecision { value }) =
            b.to_type(SqlType::DoublePrecision)?
        {
            if value == 0.0 {
                return Err(SqlTypeError::OperationError("Division by zero".to_string()));
            }
        }
        match (&a, &b) {
            (SqlValue::Numeric(ref num1), SqlValue::Numeric(ref num2)) => match (num1, num2) {
                (
                    SqlNumeric::DoublePrecision { value: v1 },
                    SqlNumeric::DoublePrecision { value: v2 },
                ) => Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision {
                    value: v1.powf(*v2),
                })),
                (SqlNumeric::Real { value: v1 }, SqlNumeric::Real { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Real {
                        value: v1.powf(*v2),
                    }))
                }
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::SmallInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::SmallInt {
                        value: match v1.checked_pow((*v2).try_into()?) {
                            Some(val) => val,
                            None => {
                                return SqlValue::exponentiation(a.to_type(SqlType::Int)?, b);
                            }
                        },
                    }))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Int { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: match v1.checked_pow((*v2).try_into()?) {
                            Some(val) => val,
                            None => {
                                return SqlValue::exponentiation(a.to_type(SqlType::BigInt)?, b);
                            }
                        },
                    }))
                }
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: match v1.checked_pow((*v2).try_into()?) {
                            Some(val) => val,
                            None => {
                                return SqlValue::exponentiation(
                                    a.to_type(SqlType::DoublePrecision)?,
                                    b,
                                );
                            }
                        },
                    }))
                }
                _ => Err(SqlTypeError::TypeMismatchError(
                    "Type mismatch for modulo".to_string(),
                )),
            },
            _ => Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for modulo".to_string(),
            )),
        }
    }

    pub fn concat(a: Self, b: Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::convert_to_matching_types(a, b)?;
        match (a, b) {
            (SqlValue::Text(ref text1), SqlValue::Text(ref text2)) => match (text1, text2) {
                (SqlText::Text { value: v1 }, SqlText::Text { value: v2 }) => {
                    let mut new_v = v1.clone();
                    new_v.push_str(v2.as_str());
                    Ok(SqlValue::Text(SqlText::Text { value: new_v }))
                }
                _ => Err(SqlTypeError::TypeMismatchError(
                    "Type mismatch for concat".to_string(),
                )),
            },
            _ => Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for concat".to_string(),
            )),
        }
    }

    pub fn equals(a: Self, b: Self) -> Result<Self, SqlTypeError> {
        if a.is_null() || b.is_null() {
            Ok(SqlValue::Null)
        } else {
            let (a, b) = SqlValue::convert_to_matching_types(a, b)?;
            Ok(SqlValue::Boolean(a == b))
        }
    }

    pub fn not_equal(a: Self, b: Self) -> Result<Self, SqlTypeError> {
        if a.is_null() || b.is_null() {
            Ok(SqlValue::Null)
        } else {
            let (a, b) = SqlValue::convert_to_matching_types(a, b)?;
            Ok(SqlValue::Boolean(a != b))
        }
    }

    pub fn less_than(a: Self, b: Self) -> Result<Self, SqlTypeError> {
        if a.is_null() || b.is_null() {
            Ok(SqlValue::Null)
        } else {
            let (a, b) = SqlValue::convert_to_matching_types(a, b)?;
            Ok(SqlValue::Boolean(a < b))
        }
    }

    pub fn less_than_or_equals(a: Self, b: Self) -> Result<Self, SqlTypeError> {
        if a.is_null() || b.is_null() {
            Ok(SqlValue::Null)
        } else {
            let (a, b) = SqlValue::convert_to_matching_types(a, b)?;
            Ok(SqlValue::Boolean(a <= b))
        }
    }

    pub fn greater_than(a: Self, b: Self) -> Result<Self, SqlTypeError> {
        if a.is_null() || b.is_null() {
            Ok(SqlValue::Null)
        } else {
            let (a, b) = SqlValue::convert_to_matching_types(a, b)?;
            Ok(SqlValue::Boolean(a > b))
        }
    }

    pub fn greater_than_or_equals(a: Self, b: Self) -> Result<Self, SqlTypeError> {
        if a.is_null() || b.is_null() {
            Ok(SqlValue::Null)
        } else {
            let (a, b) = SqlValue::convert_to_matching_types(a, b)?;
            Ok(SqlValue::Boolean(a >= b))
        }
    }

    pub fn and(a: Self, b: Self) -> Result<Self, SqlTypeError> {
        if let (SqlValue::Boolean(a), SqlValue::Boolean(b)) = (a, b) {
            Ok(SqlValue::Boolean(a && b))
        } else {
            Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for concat".to_string(),
            ))
        }
    }

    pub fn or(a: Self, b: Self) -> Result<Self, SqlTypeError> {
        if let (SqlValue::Boolean(a), SqlValue::Boolean(b)) = (a, b) {
            Ok(SqlValue::Boolean(a || b))
        } else {
            Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for concat".to_string(),
            ))
        }
    }

    pub fn get_type(&self) -> SqlType {
        match self {
            SqlValue::Numeric(num) => match &num {
                SqlNumeric::SmallInt { value: _ } => SqlType::SmallInt,
                SqlNumeric::Int { value: _ } => SqlType::Int,
                SqlNumeric::BigInt { value: _ } => SqlType::BigInt,
                SqlNumeric::Real { value: _ } => SqlType::Real,
                SqlNumeric::DoublePrecision { value: _ } => SqlType::DoublePrecision,
            },
            SqlValue::Text(text) => match &text {
                SqlText::Text { value: _ } => SqlType::Text,
                SqlText::VarChar {
                    value: _,
                    maxlen: _,
                    len: _,
                } => SqlType::VarChar,
                SqlText::Char { value: _, len: _ } => SqlType::Char,
            },
            SqlValue::Boolean(_) => SqlType::Boolean,
            SqlValue::Null => SqlType::Null,
        }
    }

    pub fn to_type(&self, typ: SqlType) -> Result<Self, SqlTypeError> {
        if self.is_null() {
            return Ok(SqlValue::Null);
        }
        match typ {
            SqlType::BigInt => match self {
                SqlValue::Numeric(num) => match num {
                    SqlNumeric::SmallInt { value } => Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: i64::from(*value),
                    })),
                    SqlNumeric::Int { value } => Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: i64::try_from(*value)?,
                    })),
                    SqlNumeric::BigInt { value } => Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: i64::try_from(*value)?,
                    })),
                    SqlNumeric::Real { value } => Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: *value as i64,
                    })),
                    SqlNumeric::DoublePrecision { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                            value: *value as i64,
                        }))
                    }
                },
                _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
            },
            SqlType::Int => match self {
                SqlValue::Numeric(num) => match num {
                    SqlNumeric::SmallInt { value } => Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: i32::from(*value),
                    })),
                    SqlNumeric::Int { value } => Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: i32::try_from(*value)?,
                    })),
                    SqlNumeric::BigInt { value } => Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: i32::try_from(*value)?,
                    })),
                    SqlNumeric::Real { value } => Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: *value as i32,
                    })),
                    SqlNumeric::DoublePrecision { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::Int {
                            value: *value as i32,
                        }))
                    }
                },
                _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
            },
            SqlType::SmallInt => match self {
                SqlValue::Numeric(num) => match num {
                    SqlNumeric::SmallInt { value } => Ok(SqlValue::Numeric(SqlNumeric::SmallInt {
                        value: i16::from(*value),
                    })),
                    SqlNumeric::Int { value } => Ok(SqlValue::Numeric(SqlNumeric::SmallInt {
                        value: i16::try_from(*value)?,
                    })),
                    SqlNumeric::BigInt { value } => Ok(SqlValue::Numeric(SqlNumeric::SmallInt {
                        value: i16::try_from(*value)?,
                    })),
                    SqlNumeric::Real { value } => Ok(SqlValue::Numeric(SqlNumeric::SmallInt {
                        value: *value as i16,
                    })),
                    SqlNumeric::DoublePrecision { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::SmallInt {
                            value: *value as i16,
                        }))
                    }
                },
                _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
            },
            SqlType::Real => match self {
                SqlValue::Numeric(num) => match num {
                    SqlNumeric::SmallInt { value } => Ok(SqlValue::Numeric(SqlNumeric::Real {
                        value: f32::from(*value),
                    })),
                    SqlNumeric::Int { value } => Ok(SqlValue::Numeric(SqlNumeric::Real {
                        value: *value as f32,
                    })),
                    SqlNumeric::BigInt { value } => Ok(SqlValue::Numeric(SqlNumeric::Real {
                        value: *value as f32,
                    })),
                    SqlNumeric::Real { value } => Ok(SqlValue::Numeric(SqlNumeric::Real {
                        value: f32::try_from(*value)?,
                    })),
                    SqlNumeric::DoublePrecision { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::Real {
                            value: *value as f32,
                        }))
                    }
                },
                _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
            },
            SqlType::DoublePrecision => match self {
                SqlValue::Numeric(num) => match num {
                    SqlNumeric::SmallInt { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision {
                            value: f64::from(*value),
                        }))
                    }
                    SqlNumeric::Int { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision {
                            value: f64::from(*value),
                        }))
                    }
                    SqlNumeric::BigInt { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision {
                            value: *value as f64,
                        }))
                    }
                    SqlNumeric::Real { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision {
                            value: f64::from(*value),
                        }))
                    }
                    SqlNumeric::DoublePrecision { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision {
                            value: f64::from(*value),
                        }))
                    }
                },
                _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
            },
            SqlType::Text => match self {
                SqlValue::Text(text) => match text {
                    SqlText::Text { value } => Ok(SqlValue::Text(SqlText::Text {
                        value: value.clone(),
                    })),
                    SqlText::VarChar {
                        value,
                        len: _,
                        maxlen: _,
                    } => Ok(SqlValue::Text(SqlText::Text {
                        value: value.clone(),
                    })),
                    SqlText::Char { value, len: _ } => Ok(SqlValue::Text(SqlText::Text {
                        value: value.clone(),
                    })),
                },
                _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
            },
            SqlType::VarChar => match self {
                SqlValue::Text(text) => match text {
                    SqlText::Text { value } => Ok(SqlValue::Text(SqlText::VarChar {
                        len: value.len(),
                        maxlen: value.len(),
                        value: value.clone(),
                    })),
                    SqlText::VarChar { value, len, maxlen } => {
                        Ok(SqlValue::Text(SqlText::VarChar {
                            value: value.clone(),
                            len: *len,
                            maxlen: *maxlen,
                        }))
                    }
                    SqlText::Char { value, len } => Ok(SqlValue::Text(SqlText::VarChar {
                        value: value.clone(),
                        len: *len,
                        maxlen: *len,
                    })),
                },
                _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
            },
            SqlType::Char => match self {
                SqlValue::Text(text) => match text {
                    SqlText::Text { value } => Ok(SqlValue::Text(SqlText::Char {
                        len: value.len(),
                        value: value.clone(),
                    })),
                    SqlText::VarChar {
                        value,
                        len,
                        maxlen: _,
                    } => Ok(SqlValue::Text(SqlText::Char {
                        value: value.clone(),
                        len: *len,
                    })),
                    SqlText::Char { value, len } => Ok(SqlValue::Text(SqlText::Char {
                        value: value.clone(),
                        len: *len,
                    })),
                },
                _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
            },
            SqlType::Boolean => match self {
                SqlValue::Boolean(val) => Ok(SqlValue::Boolean(*val)),
                _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
            },
            SqlType::Null => Ok(SqlValue::Null),
        }
    }
}

impl SqlNumeric {
    pub fn parse(data: &String) -> Result<Self, SqlTypeError> {
        if let Ok(value) = data.parse::<i16>() {
            Ok(SqlNumeric::SmallInt { value })
        } else if let Ok(value) = data.parse::<i32>() {
            Ok(SqlNumeric::Int { value })
        } else if let Ok(value) = data.parse::<i64>() {
            Ok(SqlNumeric::BigInt { value })
        } else if let Ok(value) = data.parse::<f32>() {
            match (data.parse::<f32>(), data.parse::<f64>()) {
                (Ok(v1), Ok(v2)) => {
                    if v1 as f64 != v2 {
                        Ok(SqlNumeric::DoublePrecision { value: v2 })
                    } else {
                        Ok(SqlNumeric::Real { value })
                    }
                }
                _ => Ok(SqlNumeric::Real { value }),
            }
        } else if let Ok(value) = data.parse::<f64>() {
            Ok(SqlNumeric::DoublePrecision { value })
        } else {
            Err(SqlTypeError::ParseError(
                "Failed to parse bytes to numeric.".to_string(),
            ))
        }
    }

    pub fn parse_small_int(data: String) -> Result<Self, SqlTypeError> {
        if let Ok(value) = data.parse::<i16>() {
            Ok(SqlNumeric::SmallInt { value })
        } else {
            Err(SqlTypeError::ParseError(
                "Failed to parse bytes to small int.".to_string(),
            ))
        }
    }

    pub fn parse_int(data: String) -> Result<Self, SqlTypeError> {
        if let Ok(value) = data.parse::<i32>() {
            Ok(SqlNumeric::Int { value })
        } else {
            Err(SqlTypeError::ParseError(
                "Failed to parse bytes to int.".to_string(),
            ))
        }
    }

    pub fn parse_big_int(data: String) -> Result<Self, SqlTypeError> {
        if let Ok(value) = data.parse::<i64>() {
            Ok(SqlNumeric::BigInt { value })
        } else {
            Err(SqlTypeError::ParseError(
                "Failed to parse bytes to big int.".to_string(),
            ))
        }
    }

    pub fn parse_real(data: String) -> Result<Self, SqlTypeError> {
        if let Ok(value) = data.parse::<f32>() {
            Ok(SqlNumeric::Real { value })
        } else {
            Err(SqlTypeError::ParseError(
                "Failed to parse bytes to real.".to_string(),
            ))
        }
    }

    pub fn parse_double_precision(data: String) -> Result<Self, SqlTypeError> {
        if let Ok(value) = data.parse::<f64>() {
            Ok(SqlNumeric::DoublePrecision { value })
        } else {
            Err(SqlTypeError::ParseError(
                "Failed to parse bytes to double precision.".to_string(),
            ))
        }
    }

    pub fn decode_small_int(data: &MemoryCell) -> Result<Self, SqlTypeError> {
        let mut rdr = std::io::Cursor::new(&data.bytes);

        if let Ok(value) = rdr.read_i16::<BigEndian>() {
            Ok(SqlNumeric::SmallInt { value })
        } else {
            Err(SqlTypeError::DecodeError(
                "Failed to decode bytes to i16.".to_string(),
            ))
        }
    }

    pub fn decode_int(data: &MemoryCell) -> Result<Self, SqlTypeError> {
        let mut rdr = std::io::Cursor::new(&data.bytes);

        if let Ok(value) = rdr.read_i32::<BigEndian>() {
            Ok(SqlNumeric::Int { value })
        } else {
            Err(SqlTypeError::DecodeError(
                "Failed to decode bytes to i32.".to_string(),
            ))
        }
    }

    pub fn decode_big_int(data: &MemoryCell) -> Result<Self, SqlTypeError> {
        let mut rdr = std::io::Cursor::new(&data.bytes);
        if let Ok(value) = rdr.read_i64::<BigEndian>() {
            Ok(SqlNumeric::BigInt { value })
        } else {
            Err(SqlTypeError::DecodeError(
                "Failed to decode bytes to i64.".to_string(),
            ))
        }
    }

    pub fn decode_real(data: &MemoryCell) -> Result<Self, SqlTypeError> {
        let mut rdr = std::io::Cursor::new(&data.bytes);
        if let Ok(value) = rdr.read_f32::<BigEndian>() {
            Ok(SqlNumeric::Real { value })
        } else {
            Err(SqlTypeError::DecodeError(
                "Failed to decode bytes to f32.".to_string(),
            ))
        }
    }

    pub fn decode_double_precision(data: &MemoryCell) -> Result<Self, SqlTypeError> {
        let mut rdr = std::io::Cursor::new(&data.bytes);
        if let Ok(value) = rdr.read_f64::<BigEndian>() {
            Ok(SqlNumeric::DoublePrecision { value })
        } else {
            Err(SqlTypeError::DecodeError(
                "Failed to decode bytes to f64.".to_string(),
            ))
        }
    }
}

impl SqlText {
    pub fn parse_text(data: String) -> Result<Self, SqlTypeError> {
        Ok(SqlText::Text { value: data })
    }

    pub fn parse_varchar(data: String, maxlen: usize) -> Result<Self, SqlTypeError> {
        if maxlen <= data.len() {
            Ok(SqlText::Text { value: data })
        } else {
            Err(SqlTypeError::ParseError(format!(
                "Input text too long, max length: {}, current length: {}",
                maxlen,
                data.len()
            )))
        }
    }

    pub fn parse_char(data: String, len: usize) -> Result<Self, SqlTypeError> {
        if len == data.len() {
            Ok(SqlText::Text { value: data })
        } else if len < data.len() {
            Err(SqlTypeError::ParseError(format!(
                "Input text too long, expected length: {}, current length: {}",
                len,
                data.len()
            )))
        } else {
            Err(SqlTypeError::ParseError(format!(
                "Input text too short, expected length: {}, current length: {}",
                len,
                data.len()
            )))
        }
    }

    pub fn decode_text(data: &MemoryCell) -> Result<Self, SqlTypeError> {
        let mut rdr = std::io::Cursor::new(&data.bytes);

        let mut text = "".to_owned();
        match rdr.read_to_string(&mut text) {
            Ok(_) => Ok(SqlText::Text { value: text }),
            Err(_err) => Err(SqlTypeError::DecodeError(
                "Failed to decode bytes to String.".to_string(),
            )),
        }
    }

    pub fn decode_varchar(data: &MemoryCell) -> Result<Self, SqlTypeError> {
        SqlText::decode_text(data)
    }

    pub fn decode_char(data: &MemoryCell) -> Result<Self, SqlTypeError> {
        SqlText::decode_text(data)
    }
}

impl std::fmt::Display for SqlValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SqlValue::Numeric(val) => val.to_string(),
                SqlValue::Text(val) => val.to_string(),
                SqlValue::Boolean(val) => val.to_string(),
                SqlValue::Null => "NULL".to_string(),
            }
        )
    }
}

impl std::fmt::Display for SqlText {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SqlText::Char { value, len: _ } => value.clone(),
                SqlText::VarChar {
                    value,
                    maxlen: _,
                    len: _,
                } => value.clone(),
                SqlText::Text { value } => value.clone(),
            }
        )
    }
}

impl std::fmt::Display for SqlNumeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SqlNumeric::SmallInt { value } => value.to_string(),
                SqlNumeric::Int { value } => value.to_string(),
                SqlNumeric::BigInt { value } => value.to_string(),
                SqlNumeric::Real { value } => value.to_string(),
                SqlNumeric::DoublePrecision { value } => value.to_string(),
            }
        )
    }
}

impl From<&str> for SqlTypeError {
    fn from(err: &str) -> Self {
        SqlTypeError::DecodeError(err.to_string())
    }
}

impl From<SqlTypeError> for String {
    fn from(err: SqlTypeError) -> Self {
        err.to_string()
    }
}

impl From<std::convert::Infallible> for SqlTypeError {
    fn from(_: std::convert::Infallible) -> Self {
        SqlTypeError::Infallible
    }
}

impl From<std::num::TryFromIntError> for SqlTypeError {
    fn from(err: std::num::TryFromIntError) -> Self {
        SqlTypeError::ConversionError(err.to_string())
    }
}
