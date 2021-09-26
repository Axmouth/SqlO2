use byteorder::{BigEndian, ReadBytesExt};
use std::convert::{TryFrom, TryInto};
use std::io::Read;

use crate::lexer::TokenContainer;
use crate::parser::ParsingError;
use crate::{
    backend::{Cell, MemoryCell, ERR_INVALID_DATA_TYPE},
    lexer::Token,
};
use serde::{Deserialize, Serialize, Serializer};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
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
    Type,
}

impl SqlType {
    #[inline]
    pub fn from_token(token_container: TokenContainer) -> Result<Self, ParsingError> {
        match token_container.token {
            Token::SmallInt => Ok(SqlType::SmallInt),
            Token::Int => Ok(SqlType::Int),
            Token::BigInt => Ok(SqlType::BigInt),
            Token::Real => Ok(SqlType::Real),
            Token::DoublePrecision => Ok(SqlType::DoublePrecision),
            Token::Varchar => Ok(SqlType::VarChar),
            Token::Text => Ok(SqlType::Text),
            Token::Char => Ok(SqlType::Char),
            Token::Bool => Ok(SqlType::Boolean),
            _ => Err(ParsingError::Internal {
                msg: ERR_INVALID_DATA_TYPE.to_string(),
            }),
        }
    }

    #[inline]
    pub fn order(&self) -> i32 {
        match self {
            SqlType::SmallInt => 1,
            SqlType::Int => 2,
            SqlType::BigInt => 3,
            SqlType::Real => 20,
            SqlType::DoublePrecision => 21,
            SqlType::Char => 101,
            SqlType::VarChar => 102,
            SqlType::Text => 103,
            SqlType::Boolean => 0,
            SqlType::Null => -1000,
            SqlType::Type => -2000,
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

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Deserialize)]
pub enum SqlValue {
    Null,
    Text(SqlText),
    Numeric(SqlNumeric),
    Boolean(bool),
    Type(SqlType),
}

impl Default for SqlValue {
    fn default() -> Self {
        SqlValue::Null
    }
}

impl Serialize for SqlValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            SqlValue::Null => serializer.serialize_none(),
            SqlValue::Text(text) => match text {
                SqlText::Char { value, len: _ } => serializer.serialize_str(value),
                SqlText::VarChar {
                    value,
                    maxlen: _,
                    len: _,
                } => serializer.serialize_str(value),
                SqlText::Text { value } => serializer.serialize_str(value),
            },
            SqlValue::Numeric(num) => match num {
                SqlNumeric::SmallInt { value } => serializer.serialize_i16(*value),
                SqlNumeric::Int { value } => serializer.serialize_i32(*value),
                SqlNumeric::BigInt { value } => serializer.serialize_i64(*value),
                SqlNumeric::Real { value } => serializer.serialize_f32(*value),
                SqlNumeric::DoublePrecision { value } => serializer.serialize_f64(*value),
            },
            SqlValue::Boolean(val) => serializer.serialize_bool(*val),
            SqlValue::Type(typ) => serializer.serialize_str(&typ.to_string()),
        }
    }
}
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Deserialize, Serialize)]
pub enum SqlNumeric {
    SmallInt { value: i16 },
    Int { value: i32 },
    BigInt { value: i64 },
    Real { value: f32 },
    DoublePrecision { value: f64 },
}

impl Eq for SqlNumeric {}

// TODO: Find better approach
#[allow(clippy::derive_ord_xor_partial_ord)]
impl Ord for SqlNumeric {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self).partial_cmp(other) {
            Some(val) => val,
            None => std::cmp::Ordering::Greater,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Deserialize, Serialize)]
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

#[inline]
fn factorial(num: i64) -> Result<i64, SqlTypeError> {
    if num < 0 {
        return Err(SqlTypeError::OperationError(
            "Cannot calculate factorial of negative number".to_string(),
        ));
    }
    let mut acc: i64 = 1;
    for n in 1..=num {
        acc = match acc.checked_mul(n) {
            Some(val) => val,
            None => {
                return Err(SqlTypeError::OverflowError(
                    "Overflow calculating factorial".to_string(),
                ));
            }
        };
    }
    Ok(acc)
}

impl SqlValue {
    #[inline]
    pub fn is_numeric(&self) -> bool {
        matches!(self, SqlValue::Numeric(_))
    }

    #[inline]
    pub fn is_int(&self) -> bool {
        match self {
            SqlValue::Numeric(num) => matches!(
                num,
                SqlNumeric::SmallInt { value: _ }
                    | SqlNumeric::Int { value: _ }
                    | SqlNumeric::BigInt { value: _ }
            ),
            _ => false,
        }
    }

    #[inline]
    pub fn is_float(&self) -> bool {
        match self {
            SqlValue::Numeric(num) => matches!(
                num,
                SqlNumeric::Real { value: _ } | SqlNumeric::DoublePrecision { value: _ }
            ),
            _ => false,
        }
    }

    #[inline]
    pub fn is_null(&self) -> bool {
        matches!(self, SqlValue::Null)
    }

    #[inline]
    pub fn is_text(&self) -> bool {
        matches!(self, SqlValue::Text(_))
    }

    #[inline]
    pub fn is_bool(&self) -> bool {
        matches!(self, SqlValue::Boolean(_))
    }

    #[inline]
    pub fn implicist_cast_to_matching_types(
        &self,
        b: &SqlValue,
    ) -> Result<(SqlValue, SqlValue), SqlTypeError> {
        match (self, b) {
            (SqlValue::Numeric(num1), SqlValue::Numeric(num2)) => match (num1, num2) {
                (SqlNumeric::Int { value: _ }, SqlNumeric::Int { value: _ })
                | (SqlNumeric::SmallInt { value: _ }, SqlNumeric::SmallInt { value: _ })
                | (SqlNumeric::BigInt { value: _ }, SqlNumeric::BigInt { value: _ })
                | (SqlNumeric::Real { value: _ }, SqlNumeric::Real { value: _ })
                | (
                    SqlNumeric::DoublePrecision { value: _ },
                    SqlNumeric::DoublePrecision { value: _ },
                ) => Ok((SqlValue::Numeric(*num1), SqlValue::Numeric(*num2))),
                (SqlNumeric::Real { value: v1 }, SqlNumeric::DoublePrecision { value: _ }) => Ok((
                    SqlValue::Numeric(SqlNumeric::DoublePrecision { value: *v1 as f64 }),
                    SqlValue::Numeric(*num2),
                )),
                (SqlNumeric::DoublePrecision { value: _ }, SqlNumeric::Real { value: v2 }) => Ok((
                    SqlValue::Numeric(*num1),
                    SqlValue::Numeric(SqlNumeric::DoublePrecision { value: *v2 as f64 }),
                )),
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::DoublePrecision { value: _ }) => {
                    Ok((
                        SqlValue::Numeric(SqlNumeric::DoublePrecision { value: *v1 as f64 }),
                        SqlValue::Numeric(*num2),
                    ))
                }
                (SqlNumeric::DoublePrecision { value: _ }, SqlNumeric::SmallInt { value: v2 }) => {
                    Ok((
                        SqlValue::Numeric(*num1),
                        SqlValue::Numeric(SqlNumeric::DoublePrecision { value: *v2 as f64 }),
                    ))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::DoublePrecision { value: _ }) => Ok((
                    SqlValue::Numeric(SqlNumeric::DoublePrecision { value: *v1 as f64 }),
                    SqlValue::Numeric(*num2),
                )),
                (SqlNumeric::DoublePrecision { value: _ }, SqlNumeric::Int { value: v2 }) => Ok((
                    SqlValue::Numeric(*num1),
                    SqlValue::Numeric(SqlNumeric::DoublePrecision { value: *v2 as f64 }),
                )),
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::DoublePrecision { value: _ }) => {
                    Ok((
                        SqlValue::Numeric(SqlNumeric::DoublePrecision { value: *v1 as f64 }),
                        SqlValue::Numeric(*num2),
                    ))
                }
                (SqlNumeric::DoublePrecision { value: _ }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok((
                        SqlValue::Numeric(*num1),
                        SqlValue::Numeric(SqlNumeric::DoublePrecision { value: *v2 as f64 }),
                    ))
                }
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::Real { value: _ }) => Ok((
                    SqlValue::Numeric(SqlNumeric::Real { value: *v1 as f32 }),
                    SqlValue::Numeric(*num2),
                )),
                (SqlNumeric::Real { value: _ }, SqlNumeric::SmallInt { value: v2 }) => Ok((
                    SqlValue::Numeric(*num1),
                    SqlValue::Numeric(SqlNumeric::Real { value: *v2 as f32 }),
                )),
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Real { value: _ }) => Ok((
                    SqlValue::Numeric(SqlNumeric::Real { value: *v1 as f32 }),
                    SqlValue::Numeric(*num2),
                )),
                (SqlNumeric::Real { value: _ }, SqlNumeric::Int { value: v2 }) => Ok((
                    SqlValue::Numeric(*num1),
                    SqlValue::Numeric(SqlNumeric::Real { value: *v2 as f32 }),
                )),
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::Real { value: _ }) => Ok((
                    SqlValue::Numeric(SqlNumeric::Real { value: *v1 as f32 }),
                    SqlValue::Numeric(*num2),
                )),
                (SqlNumeric::Real { value: _ }, SqlNumeric::BigInt { value: v2 }) => Ok((
                    SqlValue::Numeric(*num1),
                    SqlValue::Numeric(SqlNumeric::Real { value: *v2 as f32 }),
                )),
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::BigInt { value: _ }) => Ok((
                    SqlValue::Numeric(SqlNumeric::BigInt { value: *v1 as i64 }),
                    SqlValue::Numeric(*num2),
                )),
                (SqlNumeric::BigInt { value: _ }, SqlNumeric::SmallInt { value: v2 }) => Ok((
                    SqlValue::Numeric(*num1),
                    SqlValue::Numeric(SqlNumeric::BigInt { value: *v2 as i64 }),
                )),
                (SqlNumeric::Int { value: v1 }, SqlNumeric::BigInt { value: _ }) => Ok((
                    SqlValue::Numeric(SqlNumeric::BigInt { value: *v1 as i64 }),
                    SqlValue::Numeric(*num2),
                )),
                (SqlNumeric::BigInt { value: _ }, SqlNumeric::Int { value: v2 }) => Ok((
                    SqlValue::Numeric(*num1),
                    SqlValue::Numeric(SqlNumeric::BigInt { value: *v2 as i64 }),
                )),
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::Int { value: _ }) => Ok((
                    SqlValue::Numeric(SqlNumeric::Int { value: *v1 as i32 }),
                    SqlValue::Numeric(*num2),
                )),
                (SqlNumeric::Int { value: _ }, SqlNumeric::SmallInt { value: v2 }) => Ok((
                    SqlValue::Numeric(*num1),
                    SqlValue::Numeric(SqlNumeric::Int { value: *v2 as i32 }),
                )),
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
                    Ok((SqlValue::Text(text1.clone()), SqlValue::Text(text2.clone())))
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
                    SqlValue::Text(text2.clone()),
                )),
                (
                    &SqlText::Text { value: _ },
                    &SqlText::VarChar {
                        value: ref v2,
                        maxlen: _,
                        len: _,
                    },
                ) => Ok((
                    SqlValue::Text(text1.clone()),
                    SqlValue::Text(SqlText::Text { value: v2.clone() }),
                )),
                _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
            },
            _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
        }
    }

    #[inline]
    pub fn decode_type(data: &MemoryCell, typ: SqlType) -> Result<Self, SqlTypeError> {
        if data.bytes.is_empty() {
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
            SqlType::Type => Ok(SqlValue::Text(SqlText::decode_text(data)?)),
        }
    }

    #[inline]
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

    #[inline]
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
            SqlValue::Type(typ) => MemoryCell {
                bytes: format!("{:?}", typ).as_bytes().into(),
            },
        }
    }

    #[inline]
    pub fn subtract(&self, b: &Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;
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
                                return SqlValue::subtract(&a.to_type(SqlType::Int)?, &b);
                            }
                        },
                    }))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Int { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: match v1.checked_sub(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::subtract(&a.to_type(SqlType::BigInt)?, &b);
                            }
                        },
                    }))
                }
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: match v1.checked_sub(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::subtract(
                                    &a.to_type(SqlType::DoublePrecision)?,
                                    &b,
                                );
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

    #[inline]
    pub fn add(&self, b: &Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;
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
                                return SqlValue::add(&a.to_type(SqlType::Int)?, &b);
                            }
                        },
                    }))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Int { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: match v1.checked_add(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::add(&a.to_type(SqlType::BigInt)?, &b);
                            }
                        },
                    }))
                }
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: match v1.checked_add(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::add(&a.to_type(SqlType::DoublePrecision)?, &b);
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

    #[inline]
    pub fn multiply(&self, b: &Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;
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
                                return SqlValue::multiply(&a.to_type(SqlType::Int)?, &b);
                            }
                        },
                    }))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Int { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: match v1.checked_mul(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::multiply(&a.to_type(SqlType::BigInt)?, &b);
                            }
                        },
                    }))
                }
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: match v1.checked_mul(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::multiply(
                                    &a.to_type(SqlType::DoublePrecision)?,
                                    &b,
                                );
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

    #[inline]
    pub fn divide(&self, b: &Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;

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
                                return SqlValue::divide(&a.to_type(SqlType::Int)?, &b);
                            }
                        },
                    }))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Int { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: match v1.checked_div(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::divide(&a.to_type(SqlType::BigInt)?, &b);
                            }
                        },
                    }))
                }
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: match v1.checked_div(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::divide(&a.to_type(SqlType::DoublePrecision)?, &b);
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

    #[inline]
    pub fn modulo(&self, b: &Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;

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
                                return SqlValue::modulo(&a.to_type(SqlType::Int)?, &b);
                            }
                        },
                    }))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Int { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: match v1.checked_rem(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::modulo(&a.to_type(SqlType::BigInt)?, &b);
                            }
                        },
                    }))
                }
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: match v1.checked_rem(*v2) {
                            Some(val) => val,
                            None => {
                                return SqlValue::modulo(&a.to_type(SqlType::DoublePrecision)?, &b);
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

    #[inline]
    pub fn exponentiation(&self, b: &Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;

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
                                return SqlValue::exponentiation(&a.to_type(SqlType::Int)?, &b);
                            }
                        },
                    }))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Int { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: match v1.checked_pow((*v2).try_into()?) {
                            Some(val) => val,
                            None => {
                                return SqlValue::exponentiation(&a.to_type(SqlType::BigInt)?, &b);
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
                                    &a.to_type(SqlType::DoublePrecision)?,
                                    &b,
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

    #[inline]
    pub fn bitwise_and(&self, b: &Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;

        if !(a.is_int() && b.is_int()) {
            return Err(SqlTypeError::TypeMismatchError(
                "Can only apply bitwise and to integer types".to_string(),
            ));
        }
        match (&a, &b) {
            (SqlValue::Numeric(ref num1), SqlValue::Numeric(ref num2)) => match (num1, num2) {
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::SmallInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::SmallInt { value: v1 & v2 }))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Int { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int { value: v1 & v2 }))
                }
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt { value: v1 & v2 }))
                }
                _ => Err(SqlTypeError::TypeMismatchError(
                    "Type mismatch for bitwise and".to_string(),
                )),
            },
            _ => Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for bitwise and".to_string(),
            )),
        }
    }

    #[inline]
    pub fn bitwise_or(&self, b: &Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;

        if !(a.is_int() && b.is_int()) {
            return Err(SqlTypeError::TypeMismatchError(
                "Can only apply bitwise or to integer types".to_string(),
            ));
        }
        match (&a, &b) {
            (SqlValue::Numeric(ref num1), SqlValue::Numeric(ref num2)) => match (num1, num2) {
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::SmallInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::SmallInt { value: v1 | v2 }))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Int { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int { value: v1 | v2 }))
                }
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt { value: v1 | v2 }))
                }
                _ => Err(SqlTypeError::TypeMismatchError(
                    "Type mismatch for bitwise or".to_string(),
                )),
            },
            _ => Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for bitwise or".to_string(),
            )),
        }
    }

    #[inline]
    pub fn bitwise_xor(&self, b: &Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;

        if !(a.is_int() && b.is_int()) {
            return Err(SqlTypeError::TypeMismatchError(
                "Can only apply bitwise xor to integer types".to_string(),
            ));
        }
        match (&a, &b) {
            (SqlValue::Numeric(ref num1), SqlValue::Numeric(ref num2)) => match (num1, num2) {
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::SmallInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::SmallInt { value: v1 ^ v2 }))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Int { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int { value: v1 ^ v2 }))
                }
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt { value: v1 ^ v2 }))
                }
                _ => Err(SqlTypeError::TypeMismatchError(
                    "Type mismatch for bitwise xor".to_string(),
                )),
            },
            _ => Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for bitwise xor".to_string(),
            )),
        }
    }

    #[inline]
    pub fn bitwise_shift_left(&self, b: &Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;

        if !(a.is_int() && b.is_int()) {
            return Err(SqlTypeError::TypeMismatchError(
                "Can only apply bitwise shift left to integer types".to_string(),
            ));
        }
        match (&a, &b) {
            (SqlValue::Numeric(ref num1), SqlValue::Numeric(ref num2)) => match (num1, num2) {
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::SmallInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::SmallInt { value: v1 << v2 }))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Int { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int { value: v1 << v2 }))
                }
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt { value: v1 << v2 }))
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

    #[inline]
    pub fn bitwise_shift_right(&self, b: &Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;

        if !(a.is_int() && b.is_int()) {
            return Err(SqlTypeError::TypeMismatchError(
                "Can only apply bitwise shift left to integer types".to_string(),
            ));
        }
        match (&a, &b) {
            (SqlValue::Numeric(ref num1), SqlValue::Numeric(ref num2)) => match (num1, num2) {
                (SqlNumeric::SmallInt { value: v1 }, SqlNumeric::SmallInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::SmallInt { value: v1 >> v2 }))
                }
                (SqlNumeric::Int { value: v1 }, SqlNumeric::Int { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int { value: v1 >> v2 }))
                }
                (SqlNumeric::BigInt { value: v1 }, SqlNumeric::BigInt { value: v2 }) => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt { value: v1 >> v2 }))
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

    #[inline]
    pub fn factorial(&self) -> Result<Self, SqlTypeError> {
        match self {
            SqlValue::Numeric(num) => match num {
                SqlNumeric::SmallInt { value } => Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                    value: factorial(*value as i64)?,
                })),
                SqlNumeric::Int { value } => Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                    value: factorial(*value as i64)?,
                })),
                SqlNumeric::BigInt { value } => Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                    value: factorial(*value as i64)?,
                })),
                _ => Err(SqlTypeError::TypeMismatchError(
                    "Type mismatch for factorial".to_string(),
                )),
            },
            _ => Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for factorial".to_string(),
            )),
        }
    }

    #[inline]
    pub fn square_root(&self) -> Result<Self, SqlTypeError> {
        match self {
            SqlValue::Numeric(num) => match num {
                SqlNumeric::SmallInt { value } => {
                    if value < &0 {
                        return Err(SqlTypeError::OperationError(
                            "Can't find square root of negative number".to_string(),
                        ));
                    }
                    let value = (*value as f64).sqrt();
                    if value.is_nan() {
                        Err(SqlTypeError::OperationError("NaN".to_string()))
                    } else {
                        Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision { value }))
                    }
                }
                SqlNumeric::Int { value } => {
                    if value < &0 {
                        return Err(SqlTypeError::OperationError(
                            "Can't find square root of negative number".to_string(),
                        ));
                    }
                    let value = (*value as f64).sqrt();
                    if value.is_nan() {
                        Err(SqlTypeError::OperationError("NaN".to_string()))
                    } else {
                        Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision { value }))
                    }
                }
                SqlNumeric::BigInt { value } => {
                    if value < &0 {
                        return Err(SqlTypeError::OperationError(
                            "Can't find square root of negative number".to_string(),
                        ));
                    }
                    let value = (*value as f64).sqrt();
                    if value.is_nan() {
                        Err(SqlTypeError::OperationError("NaN".to_string()))
                    } else {
                        Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision { value }))
                    }
                }
                SqlNumeric::Real { value } => {
                    if value < &0. {
                        return Err(SqlTypeError::OperationError(
                            "Can't find square root of negative number".to_string(),
                        ));
                    }
                    let value = (*value as f64).sqrt();
                    if value.is_nan() {
                        Err(SqlTypeError::OperationError("NaN".to_string()))
                    } else {
                        Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision { value }))
                    }
                }
                SqlNumeric::DoublePrecision { value } => {
                    if value < &0. {
                        return Err(SqlTypeError::OperationError(
                            "Can't find square root of negative number".to_string(),
                        ));
                    }
                    let value = (*value as f64).sqrt();
                    if value.is_nan() {
                        Err(SqlTypeError::OperationError("NaN".to_string()))
                    } else {
                        Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision { value }))
                    }
                }
            },
            _ => Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for square root".to_string(),
            )),
        }
    }

    #[inline]
    pub fn cube_root(&self) -> Result<Self, SqlTypeError> {
        match self {
            SqlValue::Numeric(num) => match num {
                SqlNumeric::SmallInt { value } => {
                    let value = (*value as f64).cbrt();
                    if value.is_nan() {
                        Err(SqlTypeError::OperationError("NaN".to_string()))
                    } else {
                        Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision { value }))
                    }
                }
                SqlNumeric::Int { value } => {
                    let value = (*value as f64).cbrt();
                    if value.is_nan() {
                        Err(SqlTypeError::OperationError("NaN".to_string()))
                    } else {
                        Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision { value }))
                    }
                }
                SqlNumeric::BigInt { value } => {
                    let value = (*value as f64).cbrt();
                    if value.is_nan() {
                        Err(SqlTypeError::OperationError("NaN".to_string()))
                    } else {
                        Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision { value }))
                    }
                }
                SqlNumeric::Real { value } => {
                    let value = (*value as f64).cbrt();
                    if value.is_nan() {
                        Err(SqlTypeError::OperationError("NaN".to_string()))
                    } else {
                        Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision { value }))
                    }
                }
                SqlNumeric::DoublePrecision { value } => {
                    let value = (*value as f64).cbrt();
                    if value.is_nan() {
                        Err(SqlTypeError::OperationError("NaN".to_string()))
                    } else {
                        Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision { value }))
                    }
                }
            },
            _ => Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for cube root".to_string(),
            )),
        }
    }

    #[inline]
    pub fn abs(&self) -> Result<Self, SqlTypeError> {
        match self {
            SqlValue::Numeric(num) => match num {
                SqlNumeric::SmallInt { value } => Ok(SqlValue::Numeric(SqlNumeric::SmallInt {
                    value: value.abs(),
                })),
                SqlNumeric::Int { value } => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int { value: value.abs() }))
                }
                SqlNumeric::BigInt { value } => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt { value: value.abs() }))
                }
                SqlNumeric::Real { value } => {
                    Ok(SqlValue::Numeric(SqlNumeric::Real { value: value.abs() }))
                }
                SqlNumeric::DoublePrecision { value } => {
                    Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision {
                        value: value.abs(),
                    }))
                }
            },
            _ => Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for absolute value".to_string(),
            )),
        }
    }

    #[inline]
    pub fn minus(&self) -> Result<Self, SqlTypeError> {
        match self {
            SqlValue::Numeric(num) => match num {
                SqlNumeric::SmallInt { value } => {
                    Ok(SqlValue::Numeric(SqlNumeric::SmallInt { value: -value }))
                }
                SqlNumeric::Int { value } => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int { value: -value }))
                }
                SqlNumeric::BigInt { value } => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt { value: -value }))
                }
                SqlNumeric::Real { value } => {
                    Ok(SqlValue::Numeric(SqlNumeric::Real { value: -value }))
                }
                SqlNumeric::DoublePrecision { value } => {
                    Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision {
                        value: -value,
                    }))
                }
            },
            _ => Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for minus".to_string(),
            )),
        }
    }

    #[inline]
    pub fn bitwise_not(&self) -> Result<Self, SqlTypeError> {
        match self {
            SqlValue::Numeric(num) => match num {
                SqlNumeric::SmallInt { value } => {
                    Ok(SqlValue::Numeric(SqlNumeric::SmallInt { value: !value }))
                }
                SqlNumeric::Int { value } => {
                    Ok(SqlValue::Numeric(SqlNumeric::Int { value: !value }))
                }
                SqlNumeric::BigInt { value } => {
                    Ok(SqlValue::Numeric(SqlNumeric::BigInt { value: !value }))
                }
                _ => Err(SqlTypeError::TypeMismatchError(
                    "Type mismatch for bitwise not".to_string(),
                )),
            },
            _ => Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for bitwise not".to_string(),
            )),
        }
    }

    #[inline]
    pub fn not(&self) -> Result<Self, SqlTypeError> {
        match self {
            SqlValue::Boolean(val) => Ok(SqlValue::Boolean(!val)),
            _ => Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for cube root".to_string(),
            )),
        }
    }

    #[inline]
    pub fn concat(&self, b: &Self) -> Result<Self, SqlTypeError> {
        let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;
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

    #[inline]
    pub fn equals(&self, b: &Self) -> Result<Self, SqlTypeError> {
        if self.is_null() || b.is_null() {
            Ok(SqlValue::Null)
        } else {
            let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;
            Ok(SqlValue::Boolean(a == b))
        }
    }

    #[inline]
    pub fn not_equal(&self, b: &Self) -> Result<Self, SqlTypeError> {
        if self.is_null() || b.is_null() {
            Ok(SqlValue::Null)
        } else {
            let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;
            Ok(SqlValue::Boolean(a != b))
        }
    }

    #[inline]
    pub fn less_than(&self, b: &Self) -> Result<Self, SqlTypeError> {
        if self.is_null() || b.is_null() {
            Ok(SqlValue::Null)
        } else {
            let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;
            Ok(SqlValue::Boolean(a < b))
        }
    }

    #[inline]
    pub fn less_than_or_equals(&self, b: &Self) -> Result<Self, SqlTypeError> {
        if self.is_null() || b.is_null() {
            Ok(SqlValue::Null)
        } else {
            let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;
            Ok(SqlValue::Boolean(a <= b))
        }
    }

    #[inline]
    pub fn greater_than(&self, b: &Self) -> Result<Self, SqlTypeError> {
        if self.is_null() || b.is_null() {
            Ok(SqlValue::Null)
        } else {
            let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;
            Ok(SqlValue::Boolean(a > b))
        }
    }

    #[inline]
    pub fn greater_than_or_equals(&self, b: &Self) -> Result<Self, SqlTypeError> {
        if self.is_null() || b.is_null() {
            Ok(SqlValue::Null)
        } else {
            let (a, b) = SqlValue::implicist_cast_to_matching_types(self, b)?;
            Ok(SqlValue::Boolean(a >= b))
        }
    }

    #[inline]
    pub fn and(&self, b: &Self) -> Result<Self, SqlTypeError> {
        if let (SqlValue::Boolean(a), SqlValue::Boolean(b)) = (self, b) {
            Ok(SqlValue::Boolean(*a && *b))
        } else {
            Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for concat".to_string(),
            ))
        }
    }

    #[inline]
    pub fn or(&self, b: &Self) -> Result<Self, SqlTypeError> {
        if let (SqlValue::Boolean(a), SqlValue::Boolean(b)) = (self, b) {
            Ok(SqlValue::Boolean(*a || *b))
        } else {
            Err(SqlTypeError::TypeMismatchError(
                "Type mismatch for concat".to_string(),
            ))
        }
    }

    #[inline]
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
            SqlValue::Type(_) => SqlType::Type,
            SqlValue::Null => SqlType::Null,
        }
    }

    #[inline]
    pub fn explicit_cast_to_type(&self, typ: SqlType) -> Result<Self, SqlTypeError> {
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
                    SqlNumeric::BigInt { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::BigInt { value: *value }))
                    }
                    SqlNumeric::Real { value } => Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: *value as i64,
                    })),
                    SqlNumeric::DoublePrecision { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                            value: *value as i64,
                        }))
                    }
                },
                SqlValue::Text(text) => match text {
                    SqlText::Text { value } => Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: value.parse()?,
                    })),
                    SqlText::VarChar {
                        value,
                        len: _,
                        maxlen: _,
                    } => Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: value.parse()?,
                    })),
                    SqlText::Char { value, len: _ } => Ok(SqlValue::Numeric(SqlNumeric::BigInt {
                        value: value.parse()?,
                    })),
                },
                SqlValue::Boolean(value) => {
                    if *value {
                        Ok(SqlValue::Numeric(SqlNumeric::BigInt { value: 1 }))
                    } else {
                        Ok(SqlValue::Numeric(SqlNumeric::BigInt { value: 0 }))
                    }
                }
                _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
            },
            SqlType::Int => match self {
                SqlValue::Numeric(num) => match num {
                    SqlNumeric::SmallInt { value } => Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: i32::from(*value),
                    })),
                    SqlNumeric::Int { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::Int { value: *value }))
                    }
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
                SqlValue::Text(text) => match text {
                    SqlText::Text { value } => Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: value.parse()?,
                    })),
                    SqlText::VarChar {
                        value,
                        len: _,
                        maxlen: _,
                    } => Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: value.parse()?,
                    })),
                    SqlText::Char { value, len: _ } => Ok(SqlValue::Numeric(SqlNumeric::Int {
                        value: value.parse()?,
                    })),
                },
                SqlValue::Boolean(value) => {
                    if *value {
                        Ok(SqlValue::Numeric(SqlNumeric::Int { value: 1 }))
                    } else {
                        Ok(SqlValue::Numeric(SqlNumeric::Int { value: 0 }))
                    }
                }
                _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
            },
            SqlType::SmallInt => match self {
                SqlValue::Numeric(num) => match num {
                    SqlNumeric::SmallInt { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::SmallInt { value: *value }))
                    }
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
                SqlValue::Text(text) => match text {
                    SqlText::Text { value } => Ok(SqlValue::Numeric(SqlNumeric::SmallInt {
                        value: value.parse()?,
                    })),
                    SqlText::VarChar {
                        value,
                        len: _,
                        maxlen: _,
                    } => Ok(SqlValue::Numeric(SqlNumeric::SmallInt {
                        value: value.parse()?,
                    })),
                    SqlText::Char { value, len: _ } => {
                        Ok(SqlValue::Numeric(SqlNumeric::SmallInt {
                            value: value.parse()?,
                        }))
                    }
                },
                SqlValue::Boolean(value) => {
                    if *value {
                        Ok(SqlValue::Numeric(SqlNumeric::SmallInt { value: 1 }))
                    } else {
                        Ok(SqlValue::Numeric(SqlNumeric::SmallInt { value: 0 }))
                    }
                }
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
                    SqlNumeric::Real { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::Real { value: *value }))
                    }
                    SqlNumeric::DoublePrecision { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::Real {
                            value: *value as f32,
                        }))
                    }
                },
                SqlValue::Text(text) => match text {
                    SqlText::Text { value } => Ok(SqlValue::Numeric(SqlNumeric::Real {
                        value: value.parse()?,
                    })),
                    SqlText::VarChar {
                        value,
                        len: _,
                        maxlen: _,
                    } => Ok(SqlValue::Numeric(SqlNumeric::Real {
                        value: value.parse()?,
                    })),
                    SqlText::Char { value, len: _ } => Ok(SqlValue::Numeric(SqlNumeric::Real {
                        value: value.parse()?,
                    })),
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
                            value: *value,
                        }))
                    }
                },
                SqlValue::Text(text) => match text {
                    SqlText::Text { value } => Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision {
                        value: value.parse()?,
                    })),
                    SqlText::VarChar {
                        value,
                        len: _,
                        maxlen: _,
                    } => Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision {
                        value: value.parse()?,
                    })),
                    SqlText::Char { value, len: _ } => {
                        Ok(SqlValue::Numeric(SqlNumeric::DoublePrecision {
                            value: value.parse()?,
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
                SqlValue::Numeric(num) => match num {
                    SqlNumeric::SmallInt { value } => Ok(SqlValue::Text(SqlText::Text {
                        value: value.to_string(),
                    })),
                    SqlNumeric::Int { value } => Ok(SqlValue::Text(SqlText::Text {
                        value: value.to_string(),
                    })),
                    SqlNumeric::BigInt { value } => Ok(SqlValue::Text(SqlText::Text {
                        value: value.to_string(),
                    })),
                    SqlNumeric::Real { value } => Ok(SqlValue::Text(SqlText::Text {
                        value: value.to_string(),
                    })),
                    SqlNumeric::DoublePrecision { value } => Ok(SqlValue::Text(SqlText::Text {
                        value: value.to_string(),
                    })),
                },
                SqlValue::Boolean(value) => match value {
                    true => Ok(SqlValue::Text(SqlText::Text {
                        value: "true".to_string(),
                    })),
                    false => Ok(SqlValue::Text(SqlText::Text {
                        value: "false".to_string(),
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
                SqlValue::Numeric(num) => match num {
                    SqlNumeric::SmallInt { value } => {
                        let value = value.to_string();
                        Ok(SqlValue::Text(SqlText::VarChar {
                            len: value.len(),
                            maxlen: value.len(),
                            value,
                        }))
                    }
                    SqlNumeric::Int { value } => {
                        let value = value.to_string();
                        Ok(SqlValue::Text(SqlText::VarChar {
                            len: value.len(),
                            maxlen: value.len(),
                            value,
                        }))
                    }
                    SqlNumeric::BigInt { value } => {
                        let value = value.to_string();
                        Ok(SqlValue::Text(SqlText::VarChar {
                            len: value.len(),
                            maxlen: value.len(),
                            value,
                        }))
                    }
                    SqlNumeric::Real { value } => {
                        let value = value.to_string();
                        Ok(SqlValue::Text(SqlText::VarChar {
                            len: value.len(),
                            maxlen: value.len(),
                            value,
                        }))
                    }
                    SqlNumeric::DoublePrecision { value } => {
                        let value = value.to_string();
                        Ok(SqlValue::Text(SqlText::VarChar {
                            len: value.len(),
                            maxlen: value.len(),
                            value,
                        }))
                    }
                },
                SqlValue::Boolean(value) => match value {
                    true => Ok(SqlValue::Text(SqlText::VarChar {
                        len: 4,
                        maxlen: 4,
                        value: "true".to_string(),
                    })),
                    false => Ok(SqlValue::Text(SqlText::VarChar {
                        len: 5,
                        maxlen: 5,
                        value: "false".to_string(),
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
                SqlValue::Numeric(num) => match num {
                    SqlNumeric::SmallInt { value } => {
                        let value = value.to_string();
                        Ok(SqlValue::Text(SqlText::Char {
                            len: value.len(),
                            value,
                        }))
                    }
                    SqlNumeric::Int { value } => {
                        let value = value.to_string();
                        Ok(SqlValue::Text(SqlText::Char {
                            len: value.len(),
                            value,
                        }))
                    }
                    SqlNumeric::BigInt { value } => {
                        let value = value.to_string();
                        Ok(SqlValue::Text(SqlText::Char {
                            len: value.len(),
                            value,
                        }))
                    }
                    SqlNumeric::Real { value } => {
                        let value = value.to_string();
                        Ok(SqlValue::Text(SqlText::Char {
                            len: value.len(),
                            value,
                        }))
                    }
                    SqlNumeric::DoublePrecision { value } => {
                        let value = value.to_string();
                        Ok(SqlValue::Text(SqlText::Char {
                            len: value.len(),
                            value,
                        }))
                    }
                },
                SqlValue::Boolean(value) => match value {
                    true => Ok(SqlValue::Text(SqlText::Char {
                        len: 4,
                        value: "true".to_string(),
                    })),
                    false => Ok(SqlValue::Text(SqlText::Char {
                        len: 5,
                        value: "false".to_string(),
                    })),
                },
                _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
            },
            SqlType::Boolean => match self {
                SqlValue::Boolean(val) => Ok(SqlValue::Boolean(*val)),
                SqlValue::Numeric(num) => match num {
                    SqlNumeric::SmallInt { value } => Ok(SqlValue::Boolean(*value != 0)),
                    SqlNumeric::Int { value } => Ok(SqlValue::Boolean(*value != 0)),
                    SqlNumeric::BigInt { value } => Ok(SqlValue::Boolean(*value != 0)),
                    _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
                },
                SqlValue::Text(text) => match text {
                    SqlText::Text { value } => match value.as_str() {
                        "true" => Ok(SqlValue::Boolean(true)),
                        "false" => Ok(SqlValue::Boolean(false)),
                        _ => Err(SqlTypeError::ConversionError(format!(
                            "Invalid input syntax for type boolean: \"{}\"",
                            value
                        ))),
                    },
                    SqlText::VarChar {
                        value,
                        len: _,
                        maxlen: _,
                    } => match value.as_str() {
                        "true" => Ok(SqlValue::Boolean(true)),
                        "false" => Ok(SqlValue::Boolean(false)),
                        _ => Err(SqlTypeError::ConversionError(format!(
                            "Invalid input syntax for type boolean: \"{}\"",
                            value
                        ))),
                    },
                    SqlText::Char { value, len: _ } => match value.as_str() {
                        "true" => Ok(SqlValue::Boolean(true)),
                        "false" => Ok(SqlValue::Boolean(false)),
                        _ => Err(SqlTypeError::ConversionError(format!(
                            "Invalid input syntax for type boolean: \"{}\"",
                            value
                        ))),
                    },
                },
                _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
            },
            SqlType::Null => Ok(SqlValue::Null),
            SqlType::Type => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
        }
    }

    #[inline]
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
                    SqlNumeric::BigInt { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::BigInt { value: *value }))
                    }
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
                    SqlNumeric::Int { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::Int { value: *value }))
                    }
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
                    SqlNumeric::SmallInt { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::SmallInt { value: *value }))
                    }
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
                    SqlNumeric::Real { value } => {
                        Ok(SqlValue::Numeric(SqlNumeric::Real { value: *value }))
                    }
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
                            value: *value,
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
            SqlType::Type => match self {
                SqlValue::Type(typ) => Ok(SqlValue::Type(*typ)),
                _ => Err(SqlTypeError::TypeMismatchError("Type mismatch".to_string())),
            },
        }
    }
}

impl SqlNumeric {
    #[inline]
    pub fn parse(data: &str) -> Result<Self, SqlTypeError> {
        if let Ok(value) = data.parse::<i16>() {
            Ok(SqlNumeric::SmallInt { value })
        } else if let Ok(value) = data.parse::<i32>() {
            Ok(SqlNumeric::Int { value })
        } else if let Ok(value) = data.parse::<i64>() {
            Ok(SqlNumeric::BigInt { value })
        } else if let Ok(value) = data.parse::<f32>() {
            match (data.parse::<f32>(), data.parse::<f64>()) {
                (Ok(v1), Ok(v2)) => {
                    if (v1 as f64 - v2).abs() > 0. {
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

    #[inline]
    pub fn parse_small_int(data: String) -> Result<Self, SqlTypeError> {
        if let Ok(value) = data.parse::<i16>() {
            Ok(SqlNumeric::SmallInt { value })
        } else {
            Err(SqlTypeError::ParseError(
                "Failed to parse bytes to small int.".to_string(),
            ))
        }
    }

    #[inline]
    pub fn parse_int(data: String) -> Result<Self, SqlTypeError> {
        if let Ok(value) = data.parse::<i32>() {
            Ok(SqlNumeric::Int { value })
        } else {
            Err(SqlTypeError::ParseError(
                "Failed to parse bytes to int.".to_string(),
            ))
        }
    }

    #[inline]
    pub fn parse_big_int(data: String) -> Result<Self, SqlTypeError> {
        if let Ok(value) = data.parse::<i64>() {
            Ok(SqlNumeric::BigInt { value })
        } else {
            Err(SqlTypeError::ParseError(
                "Failed to parse bytes to big int.".to_string(),
            ))
        }
    }

    #[inline]
    pub fn parse_real(data: String) -> Result<Self, SqlTypeError> {
        if let Ok(value) = data.parse::<f32>() {
            Ok(SqlNumeric::Real { value })
        } else {
            Err(SqlTypeError::ParseError(
                "Failed to parse bytes to real.".to_string(),
            ))
        }
    }

    #[inline]
    pub fn parse_double_precision(data: String) -> Result<Self, SqlTypeError> {
        if let Ok(value) = data.parse::<f64>() {
            Ok(SqlNumeric::DoublePrecision { value })
        } else {
            Err(SqlTypeError::ParseError(
                "Failed to parse bytes to double precision.".to_string(),
            ))
        }
    }

    #[inline]
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

    #[inline]
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

    #[inline]
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

    #[inline]
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

    #[inline]
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
    #[inline]
    pub fn parse_text(data: String) -> Result<Self, SqlTypeError> {
        Ok(SqlText::Text { value: data })
    }

    #[inline]
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

    #[inline]
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

    #[inline]
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

    #[inline]
    pub fn decode_varchar(data: &MemoryCell) -> Result<Self, SqlTypeError> {
        SqlText::decode_text(data)
    }

    #[inline]
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
                SqlValue::Type(typ) => typ.to_string(),
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

impl From<std::num::ParseIntError> for SqlTypeError {
    fn from(err: std::num::ParseIntError) -> Self {
        SqlTypeError::ParseError(err.to_string())
    }
}

impl From<std::num::ParseFloatError> for SqlTypeError {
    fn from(err: std::num::ParseFloatError) -> Self {
        SqlTypeError::ParseError(err.to_string())
    }
}
