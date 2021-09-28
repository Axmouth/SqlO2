use crate::sql_types::SqlType;

use super::lexer::*;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Ast {
    pub statements: Vec<Statement>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Statement {
    SelectStatement(SelectStatement),
    CreateTableStatement(CreateTableStatement),
    CreateIndexStatement(CreateIndexStatement),
    DropTableStatement(DropTableStatement),
    InsertStatement(InsertStatement),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct OrderByClause {
    pub asc: bool,
    pub exp: Expression,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct JoinClause {
    pub kind: JoinKind,
    pub source: RowDataSource,
    pub on: Expression,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum JoinKind {
    Inner,
    FullOuter,
    LeftOuter,
    RightOuter,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum RowDataSource {
    SubSelect {
        select: SelectStatement,
        as_clause: String,
        joins: Vec<JoinClause>,
    },
    Table {
        table_name: String,
        as_clause: Option<String>,
        joins: Vec<JoinClause>,
    },
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct TableColumn {
    pub col_name: String,
    pub table_name: Option<String>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ProcessedTableColumn {
    pub col_name: Option<String>,
    pub col_idx: usize,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct InsertStatement {
    pub table: String,
    pub values: Vec<Expression>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CreateTableStatement {
    pub name: String,
    pub cols: Vec<ColumnDefinition>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ColumnDefinition {
    pub name: String,
    pub data_type: SqlType,
    pub is_primary_key: bool,
}

#[derive(Clone, Eq, PartialEq, Debug, Default)]
pub struct SelectStatement {
    pub items: Vec<SelectItem>,
    pub from: Vec<RowDataSource>,
    pub where_clause: Expression,
    pub is_distinct: bool,
    pub order_by: Option<OrderByClause>,
    pub limit: Option<usize>,
    pub offset: Option<usize>,
}

impl SelectStatement {
    pub fn new() -> Self {
        SelectStatement {
            items: Vec::with_capacity(10),
            from: vec![],
            where_clause: Expression::new(),
            is_distinct: false,
            order_by: None,
            limit: None,
            offset: None,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CreateIndexStatement {
    pub name: String,
    pub is_unique: bool,
    pub is_primary_key: bool,
    pub table: String,
    pub expression: Expression,
}

impl CreateIndexStatement {
    pub fn generate_code(&self) -> Result<String, String> {
        let unique = if self.is_unique { " UNIQUE" } else { "" };
        Ok(format!(
            "CREATE{} INDEX \"{}\" ON \"{}\" ({});",
            unique,
            self.name,
            self.table,
            self.expression.generate_code()?
        ))
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CreateConstraintStatement {
    pub name: String,
    pub constraint: ConstraintType,
    pub table: String,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum ConstraintType {
    Foreign { references: Vec<(String, String)> },
    Check { expression: Expression },
    Unique { expression: Expression },
    Exclude { expression: Expression },
}

impl CreateConstraintStatement {
    pub fn generate_code(&self) -> Result<String, String> {
        let unique = if false { " UNIQUE" } else { "" };
        Ok(format!(
            "CREATE{} CONSTRAINT \"{}\" ON \"{}\" ();",
            unique, self.name, self.table,
        ))
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct DropTableStatement {
    pub name: String,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Expression {
    Literal(LiteralExpression),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    SubSelect(Box<SelectStatement>),
    TableColumn(TableColumn),
    ProcessedTableColumn(ProcessedTableColumn),
    Cast { data: Box<Expression>, typ: SqlType },
    Empty,
}

impl Default for Expression {
    fn default() -> Self {
        Expression::Empty
    }
}

impl Expression {
    pub fn new() -> Expression {
        Expression::Empty
    }

    pub fn new_literal_id(value: String) -> Expression {
        Expression::Literal(LiteralExpression::Identifier(value))
    }
    pub fn new_literal_num(value: String) -> Expression {
        Expression::Literal(LiteralExpression::Numeric(value))
    }
    pub fn new_literal_string(value: String) -> Expression {
        Expression::Literal(LiteralExpression::String(value))
    }
    pub fn new_literal_bool(value: String) -> Expression {
        Expression::Literal(LiteralExpression::Bool(matches!(
            value.to_lowercase().as_str(),
            "true" | "t"
        )))
    }
    pub fn new_literal_null() -> Expression {
        Expression::Literal(LiteralExpression::Null)
    }

    pub fn generate_code(&self) -> Result<String, String> {
        match self {
            Expression::Literal(value) => value.generate_code(),
            Expression::Binary(value) => value.generate_code(),
            Expression::Unary(value) => value.generate_code(),
            // Expression::SubSelect(value) => value.generate_code(),
            Expression::TableColumn(value) => Ok(value.col_name.clone()),
            Expression::Cast { data, typ } => data
                .generate_code()
                .map(|s| format!("CAST({} AS {})", s, typ.to_string())),
            Expression::Empty => Ok("".to_string()),
            _ => Err("Unknown Expression Kind".to_string()),
        }
    }

    #[inline]
    pub fn is_unary(&self) -> bool {
        matches!(self, Expression::Unary(_))
    }

    #[inline]
    pub fn is_binary(&self) -> bool {
        matches!(self, Expression::Binary(_))
    }

    #[inline]
    pub fn is_literal(&self) -> bool {
        matches!(self, Expression::Literal(_))
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        matches!(self, Expression::Empty)
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum LiteralExpression {
    String(String),
    Identifier(String),
    Numeric(String),
    Bool(bool),
    Null,
}

impl LiteralExpression {
    pub fn generate_code(&self) -> Result<String, String> {
        match self {
            LiteralExpression::String(value) => Ok(format!("'{}'", value)),
            LiteralExpression::Identifier(value) => Ok(format!("\"{}\"", value)),
            LiteralExpression::Numeric(value) => Ok(value.clone()),
            LiteralExpression::Bool(value) => Ok(if *value {
                TRUE_KEYWORD.to_string()
            } else {
                FALSE_KEYWORD.to_string()
            }),
            LiteralExpression::Null => Ok(NULL_KEYWORD.to_string()),
        }
    }

    pub fn from_token(token: &Token) -> Option<LiteralExpression> {
        match token {
            Token::StringValue { value } => Some(LiteralExpression::String(value.to_string())),
            Token::IdentifierValue { value } => {
                Some(LiteralExpression::Identifier(value.to_string()))
            }
            Token::NumericValue { value } => Some(LiteralExpression::Numeric(value.to_string())),
            Token::BoolValue { value } => Some(LiteralExpression::Bool(*value)),
            Token::Null => Some(LiteralExpression::Null),
            _ => None,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Operand {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    And,
    Or,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Not,
    SquareRoot,
    CubeRoot,
    Factorial,
    FactorialPrefix,
    AbsoluteValue,
    Exponentiation,
    Concat,
    Is,
    IsNot,
    In,
    NotIn,
    Like,
    NotLike,
    Between,
    NotBetween,
    Exists,
    NotExists,
    Null,
    NotNull,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    BitwiseShiftLeft,
    BitwiseShiftRight,
    BitwiseShiftRightZeroFill,
}

impl Operand {
    pub fn generate_code(&self) -> String {
        match self {
            Operand::Add => PLUS_SYMBOL,
            Operand::Subtract => MINUS_SYMBOL,
            Operand::Multiply => ASTERISK_SYMBOL,
            Operand::Divide => SLASH_SYMBOL,
            Operand::Modulo => MODULO_SYMBOL,
            Operand::And => AND_KEYWORD,
            Operand::Or => OR_KEYWORD,
            Operand::Equal => EQUAL_SYMBOL,
            Operand::NotEqual => NOT_EQUAL_SYMBOL,
            Operand::GreaterThan => GREATER_THAN_SYMBOL,
            Operand::GreaterThanOrEqual => GREATER_THAN_OR_EQUAL_SYMBOL,
            Operand::LessThan => LESS_THAN_SYMBOL,
            Operand::LessThanOrEqual => LESS_THAN_OR_EQUAL_SYMBOL,
            Operand::Not => NOT_KEYWORD,
            Operand::SquareRoot => SQUARE_ROOT_SYMBOL,
            Operand::CubeRoot => CUBE_ROOT_SYMBOL,
            Operand::Factorial => FACTORIAL_SYMBOL,
            Operand::FactorialPrefix => FACTORIAL_PREFIX_SYMBOL,
            Operand::AbsoluteValue => ABS_SYMBOL,
            Operand::Exponentiation => EXPONENTIATION_SYMBOL,
            Operand::Concat => CONCAT_SYMBOL,
            Operand::Is => IS_KEYWORD,
            Operand::IsNot => "IS NOT",
            Operand::In => "IN",
            Operand::NotIn => "NOT IN",
            Operand::Like => LIKE_KEYWORD,
            Operand::NotLike => "NOT LIKE",
            Operand::Between => "BETWEEN",
            Operand::NotBetween => "NOT BETWEEN",
            Operand::Exists => "EXISTS",
            Operand::NotExists => "NOT EXISTS",
            Operand::Null => NULL_KEYWORD,
            Operand::NotNull => "NOT NULL",
            Operand::BitwiseAnd => BITWISE_AND_SYMBOL,
            Operand::BitwiseOr => BITWISE_OR_SYMBOL,
            Operand::BitwiseXor => BITWISE_XOR_SYMBOL,
            Operand::BitwiseNot => BITWISE_NOT_SYMBOL,
            Operand::BitwiseShiftLeft => BITWISE_SHIFT_LEFT_SYMBOL,
            Operand::BitwiseShiftRight => BITWISE_SHIFT_RIGHT_SYMBOL,
            Operand::BitwiseShiftRightZeroFill => ">>",
        }
        .to_string()
    }

    pub fn from_token(token: &Token) -> Option<Operand> {
        match token {
            Token::Plus => Some(Operand::Add),
            Token::Minus => Some(Operand::Subtract),
            Token::Asterisk => Some(Operand::Multiply),
            Token::Slash => Some(Operand::Divide),
            Token::Modulo => Some(Operand::Modulo),
            Token::And => Some(Operand::And),
            Token::Or => Some(Operand::Or),
            Token::Equal => Some(Operand::Equal),
            Token::NotEqual => Some(Operand::NotEqual),
            Token::GreaterThan => Some(Operand::GreaterThan),
            Token::GreaterThanOrEqual => Some(Operand::GreaterThanOrEqual),
            Token::LessThan => Some(Operand::LessThan),
            Token::LessThanOrEqual => Some(Operand::LessThanOrEqual),
            Token::Not => Some(Operand::Not),
            Token::SquareRoot => Some(Operand::SquareRoot),
            Token::CubeRoot => Some(Operand::CubeRoot),
            Token::Factorial => Some(Operand::Factorial),
            Token::FactorialPrefix => Some(Operand::FactorialPrefix),
            Token::AbsoluteValue => Some(Operand::AbsoluteValue),
            Token::Exponentiation => Some(Operand::Exponentiation),
            Token::Concat => Some(Operand::Concat),
            Token::Is => Some(Operand::Is),
            // Token::IsNot => Some(Operand::IsNot),
            // Token::In => Some(Operand::In),
            // Token::NotIn => Some(Operand::NotIn),
            Token::Like => Some(Operand::Like),
            // Token::NotLike => Some(Operand::NotLike),
            // Token::Between => Some(Operand::Between),
            // Token::NotBetween => Some(Operand::NotBetween),
            // Token::Exists => Some(Operand::Exists),
            // Token::NotExists => Some(Operand::NotExists),
            // Token::Null => Some(Operand::Null),
            // Token::NotNull => Some(Operand::NotNull),
            Token::BitwiseAnd => Some(Operand::BitwiseAnd),
            Token::BitwiseOr => Some(Operand::BitwiseOr),
            Token::BitwiseXor => Some(Operand::BitwiseXor),
            Token::BitwiseNot => Some(Operand::BitwiseNot),
            Token::BitwiseShiftLeft => Some(Operand::BitwiseShiftLeft),
            Token::BitwiseShiftRight => Some(Operand::BitwiseShiftRight),
            // Token::BitwiseShiftRightZeroFill => Some(Operand::BitwiseShiftRightZeroFill),
            _ => None,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct BinaryExpression {
    pub first: Box<Expression>,
    pub second: Box<Expression>,
    pub operand: Operand,
}

impl BinaryExpression {
    pub fn generate_code(&self) -> Result<String, String> {
        Ok(format!(
            "({} {} {})",
            self.first.generate_code()?,
            self.operand.generate_code(),
            self.second.generate_code()?
        ))
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct UnaryExpression {
    pub first: Box<Expression>,
    pub operand: Operand,
}

impl UnaryExpression {
    pub fn generate_code(&self) -> Result<String, String> {
        Ok(format!(
            "({} {})",
            self.operand.generate_code(),
            self.first.generate_code()?,
        ))
    }
}

impl Token<'_> {
    pub fn generate_code(&self) -> String {
        match self {
            Token::And => "AND".to_string(),
            Token::As => "AS".to_string(),
            Token::Asterisk => "*".to_string(),
            Token::Bool => "BOOL".to_string(),
            Token::BoolValue { value } => {
                if *value {
                    "TRUE".to_string()
                } else {
                    "FALSE".to_string()
                }
            }
            Token::Comma => COMMA_SYMBOL.to_string(),
            Token::Concat => CONCAT_SYMBOL.to_string(),
            Token::Create => CREATE_KEYWORD.to_string(),
            Token::Drop => DROP_KEYWORD.to_string(),
            Token::Empty => "".to_string(),
            Token::Equal => EQUAL_SYMBOL.to_string(),
            Token::False => FALSE_KEYWORD.to_string(),
            Token::From => FROM_KEYWORD.to_string(),
            Token::GreaterThan => GREATER_THAN_SYMBOL.to_string(),
            Token::GreaterThanOrEqual => GREATER_THAN_OR_EQUAL_SYMBOL.to_string(),
            Token::IdentifierValue { value } => format!("\"{}\"", value),
            Token::Index => INDEX_KEYWORD.to_string(),
            Token::Insert => INSERT_KEYWORD.to_string(),
            Token::Int => INT_KEYWORD.to_string(),
            Token::Into => INTO_KEYWORD.to_string(),
            Token::Key => KEY_KEYWORD.to_string(),
            Token::LeftParenthesis => LEFT_PARENTHESIS_SYMBOL.to_string(),
            Token::LessThan => LESS_THAN_SYMBOL.to_string(),
            Token::LessThanOrEqual => LESS_THAN_OR_EQUAL_SYMBOL.to_string(),
            Token::Minus => MINUS_SYMBOL.to_string(),
            Token::NotEqual => NOT_EQUAL_SYMBOL.to_string(),
            Token::Null => NULL_KEYWORD.to_string(),
            Token::NumericValue { value } => value.to_string(),
            Token::On => ON_KEYWORD.to_string(),
            Token::Or => OR_KEYWORD.to_string(),
            Token::Not => NOT_KEYWORD.to_string(),
            Token::Plus => PLUS_SYMBOL.to_string(),
            Token::Slash => SLASH_SYMBOL.to_string(),
            Token::Modulo => MODULO_SYMBOL.to_string(),
            Token::Exponentiation => EXPONENTIATION_SYMBOL.to_string(),
            Token::SquareRoot => SQUARE_ROOT_SYMBOL.to_string(),
            Token::CubeRoot => CUBE_ROOT_SYMBOL.to_string(),
            Token::Factorial => FACTORIAL_SYMBOL.to_string(),
            Token::FactorialPrefix => FACTORIAL_PREFIX_SYMBOL.to_string(),
            Token::AbsoluteValue => ABS_SYMBOL.to_string(),
            Token::BitwiseAnd => BITWISE_AND_SYMBOL.to_string(),
            Token::BitwiseOr => BITWISE_OR_SYMBOL.to_string(),
            Token::BitwiseXor => BITWISE_XOR_SYMBOL.to_string(),
            Token::BitwiseNot => BITWISE_NOT_SYMBOL.to_string(),
            Token::BitwiseShiftLeft => BITWISE_SHIFT_LEFT_SYMBOL.to_string(),
            Token::BitwiseShiftRight => BITWISE_SHIFT_RIGHT_SYMBOL.to_string(),
            Token::Primary => PRIMARY_KEYWORD.to_string(),
            Token::RightParenthesis => RIGHT_PARENTHESIS_SYMBOL.to_string(),
            Token::Select => SELECT_KEYWORD.to_string(),
            Token::Semicolon => SEMICOLON_SYMBOL.to_string(),
            Token::StringValue { value } => format!("'{}'", value),
            Token::Table => TABLE_KEYWORD.to_string(),
            Token::Text => TEXT_KEYWORD.to_string(),
            Token::True => TRUE_KEYWORD.to_string(),
            Token::Unique => UNIQUE_KEYWORD.to_string(),
            Token::Values => VALUES_KEYWORD.to_string(),
            Token::Where => WHERE_KEYWORD.to_string(),
            Token::Alter => ALTER_KEYWORD.to_string(),
            Token::Delete => DELETE_KEYWORD.to_string(),
            Token::Update => UPDATE_KEYWORD.to_string(),
            Token::Join => JOIN_KEYWORD.to_string(),
            Token::Inner => INNER_KEYWORD.to_string(),
            Token::Right => RIGHT_KEYWORD.to_string(),
            Token::Left => LEFT_KEYWORD.to_string(),
            Token::Constraint => CONSTRAINT_KEYWORD.to_string(),
            Token::Foreign => FOREIGN_KEYWORD.to_string(),
            Token::Double => DOUBLE_KEYWORD.to_string(),
            Token::DoublePrecision => "DOUBLE PRECISION".to_string(),
            Token::Precision => PRECISION_KEYWORD.to_string(),
            Token::Real => REAL_KEYWORD.to_string(),
            Token::SmallInt => SMALLINT_KEYWORD.to_string(),
            Token::BigInt => BIGINT_KEYWORD.to_string(),
            Token::Varchar => VARCHAR_KEYWORD.to_string(),
            Token::Char => CHAR_KEYWORD.to_string(),
            Token::Is => IS_KEYWORD.to_string(),
            Token::TypeCast => TYPE_CAST_SYMBOL.to_string(),
            Token::Distinct => DISTINCT_KEYWORD.to_string(),
            Token::Order => ORDER_KEYWORD.to_string(),
            Token::By => BY_KEYWORD.to_string(),
            Token::OrderBy => "ORDER BY".to_string(),
            Token::Asc => ASC_KEYWORD.to_string(),
            Token::Desc => DESC_KEYWORD.to_string(),
            Token::Limit => LIMIT_KEYWORD.to_string(),
            Token::Offset => OFFSET_KEYWORD.to_string(),
            Token::Dot => DOT_SYMBOL.to_string(),
            Token::Outer => OUTER_KEYWORD.to_string(),
            Token::Full => FULL_KEYWORD.to_string(),
            Token::Like => LIKE_KEYWORD.to_string(),
            Token::Comment => "".to_string(),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Default)]
pub struct SelectItem {
    pub expression: Expression,
    pub as_clause: Option<String>,
    pub asterisk: bool,
}

impl SelectItem {
    pub fn new() -> Self {
        SelectItem {
            expression: Expression::new(),
            as_clause: None,
            asterisk: false,
        }
    }
}

#[cfg(test)]
mod ast_tests {
    use super::super::ast::*;
    use super::super::parser::*;

    struct ParseTest {
        ast: Ast,
        input: &'static str,
    }

    #[test]
    fn test_ast() {
        let parse_tests = vec![
            ParseTest {
                input: "INSERT INTO users VALUES (105, 'George');",
                ast: Ast {
                    statements: vec![Statement::InsertStatement(InsertStatement {
                        table: "users".to_owned(),
                        values: vec![
                            Expression::Literal(LiteralExpression::Numeric("105".to_owned())),
                            Expression::Literal(LiteralExpression::String( "George".to_owned())),
                        ],
                    })],
                },
            },
            ParseTest {
                input: "CREATE TABLE users (id INT, name TEXT);",
                ast: Ast {
                    statements: vec![Statement::CreateTableStatement(CreateTableStatement {
                        name: "users".to_owned(),
                        cols: vec![
                            ColumnDefinition {
                                name: "id".to_owned(),
                                data_type: SqlType::Int,
                                is_primary_key: false,
                            },
                            ColumnDefinition {
                                name: "name".to_owned(),
                                data_type: SqlType::Text,
                                is_primary_key: false,
                            },
                        ],
                    })],
                },
            },
            ParseTest {
                input: "SELECT id, name AS fullname FROM users;",
                ast: Ast {
                    statements: vec![Statement::SelectStatement(SelectStatement {
                        items: vec![
                            SelectItem {
                                asterisk: false,
                                as_clause: None,
                                expression: Expression::TableColumn(TableColumn {
                                    col_name: "id".to_owned(),
                                    table_name: None,
                                }),
                            },
                            SelectItem {
                                asterisk: false,
                                as_clause: Some("fullname".to_owned()),
                                expression: Expression::TableColumn(TableColumn {
                                    col_name: "name".to_owned(),
                                    table_name: None,
                                }),
                            },
                        ],
                        from: vec![RowDataSource::Table {
                            table_name: "users".to_string(),
                            as_clause: None,
                            joins: vec![],
                        }],
                        where_clause: Expression::Empty,
                        is_distinct: false,
                        order_by: None,
                        limit: None,
                        offset: None,
                    })],
                },
            },
            ParseTest {
                input:
                    "SELECT id, name as charName FROM characters WHERE name != 'Rachel' AND id < 5;",
                ast: Ast {
                    statements: vec![Statement::SelectStatement(SelectStatement {
                        items: vec![
                            SelectItem {
                                asterisk: false,
                                as_clause: None,
                                expression: Expression::TableColumn(TableColumn {
                                    col_name: "id".to_owned(),
                                    table_name: None,
                                }),
                            },
                            SelectItem {
                                asterisk: false,
                                as_clause: Some("charName".to_owned()),
                                expression: Expression::TableColumn(TableColumn {
                                    col_name: "name".to_owned(),
                                    table_name: None,
                                }),
                            },
                        ],
                        from: vec![RowDataSource::Table {
                            table_name: "characters".to_string(),
                            as_clause: None,
                            joins: vec![],
                        }],
                        where_clause: Expression::Binary(BinaryExpression {
                            first: Box::new(Expression::Binary(BinaryExpression {
                                first: Box::new(Expression::TableColumn(TableColumn {
                                    col_name: "name".to_owned(),
                                    table_name: None,
                                })),
                                operand: Operand::NotEqual,
                                second: Box::new(Expression::Literal(LiteralExpression::String("Rachel".to_owned()))),
                            })),
                            operand: Operand::And,
                            second: Box::new(Expression::Binary(BinaryExpression {
                                first: Box::new(Expression::TableColumn(TableColumn {
                                    col_name: "id".to_owned(),
                                    table_name: None,
                                })),
                                operand: Operand::LessThan,
                                second: Box::new(Expression::Literal(LiteralExpression::Numeric( "5".to_owned()))),
                            })),
                        }),
                        is_distinct: false,
                        order_by: None,
                        limit: None,
                        offset: None,
                    })],
                },
            },
            ParseTest {
                input: "SELECT name FROM characters ORDER BY name ASC;",
                ast: Ast {
                    statements: vec![Statement::SelectStatement(SelectStatement {
                        items: vec![SelectItem {
                            asterisk: false,
                            as_clause: None,
                            expression: Expression::TableColumn(TableColumn {
                                col_name: "name".to_owned(),
                                table_name: None,
                            }),
                        }],
                        from: vec![RowDataSource::Table {
                            table_name: "characters".to_string(),
                            as_clause: None,
                            joins: vec![],
                        }],
                        where_clause: Expression::Empty,
                        is_distinct: false,
                        order_by: Some(OrderByClause {
                            asc: true,
                            exp: Expression::TableColumn(TableColumn {
                                col_name: "name".to_owned(),
                                table_name: None,
                            }),
                        }),
                        limit: None,
                        offset: None,
                    })],
                },
            },
            ParseTest {
                input: "SELECT DISTINCT (id / 2)::int FROM characters",
                ast: Ast {
                    statements: vec![Statement::SelectStatement(SelectStatement {
                        items: vec![SelectItem {
                            asterisk: false,
                            as_clause: None,
                            expression: Expression::Cast {
                                data: Box::new(Expression::Binary(BinaryExpression {
                                    first: Box::new(Expression::TableColumn(TableColumn {
                                        col_name: "id".to_owned(),
                                        table_name: None,
                                    })),
                                    operand: Operand::Divide,
                                    second: Box::new(Expression::Literal(LiteralExpression::Numeric( "2".to_owned()))),
                                })),
                                typ: SqlType::Int,
                            },
                        }],
                        from: vec![RowDataSource::Table {
                            table_name: "characters".to_string(),
                            as_clause: None,
                            joins: vec![],
                        }],
                        where_clause: Expression::Empty,
                        is_distinct: true,
                        order_by: None,
                        limit: None,
                        offset: None,
                    })],
                },
            },
            ParseTest {
                input: "SELECT id::text || ' ' || name AS name_with_id FROM characters WHERE id > 1 ORDER BY id DESC LIMIT 4 OFFSET 5;",
                ast: Ast {
                    statements: vec![Statement::SelectStatement(SelectStatement {
                        items: vec![SelectItem {
                            asterisk: false,
                            as_clause: Some(String::from("name_with_id")),
                            expression:Expression::Binary(BinaryExpression {
                                        first: Box::new(Expression::Cast{
                                            data: Box::new(Expression::TableColumn(TableColumn {
                                            col_name: "id".to_owned(),
                                            table_name: None,
                                            })),
                                        typ: SqlType::Text,
                                        }),
                                    operand: Operand::Concat,
                                    second: Box::new(Expression::Binary(BinaryExpression {
                                        first: Box::new(Expression::Literal(LiteralExpression::String( " ".to_owned()))),
                                        operand: Operand::Concat,
                                        second: Box::new(Expression::TableColumn(TableColumn {
                                            col_name: "name".to_owned(),
                                            table_name: None,
                                            })),
                                    })),
                                }),
                        }],
                        from: vec![RowDataSource::Table {
                            table_name: "characters".to_string(),
                            as_clause: None,
                            joins: vec![],
                        }],
                        where_clause: Expression::Binary(BinaryExpression {
                            first: Box::new(Expression::TableColumn(TableColumn {
                                col_name: "id".to_owned(),
                                table_name: None,
                            })),
                            operand: Operand::GreaterThan,
                            second: Box::new(Expression::Literal(LiteralExpression::Numeric("1".to_owned()))),
                        }),
                        is_distinct: false,
                        order_by: Some(OrderByClause {
                            asc: false,
                            exp: Expression::TableColumn(TableColumn {
                                col_name: "id".to_owned(),
                                table_name: None,
                            }),
                        }),
                        limit: Some(4),
                        offset: Some(5),
                    })],
                },
            },
            ParseTest {
                input: "SELECT * FROM characters INNER JOIN character_roles ON characters.id=character_roles.character_id WHERE id != 2 ORDER BY id;",
                ast: Ast {
                    statements: vec![Statement::SelectStatement(SelectStatement {
                        items: vec![SelectItem {
                            asterisk: true,
                            as_clause: None,
                            expression: Expression::Empty,
                        }],
                        from: vec![RowDataSource::Table {
                            table_name: String::from("characters"),
                            as_clause: None,
                            joins: vec![
                                JoinClause {
                                    kind: JoinKind::Inner,
                                    source:
                                        RowDataSource::Table {
                                            table_name: String::from("character_roles"),
                                            as_clause: None,
                                            joins: vec![],
                                        },
                                    on: Expression::Binary(BinaryExpression {
                                            first: Box::new(Expression::TableColumn(TableColumn {
                                                col_name: String::from("id"),
                                                table_name: Some(String::from("characters")),
                                            })),
                                            operand: Operand::Equal,
                                            second: Box::new(Expression::TableColumn(TableColumn {
                                                col_name: String::from("character_id"),
                                                table_name: Some(String::from("character_roles")) ,
                                            })),
                                        }),

                                }
                            ],
                        }],
                        where_clause: Expression:: Binary(BinaryExpression {
                            first: Box::new(Expression::TableColumn(TableColumn {
                                col_name: "id".to_owned(),
                                table_name: None,
                            })),
                            operand: Operand::NotEqual,
                            second: Box::new(Expression::Literal(LiteralExpression::Numeric("2".to_owned()))),
                        }),
                        is_distinct: false,
                        order_by: Some(OrderByClause {
                            asc: true,
                            exp: Expression::TableColumn(TableColumn {
                                col_name: "id".to_owned(),
                                table_name: None,
                            }),
                        }),
                        limit: None,
                        offset: None,
                    })],
                },
            },
        ];

        let mut found_faults = false;
        let mut err_msg = "\n".to_owned();
        let parser = Parser::new();

        for test in parse_tests {
            print!("(Parser) Testing: {}", test.input);

            let ast = match parser.parse(test.input) {
                Ok(value) => value,
                Err(err) => {
                    found_faults = true;
                    err_msg.push_str(err.to_string().as_str());
                    continue;
                }
            };

            assert_eq!(ast, test.ast);
            println!("  Passed!");
        }

        if found_faults {
            panic!("{}", err_msg);
        }
    }
}
