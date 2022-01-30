use crate::{parser::ParsingError, sql_types::SqlType};

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
            "CREATE{unique} INDEX \"{}\" ON \"{}\" ({});",
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
            "CREATE{unique} CONSTRAINT \"{}\" ON \"{}\" ();",
            self.name, self.table,
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
            Expression::Cast { data, typ } => {
                data.generate_code().map(|s| format!("CAST({s} AS {typ})"))
            }
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
            LiteralExpression::String(value) => Ok(format!("'{value}'")),
            LiteralExpression::Identifier(value) => Ok(format!("\"{value}\"")),
            LiteralExpression::Numeric(value) => Ok(value.clone()),
            LiteralExpression::Bool(value) => Ok(if *value {
                TRUE_KEYWORD.to_string()
            } else {
                FALSE_KEYWORD.to_string()
            }),
            LiteralExpression::Null => Ok(NULL_KEYWORD.to_string()),
        }
    }

    pub fn from_token(token: &Token, cursor: usize) -> Result<LiteralExpression, ParsingError> {
        match token {
            Token::StringValue { value } => Ok(LiteralExpression::String(value.to_string())),
            Token::IdentifierValue { value } => {
                Ok(LiteralExpression::Identifier(value.to_string()))
            }
            Token::NumericValue { value } => Ok(LiteralExpression::Numeric(value.to_string())),
            Token::BoolValue { value } => Ok(LiteralExpression::Bool(*value)),
            Token::Null => Ok(LiteralExpression::Null),
            _ => Err(ParsingError::Internal {
                msg: "Unexpected token".to_string(),
                cursor,
            }),
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

    pub fn from_token(token: &Token, cursor: usize) -> Result<Operand, ParsingError> {
        match token {
            Token::Plus => Ok(Operand::Add),
            Token::Minus => Ok(Operand::Subtract),
            Token::Asterisk => Ok(Operand::Multiply),
            Token::Slash => Ok(Operand::Divide),
            Token::Modulo => Ok(Operand::Modulo),
            Token::And => Ok(Operand::And),
            Token::Or => Ok(Operand::Or),
            Token::Equal => Ok(Operand::Equal),
            Token::NotEqual => Ok(Operand::NotEqual),
            Token::GreaterThan => Ok(Operand::GreaterThan),
            Token::GreaterThanOrEqual => Ok(Operand::GreaterThanOrEqual),
            Token::LessThan => Ok(Operand::LessThan),
            Token::LessThanOrEqual => Ok(Operand::LessThanOrEqual),
            Token::Not => Ok(Operand::Not),
            Token::SquareRoot => Ok(Operand::SquareRoot),
            Token::CubeRoot => Ok(Operand::CubeRoot),
            Token::Factorial => Ok(Operand::Factorial),
            Token::FactorialPrefix => Ok(Operand::FactorialPrefix),
            Token::AbsoluteValue => Ok(Operand::AbsoluteValue),
            Token::Exponentiation => Ok(Operand::Exponentiation),
            Token::Concat => Ok(Operand::Concat),
            Token::Is => Ok(Operand::Is),
            // Token::IsNot => Ok(Operand::IsNot),
            // Token::In => Ok(Operand::In),
            // Token::NotIn => Ok(Operand::NotIn),
            Token::Like => Ok(Operand::Like),
            // Token::NotLike => Ok(Operand::NotLike),
            // Token::Between => Ok(Operand::Between),
            // Token::NotBetween => Ok(Operand::NotBetween),
            // Token::Exists => Ok(Operand::Exists),
            // Token::NotExists => Ok(Operand::NotExists),
            // Token::Null => Ok(Operand::Null),
            // Token::NotNull => Ok(Operand::NotNull),
            Token::BitwiseAnd => Ok(Operand::BitwiseAnd),
            Token::BitwiseOr => Ok(Operand::BitwiseOr),
            Token::BitwiseXor => Ok(Operand::BitwiseXor),
            Token::BitwiseNot => Ok(Operand::BitwiseNot),
            Token::BitwiseShiftLeft => Ok(Operand::BitwiseShiftLeft),
            Token::BitwiseShiftRight => Ok(Operand::BitwiseShiftRight),
            // Token::BitwiseShiftRightZeroFill => Ok(Operand::BitwiseShiftRightZeroFill),
            _ => Err(ParsingError::Internal {
                msg: format!("Unrecognized token: {:?}", token),
                cursor,
            }),
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
        self.to_string()
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
            panic!("{err_msg}");
        }
    }
}
