use std::fmt::Display;

use tree_display::{tree_display_macros::TreeDisplay, TreeDisplay};

use crate::{parser::ParsingError, sql_types::SqlType};

use super::lexer::*;

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
pub struct Ast {
    pub statements: Vec<Statement>,
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.tree_fmt(f, Default::default(), Default::default())
    }
}

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
pub enum Statement {
    SelectStatement(SelectStatement),
    CreateTableStatement(CreateTableStatement),
    CreateIndexStatement(CreateIndexStatement),
    DropTableStatement(DropTableStatement),
    InsertStatement(InsertStatement),
}

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
#[tree_display(rename_all_pascal)]
pub struct OrderByClause {
    pub asc: bool,
    pub exp: Expression,
}

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
#[tree_display(rename_all_pascal)]
pub struct JoinClause {
    pub kind: JoinKind,
    pub source: RowDataSource,
    pub on: Expression,
}

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
pub enum JoinKind {
    Inner,
    FullOuter,
    LeftOuter,
    RightOuter,
}

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
#[tree_display(rename_all_pascal)]
pub enum RowDataSource {
    SubSelect {
        select: SelectStatement,
        as_clause: String,
        #[tree_display(skip_if_empty)]
        joins: Vec<JoinClause>,
    },
    Table {
        table_name: String,
        #[tree_display(skip_if_none)]
        as_clause: Option<String>,
        #[tree_display(skip_if_empty)]
        joins: Vec<JoinClause>,
    },
}

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
#[tree_display(rename_all_pascal)]
pub struct TableColumn {
    pub col_name: String,
    #[tree_display(skip_if_none)]
    pub table_name: Option<String>,
}

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
#[tree_display(rename_all_pascal)]
pub struct ProcessedTableColumn {
    pub col_name: Option<String>,
    pub col_idx: usize,
}

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
#[tree_display(rename_all_pascal)]
pub struct InsertStatement {
    pub table: String,
    pub values: Vec<Expression>,
}

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
#[tree_display(rename_all_pascal)]
pub struct CreateTableStatement {
    pub name: String,
    #[tree_display(skip_if_empty)]
    pub cols: Vec<ColumnDefinition>,
}

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
#[tree_display(rename_all_pascal)]
pub struct ColumnDefinition {
    pub name: String,
    pub data_type: SqlType,
    #[tree_display(skip_if_false)]
    pub is_primary_key: bool,
}

#[derive(Clone, Eq, PartialEq, Debug, Default, TreeDisplay)]
#[tree_display(rename_all_pascal)]
pub struct SelectStatement {
    #[tree_display(skip_if_empty)]
    pub from: Vec<RowDataSource>,
    #[tree_display(skip_if_empty)]
    pub where_clause: Expression,
    #[tree_display(skip_if_false, rename = "Distinct")]
    pub is_distinct: bool,
    #[tree_display(skip_if_none)]
    pub order_by: Option<OrderByClause>,
    #[tree_display(skip_if_none)]
    pub limit: Option<usize>,
    #[tree_display(skip_if_none)]
    pub offset: Option<usize>,
    pub items: Vec<SelectItem>,
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

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
#[tree_display(rename_all_pascal)]
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

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
#[tree_display(rename_all_pascal)]
pub struct CreateConstraintStatement {
    pub name: String,
    pub constraint: ConstraintType,
    pub table: String,
}

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
#[tree_display(rename_all_pascal)]
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

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
#[tree_display(rename_all_pascal)]
pub struct DropTableStatement {
    pub name: String,
}

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
#[tree_display(rename_all_pascal)]
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

    pub fn is_unary(&self) -> bool {
        matches!(self, Expression::Unary(_))
    }

    pub fn is_binary(&self) -> bool {
        matches!(self, Expression::Binary(_))
    }

    pub fn is_literal(&self) -> bool {
        matches!(self, Expression::Literal(_))
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Expression::Empty)
    }
}

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
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

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
pub enum BinaryOperand {
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
    Exponentiation,
    Concat,
    In,
    NotIn,
    Like,
    NotLike,
    Between,
    NotBetween,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseShiftLeft,
    BitwiseShiftRight,
    BitwiseShiftRightZeroFill,
}

impl BinaryOperand {
    pub fn from_token(token: &Token) -> Option<BinaryOperand> {
        match token {
            Token::Plus => Some(BinaryOperand::Add),
            Token::Minus => Some(BinaryOperand::Subtract),
            Token::Asterisk => Some(BinaryOperand::Multiply),
            Token::Slash => Some(BinaryOperand::Divide),
            Token::Modulo => Some(BinaryOperand::Modulo),
            Token::And => Some(BinaryOperand::And),
            Token::Or => Some(BinaryOperand::Or),
            Token::Equal => Some(BinaryOperand::Equal),
            Token::NotEqual => Some(BinaryOperand::NotEqual),
            Token::GreaterThan => Some(BinaryOperand::GreaterThan),
            Token::GreaterThanOrEqual => Some(BinaryOperand::GreaterThanOrEqual),
            Token::LessThan => Some(BinaryOperand::LessThan),
            Token::LessThanOrEqual => Some(BinaryOperand::LessThanOrEqual),
            Token::Exponentiation => Some(BinaryOperand::Exponentiation),
            Token::Concat => Some(BinaryOperand::Concat),
            // Token::In => Some(BinaryOperand::In),
            // Token::NotIn => Some(BinaryOperand::NotIn),
            Token::Like => Some(BinaryOperand::Like),
            // Token::NotLike => Some(BinaryOperand::NotLike),
            // Token::Between => Some(BinaryOperand::Between),
            // Token::NotBetween => Some(BinaryOperand::NotBetween),
            Token::BitwiseAnd => Some(BinaryOperand::BitwiseAnd),
            Token::BitwiseOr => Some(BinaryOperand::BitwiseOr),
            Token::BitwiseXor => Some(BinaryOperand::BitwiseXor),
            Token::BitwiseShiftLeft => Some(BinaryOperand::BitwiseShiftLeft),
            Token::BitwiseShiftRight => Some(BinaryOperand::BitwiseShiftRight),
            // Token::BitwiseShiftRightZeroFill => Some(BinaryOperand::BitwiseShiftRightZeroFill),
            _ => None,
        }
    }

    pub fn generate_code(&self) -> String {
        match self {
            BinaryOperand::Add => "+",
            BinaryOperand::Subtract => "-",
            BinaryOperand::Multiply => "*",
            BinaryOperand::Divide => "/",
            BinaryOperand::Modulo => "%",
            BinaryOperand::And => "AND",
            BinaryOperand::Or => "OR",
            BinaryOperand::Equal => "=",
            BinaryOperand::NotEqual => "<>",
            BinaryOperand::GreaterThan => ">",
            BinaryOperand::GreaterThanOrEqual => ">=",
            BinaryOperand::LessThan => "<",
            BinaryOperand::LessThanOrEqual => "<=",
            BinaryOperand::Exponentiation => "^",
            BinaryOperand::Concat => "||",
            BinaryOperand::In => "IN",
            BinaryOperand::NotIn => "NOT IN",
            BinaryOperand::Like => "LIKE",
            BinaryOperand::NotLike => "NOT LIKE",
            BinaryOperand::Between => "BETWEEN",
            BinaryOperand::NotBetween => "NOT BETWEEN",
            BinaryOperand::BitwiseAnd => "&",
            BinaryOperand::BitwiseOr => "|",
            BinaryOperand::BitwiseXor => "#",
            BinaryOperand::BitwiseShiftLeft => "<<",
            BinaryOperand::BitwiseShiftRight => ">>",
            BinaryOperand::BitwiseShiftRightZeroFill => ">>>",
        }
        .to_string()
    }
}

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
pub enum UnaryOperand {
    Not,
    SquareRoot,
    CubeRoot,
    Factorial,
    FactorialPrefix,
    AbsoluteValue,
    Exists,
    NotExists,
    IsNull,
    IsNotNull,
    BitwiseNot,
    Minus,
}

impl UnaryOperand {
    pub fn from_token(token: &Token) -> Option<UnaryOperand> {
        match token {
            Token::Not => Some(UnaryOperand::Not),
            Token::SquareRoot => Some(UnaryOperand::SquareRoot),
            Token::CubeRoot => Some(UnaryOperand::CubeRoot),
            Token::Factorial => Some(UnaryOperand::Factorial),
            Token::FactorialPrefix => Some(UnaryOperand::FactorialPrefix),
            Token::AbsoluteValue => Some(UnaryOperand::AbsoluteValue),
            // Token::Exists => Some(UnaryOperand::Exists),
            // Token::NotExists => Some(UnaryOperand::NotExists),
            // Token::IsNull => Some(UnaryOperand::IsNull),
            // Token::IsNotNull => Some(UnaryOperand::IsNotNull),
            Token::BitwiseNot => Some(UnaryOperand::BitwiseNot),
            Token::Minus => Some(UnaryOperand::Minus),
            _ => None,
        }
    }

    pub fn postfix_from_token(token: &Token) -> Option<UnaryOperand> {
        match token {
            Token::Factorial => Some(UnaryOperand::Factorial),
            _ => None,
        }
    }

    pub fn generate_code(&self) -> String {
        match self {
            UnaryOperand::Not => "NOT",
            UnaryOperand::SquareRoot => "SQRT",
            UnaryOperand::CubeRoot => "CBRT",
            UnaryOperand::Factorial => "!",
            UnaryOperand::FactorialPrefix => "!",
            UnaryOperand::AbsoluteValue => "ABS",
            UnaryOperand::Exists => "EXISTS",
            UnaryOperand::NotExists => "NOT EXISTS",
            UnaryOperand::IsNull => "IS NULL",
            UnaryOperand::IsNotNull => "IS NOT NULL",
            UnaryOperand::BitwiseNot => "~",
            UnaryOperand::Minus => "-",
        }
        .to_string()
    }
}

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
pub enum Operand {
    Binary(BinaryOperand),
    Unary(UnaryOperand),
}

impl Operand {
    pub fn generate_code(&self) -> String {
        match self {
            Operand::Binary(value) => value.generate_code(),
            Operand::Unary(value) => value.generate_code(),
        }
    }

    pub fn from_token(token: &Token) -> Option<Operand> {
        if let Some(value) = BinaryOperand::from_token(token) {
            Some(Operand::Binary(value))
        } else if let Some(value) = UnaryOperand::from_token(token) {
            Some(Operand::Unary(value))
        } else {
            None
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
#[tree_display(rename_all_pascal)]
pub struct BinaryExpression {
    pub first: Box<Expression>,
    pub second: Box<Expression>,
    pub operand: BinaryOperand,
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

#[derive(Clone, Eq, PartialEq, Debug, TreeDisplay)]
#[tree_display(rename_all_pascal)]
pub struct UnaryExpression {
    pub first: Box<Expression>,
    pub operand: UnaryOperand,
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

#[derive(Clone, Eq, PartialEq, Debug, Default, TreeDisplay)]
#[tree_display(rename_all_pascal)]
pub struct SelectItem {
    pub expression: Expression,
    #[tree_display(skip_if_none)]
    pub as_clause: Option<String>,
    #[tree_display(skip_if_false)]
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

    #[test]
    fn test_ast_fmt() {
        let sql = "SELECT * FROM table1";
        let parser = Parser::new();
        let ast = parser.parse(sql).unwrap();
        eprintln!("{}", ast);
    }

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
                                operand: BinaryOperand::NotEqual,
                                second: Box::new(Expression::Literal(LiteralExpression::String("Rachel".to_owned()))),
                            })),
                            operand: BinaryOperand::And,
                            second: Box::new(Expression::Binary(BinaryExpression {
                                first: Box::new(Expression::TableColumn(TableColumn {
                                    col_name: "id".to_owned(),
                                    table_name: None,
                                })),
                                operand: BinaryOperand::LessThan,
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
                                    operand: BinaryOperand::Divide,
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
                                    operand: BinaryOperand::Concat,
                                    second: Box::new(Expression::Binary(BinaryExpression {
                                        first: Box::new(Expression::Literal(LiteralExpression::String( " ".to_owned()))),
                                        operand: BinaryOperand::Concat,
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
                            operand: BinaryOperand::GreaterThan,
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
                                            operand: BinaryOperand::Equal,
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
                            operand: BinaryOperand::NotEqual,
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
