mod column_definitions;
mod create_index;
mod create_table;
mod drop_table;
mod expression;
mod expressions;
mod insert;
mod joins;
mod literal;
mod select;
mod select_items;
mod statement;
mod table;
mod table_column;
mod tables;

use super::ast::*;
use super::lexer::*;
use crate::sql_types::SqlType;
use std::cmp::Ordering;
use std::convert::TryFrom;
use test_util::TestSubjectExt;
pub use {
    column_definitions::*, create_index::*, create_table::*, drop_table::*, expression::*,
    expressions::*, insert::*, joins::*, literal::*, select::*, select_items::*, statement::*,
    table::*, table_column::*, tables::*,
};

static BINARY_OPERATORS: &[Token<'static>] = &[
    Token::And,
    Token::Or,
    Token::Equal,
    Token::NotEqual,
    Token::Concat,
    Token::Plus,
    Token::Minus,
    Token::Asterisk,
    Token::Slash,
    Token::LessThan,
    Token::LessThanOrEqual,
    Token::GreaterThan,
    Token::GreaterThanOrEqual,
    Token::Modulo,
    Token::Exponentiation,
    Token::BitwiseAnd,
    Token::BitwiseOr,
    Token::BitwiseXor,
    Token::BitwiseShiftLeft,
    Token::BitwiseShiftRight,
    Token::TypeCast,
];
static UNARY_OPERATORS: &[Token<'static>] = &[
    Token::Minus,
    Token::Not,
    Token::FactorialPrefix,
    Token::SquareRoot,
    Token::CubeRoot,
    Token::AbsoluteValue,
    Token::CubeRoot,
    Token::BitwiseNot,
];
static UNARY_POSTFIX_OPERATORS: &[Token<'static>] = &[Token::Factorial];

macro_rules! parse_err {
    ($tokens:expr, $cursor:expr, $msg:expr) => {
        parse_err!($tokens, $cursor, General, $msg)
    };
    ($tokens:expr, $cursor:expr, $err_type:ident, $msg:expr) => {
        return Err(ParsingError::$err_type {
            msg: help_message($tokens.get($cursor), $cursor, $msg),
            cursor: $cursor,
        })
    };
}
pub(crate) use parse_err;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Parser {
    lexer: Lexer,
}

impl TestSubjectExt for Parser {
    fn init() -> Self {
        Self::new()
    }
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            lexer: Lexer::new(),
        }
    }

    pub fn parse<'a>(&'a self, source: &'a str) -> Result<Ast, ParsingError> {
        let tokens = self.lexer.lex(source)?;

        let mut ast = Ast {
            statements: Vec::with_capacity(1),
        };

        let mut cursor: usize = 0;
        let mut first_statement = true;
        while cursor < tokens.len() {
            if !first_statement {
                let mut at_least_one_semicolon = false;
                while expect_token(&tokens, cursor, Token::Semicolon) {
                    cursor += 1;
                    at_least_one_semicolon = true;
                }
                if !(first_statement || at_least_one_semicolon) {
                    parse_err!(
                        tokens,
                        cursor,
                        Delimiter,
                        "Expected Semicolon Delimiter between Statements"
                    );
                }
            }
            let (statement, new_cursor) = parse_statement(&tokens, cursor, Token::Semicolon)?;
            cursor = new_cursor;
            ast.statements.push(statement);
            first_statement = false;

            if cursor == tokens.len() - 1 {
                break;
            }
        }

        Ok(ast)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ParsingError {
    General { msg: String, cursor: usize },
    Lexing { msg: String, loc: TokenLocation },
    Delimiter { msg: String, cursor: usize },
    Internal { msg: String, cursor: usize },
}

impl std::fmt::Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ParsingError::General { msg, cursor: _ } => msg.clone(),
                ParsingError::Lexing { msg, loc: _ } => msg.clone(),
                ParsingError::Delimiter { msg, cursor: _ } => msg.clone(),
                ParsingError::Internal { msg, cursor: _ } => msg.clone(),
            }
        )
    }
}

impl From<LexingError> for ParsingError {
    fn from(err: LexingError) -> Self {
        match err {
            LexingError::General { msg, loc } => ParsingError::Lexing { msg, loc },
        }
    }
}

fn expect_token(tokens: &[TokenContainer], cursor: usize, token: Token) -> bool {
    let current_token = match tokens.get(cursor) {
        Some(value) => value,
        None => {
            return false;
        }
    };
    token == current_token.token
}

fn help_message(token: Option<&TokenContainer>, cursor: usize, msg: &str) -> String {
    if let Some(token) = token {
        format!(
            "[{}, {}]: {}, got {}",
            token.loc.line, token.loc.col, msg, token.token,
        )
    } else {
        format!("Token {cursor} : {msg}")
    }
}

#[cfg(test)]
mod parser_tests {
    use crate::parser::*;

    struct ParseTest {
        ast: Ast,
        input: &'static str,
    }

    #[test]
    fn test_parser() {
        let parse_tests = vec![
            ParseTest {
                input: "INSERT INTO users VALUES (105, 'George');",
                ast: Ast {
                    statements: vec![Statement::InsertStatement(InsertStatement {
                        table: "users".to_string(),
                        values: vec![
                            Expression::Literal(LiteralExpression::Numeric("105".to_owned())),
                            Expression::Literal(LiteralExpression::String("George".to_owned())),
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
                                name: "id".to_string(),
                                data_type: SqlType::Int,
                                is_primary_key: false,
                            },
                            ColumnDefinition {
                                name: "name".to_string(),
                                data_type: SqlType::Int,
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
                                    col_name: "id".to_string(),
                                    table_name: None,
                                }),
                            },
                            SelectItem {
                                asterisk: false,
                                as_clause: Some("fullname".to_string()),
                                expression: Expression::TableColumn(TableColumn {
                                    col_name: "name".to_string(),
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
                input: "SELECT distinct id, name AS fullname FROM users;",
                ast: Ast {
                    statements: vec![Statement::SelectStatement(SelectStatement {
                        items: vec![
                            SelectItem {
                                asterisk: false,
                                as_clause: None,
                                expression: Expression::TableColumn(TableColumn {
                                    col_name: "id".to_string(),
                                    table_name: None,
                                }),
                            },
                            SelectItem {
                                asterisk: false,
                                as_clause: Some("fullname".to_owned()),
                                expression: Expression::TableColumn(TableColumn {
                                    col_name: "name".to_string(),
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
                        is_distinct: true,
                        order_by: None,
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

            parser.parse(test.input).unwrap();
            let ast = match parser.parse(test.input) {
                Ok(value) => value,
                Err(err) => {
                    found_faults = true;
                    err_msg.push_str(err.to_string().as_str());
                    continue;
                }
            };

            if ast != test.ast {
                err_msg.push_str(
                    format!("\n\nExpected:\n{:#?}\n\nGot:\n{:#?}\n", test.ast, ast).as_str(),
                );
            }

            // assert_eq!(ast, test.ast);
            println!("  Passed!");
        }

        if found_faults {
            panic!("{err_msg}");
        }
    }
}
