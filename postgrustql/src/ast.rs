use super::lexer::*;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Ast {
    pub statements: Vec<Statement>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Statement {
    SelectStatement(SelectStatement),
    CreateTableStatement(CreateTableStatement),
    InsertStatement(InsertStatement),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct InsertStatement {
    pub table: Token,
    pub values: Vec<Expression>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CreateTableStatement {
    pub name: Token,
    pub cols: Vec<ColumnDefinition>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ColumnDefinition {
    pub name: Token,
    pub data_type: Token,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct SelectStatement {
    pub items: Vec<SelectItem>,
    pub from: Token,
    pub where_clause: Expression,
}

impl SelectStatement {
    pub fn new() -> Self {
        SelectStatement {
            items: vec![],
            from: Token::new(),
            where_clause: Expression::new(),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Expression {
    Literal(LiteralExpression),
    Binary(BinaryExpression),
    Empty,
}

impl Expression {
    pub fn new() -> Expression {
        Expression::Empty
    }

    pub fn new_literal_id(value: String) -> Expression {
        Expression::Literal(LiteralExpression {
            literal: Token::new_with_kind_and_value(TokenKind::IdentifierKind, value),
        })
    }
    pub fn new_literal_num(value: String) -> Expression {
        Expression::Literal(LiteralExpression {
            literal: Token::new_with_kind_and_value(TokenKind::NumericKind, value),
        })
    }
    pub fn new_literal_string(value: String) -> Expression {
        Expression::Literal(LiteralExpression {
            literal: Token::new_with_kind_and_value(TokenKind::StringKind, value),
        })
    }
    pub fn new_literal_symbol(value: String) -> Expression {
        Expression::Literal(LiteralExpression {
            literal: Token::new_with_kind_and_value(TokenKind::SymbolKind, value),
        })
    }
    pub fn new_literal_bool(value: String) -> Expression {
        Expression::Literal(LiteralExpression {
            literal: Token::new_with_kind_and_value(TokenKind::BoolKind, value),
        })
    }
    pub fn new_literal_null() -> Expression {
        Expression::Literal(LiteralExpression {
            literal: Token::new_with_kind_and_value(TokenKind::NullKind, "null".to_string()),
        })
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct LiteralExpression {
    pub literal: Token,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct BinaryExpression {
    pub first: Box<Expression>,
    pub second: Box<Expression>,
    pub operand: Token,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct SelectItem {
    pub expression: Expression,
    pub as_clause: Token,
    pub asterisk: bool,
}

impl SelectItem {
    pub fn new() -> Self {
        SelectItem {
            expression: Expression::new(),
            as_clause: Token::new(),
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
                        table: Token {
                            loc: TokenLocation { col: 12, line: 0 },
                            kind: TokenKind::IdentifierKind,
                            value: "users".to_owned(),
                        },
                        values: vec![
                            Expression::Literal(LiteralExpression {
                                literal: Token {
                                    loc: TokenLocation { col: 26, line: 0 },
                                    kind: TokenKind::NumericKind,
                                    value: "105".to_owned(),
                                },
                            }),
                            Expression::Literal(LiteralExpression {
                                literal: Token {
                                    loc: TokenLocation { col: 32, line: 0 },
                                    kind: TokenKind::StringKind,
                                    value: "George".to_owned(),
                                },
                            }),
                        ],
                    })],
                },
            },
            ParseTest {
                input: "CREATE TABLE users (id INT, name TEXT);",
                ast: Ast {
                    statements: vec![Statement::CreateTableStatement(CreateTableStatement {
                        name: Token {
                            loc: TokenLocation { col: 13, line: 0 },
                            kind: TokenKind::IdentifierKind,
                            value: "users".to_owned(),
                        },
                        cols: vec![
                            ColumnDefinition {
                                name: Token {
                                    loc: TokenLocation { col: 20, line: 0 },
                                    kind: TokenKind::IdentifierKind,
                                    value: "id".to_owned(),
                                },
                                data_type: Token {
                                    loc: TokenLocation { col: 23, line: 0 },
                                    kind: TokenKind::KeywordKind,
                                    value: INT_KEYWORD.to_owned(),
                                },
                            },
                            ColumnDefinition {
                                name: Token {
                                    loc: TokenLocation { col: 28, line: 0 },
                                    kind: TokenKind::IdentifierKind,
                                    value: "name".to_owned(),
                                },
                                data_type: Token {
                                    loc: TokenLocation { col: 33, line: 0 },
                                    kind: TokenKind::KeywordKind,
                                    value: TEXT_KEYWORD.to_owned(),
                                },
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
                                as_clause: Token::new(),
                                expression: Expression::Literal(LiteralExpression {
                                    literal: Token::new_with_all(
                                        TokenKind::IdentifierKind,
                                        "id".to_owned(),
                                        7,
                                        0,
                                    ),
                                }),
                            },
                            SelectItem {
                                asterisk: false,
                                as_clause: Token::new_with_all(
                                    TokenKind::IdentifierKind,
                                    "fullname".to_owned(),
                                    19,
                                    0,
                                ),
                                expression: Expression::Literal(LiteralExpression {
                                    literal: Token::new_with_all(
                                        TokenKind::IdentifierKind,
                                        "name".to_owned(),
                                        11,
                                        0,
                                    ),
                                }),
                            },
                        ],
                        from: Token::new_with_all(
                            TokenKind::IdentifierKind,
                            "users".to_string(),
                            33,
                            0,
                        ),
                        where_clause: Expression::Empty,
                    })],
                },
            },
        ];

        let mut found_faults = false;
        let mut err_msg = "\n".to_owned();

        for test in parse_tests {
            print!("(Parser) Testing: {}", test.input);

            let ast_result = parse(test.input);

            if ast_result.is_err() {
                found_faults = true;
                err_msg.push_str(ast_result.err().unwrap().as_str());
                continue;
            }

            let ast = ast_result.ok().unwrap();

            assert_eq!(ast, test.ast);
            println!("  Passed!");
        }

        if found_faults {
            panic!(err_msg);
        }
    }
}
