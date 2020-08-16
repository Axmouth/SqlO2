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
    pub data_type: TokenContainer,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct SelectStatement {
    pub items: Vec<SelectItem>,
    pub from: Option<String>,
    pub where_clause: Expression,
}

impl SelectStatement {
    pub fn new() -> Self {
        SelectStatement {
            items: vec![],
            from: None,
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
            literal: TokenContainer::new_with_kind_and_value(
                Token::IdentifierValue {
                    value: value.clone(),
                },
                value,
            ),
        })
    }
    pub fn new_literal_num(value: String) -> Expression {
        Expression::Literal(LiteralExpression {
            literal: TokenContainer::new_with_kind_and_value(
                Token::NumericValue {
                    value: value.clone(),
                },
                value.clone(),
            ),
        })
    }
    pub fn new_literal_string(value: String) -> Expression {
        Expression::Literal(LiteralExpression {
            literal: TokenContainer::new_with_kind_and_value(
                Token::StringValue {
                    value: value.clone(),
                },
                value,
            ),
        })
    }
    pub fn new_literal_bool(value: String) -> Expression {
        Expression::Literal(LiteralExpression {
            literal: TokenContainer::new_with_kind_and_value(
                Token::BoolValue {
                    value: if value == TRUE_KEYWORD { true } else { false },
                },
                value,
            ),
        })
    }
    pub fn new_literal_null() -> Expression {
        Expression::Literal(LiteralExpression {
            literal: TokenContainer::new_with_kind_and_value(Token::Null, "null".to_string()),
        })
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct LiteralExpression {
    pub literal: TokenContainer,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct BinaryExpression {
    pub first: Box<Expression>,
    pub second: Box<Expression>,
    pub operand: TokenContainer,
}

#[derive(Clone, Eq, PartialEq, Debug)]
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
                            Expression::Literal(LiteralExpression {
                                literal: TokenContainer {
                                    loc: TokenLocation { col: 26, line: 0 },
                                    token: Token::NumericValue {
                                        value: "105".to_owned(),
                                    },
                                },
                            }),
                            Expression::Literal(LiteralExpression {
                                literal: TokenContainer {
                                    loc: TokenLocation { col: 32, line: 0 },
                                    token: Token::StringValue {
                                        value: "George".to_owned(),
                                    },
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
                        name: "users".to_owned(),
                        cols: vec![
                            ColumnDefinition {
                                name: "id".to_owned(),
                                data_type: TokenContainer {
                                    loc: TokenLocation { col: 23, line: 0 },
                                    token: Token::Int,
                                },
                            },
                            ColumnDefinition {
                                name: "name".to_owned(),
                                data_type: TokenContainer {
                                    loc: TokenLocation { col: 33, line: 0 },
                                    token: Token::Text,
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
                                as_clause: None,
                                expression: Expression::Literal(LiteralExpression {
                                    literal: TokenContainer::new_with_all(
                                        Token::IdentifierValue {
                                            value: "id".to_owned(),
                                        },
                                        "id".to_owned(),
                                        7,
                                        0,
                                    ),
                                }),
                            },
                            SelectItem {
                                asterisk: false,
                                as_clause: Some("fullname".to_owned()),
                                expression: Expression::Literal(LiteralExpression {
                                    literal: TokenContainer::new_with_all(
                                        Token::IdentifierValue {
                                            value: "name".to_owned(),
                                        },
                                        "name".to_owned(),
                                        11,
                                        0,
                                    ),
                                }),
                            },
                        ],
                        from: Some("users".to_string()),
                        where_clause: Expression::Empty,
                    })],
                },
            },
        ];

        let mut found_faults = false;
        let mut err_msg = "\n".to_owned();

        for test in parse_tests {
            print!("(Parser) Testing: {}", test.input);

            let ast = match parse(test.input) {
                Ok(value) => value,
                Err(err) => {
                    found_faults = true;
                    err_msg.push_str(err.as_str());
                    continue;
                }
            };

            assert_eq!(ast, test.ast);
            println!("  Passed!");
        }

        if found_faults {
            panic!(err_msg);
        }
    }
}
