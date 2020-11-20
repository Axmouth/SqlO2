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
    pub is_primary_key: bool,
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
pub enum Expression {
    Literal(LiteralExpression),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
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

    pub fn generate_code(&self) -> Result<String, String> {
        match self {
            Expression::Literal(value) => match &value.literal.token {
                Token::IdentifierValue { value } => Ok(format!("\"{}\"", value)),
                Token::StringValue { value } => Ok(format!("'{}'", value)),
                _ => Err("Unknown Literal Kind".to_string()),
            },
            Expression::Binary(value) => value.generate_code(),
            _ => Err("Unknown Expression Kind".to_string()),
        }
    }

    pub fn is_unary(&self) -> bool {
        match self {
            Expression::Unary(_) => true,
            _ => false,
        }
    }

    pub fn is_binary(&self) -> bool {
        match self {
            Expression::Binary(_) => true,
            _ => false,
        }
    }

    pub fn is_literal(&self) -> bool {
        match self {
            Expression::Literal(_) => true,
            _ => false,
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Expression::Empty => true,
            _ => false,
        }
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

impl BinaryExpression {
    pub fn generate_code(&self) -> Result<String, String> {
        Ok(format!(
            "({} {} {})",
            self.first.generate_code()?,
            self.operand.token.generate_code(),
            self.second.generate_code()?
        ))
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct UnaryExpression {
    pub first: Box<Expression>,
    pub operand: TokenContainer,
}

impl UnaryExpression {
    pub fn generate_code(&self) -> Result<String, String> {
        Ok(format!(
            "({} {})",
            self.operand.token.generate_code(),
            self.first.generate_code()?,
        ))
    }
}

impl Token {
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
            Token::NumericValue { value } => value.clone(),
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
        }
    }
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
                                is_primary_key: false,
                            },
                            ColumnDefinition {
                                name: "name".to_owned(),
                                data_type: TokenContainer {
                                    loc: TokenLocation { col: 33, line: 0 },
                                    token: Token::Text,
                                },
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

            parse(test.input).unwrap();
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
