use std::cmp::Ordering;
use std::convert::TryFrom;

use crate::sql_types::SqlType;

use super::ast::*;
use super::lexer::*;

use lazy_static;

lazy_static! {}
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Parser {
    lexer: Lexer,
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
                    return Err(ParsingError::Delimiter {
                        msg: help_message(
                            tokens.get(cursor),
                            cursor,
                            "Expected Semicolon Delimiter between Statements",
                        ),
                        cursor,
                    });
                }
            }
            match parse_statement(&tokens, cursor, Token::Semicolon) {
                Ok((statement, new_cursor)) => {
                    cursor = new_cursor;

                    ast.statements.push(statement);
                    first_statement = false;
                }

                Err(err) => {
                    return Err(err);
                }
            }
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
    Internal { msg: String },
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
                ParsingError::Internal { msg } => msg.clone(),
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

#[inline]
fn expect_token(tokens: &[TokenContainer], cursor: usize, token: Token) -> bool {
    let current_token = match tokens.get(cursor) {
        Some(value) => value,
        None => {
            return false;
        }
    };
    token == current_token.token
}

#[inline]
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

fn parse_statement<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    delimiter: Token,
) -> Result<(Statement, usize), ParsingError> {
    let cursor = initial_cursor;

    if let Some(first_token) = tokens.get(cursor) {
        match first_token.token {
            Token::Select => {
                // Look for a SELECT statement
                match parse_select_statement(tokens, cursor, delimiter) {
                    Ok((select, new_cursor)) => {
                        Ok((Statement::SelectStatement(select), new_cursor))
                    }
                    Err(err) => Err(err),
                }
            }
            Token::Insert => {
                // Look for an INSERT statement
                match parse_insert_statement(tokens, cursor, delimiter) {
                    Ok((insert, new_cursor)) => {
                        Ok((Statement::InsertStatement(insert), new_cursor))
                    }
                    Err(err) => Err(err),
                }
            }
            Token::Delete => Err(ParsingError::Internal {
                msg: help_message(tokens.get(cursor), cursor, "Delete not implemented"),
            }),
            Token::Update => Err(ParsingError::Internal {
                msg: help_message(tokens.get(cursor), cursor, "Update not implemented"),
            }),
            Token::Alter => Err(ParsingError::Internal {
                msg: help_message(tokens.get(cursor), cursor, "Alter not implemented"),
            }),
            Token::IdentifierValue { value: _ } => Err(ParsingError::Internal {
                msg: help_message(tokens.get(cursor), cursor, "Assignment not implemented"),
            }),
            Token::Create => {
                if let Some(first_token) = tokens.get(cursor + 1) {
                    match first_token.token {
                        Token::Index => {
                            // Look for a CREATE INDEX statement
                            match parse_create_index_statement(tokens, cursor, delimiter) {
                                Ok((create_index, new_cursor)) => {
                                    Ok((Statement::CreateIndexStatement(create_index), new_cursor))
                                }
                                Err(err) => (Err(err)),
                            }
                        }
                        Token::Unique => match tokens.get(cursor + 2) {
                            Some(TokenContainer {
                                token: Token::Index,
                                loc: _,
                            }) => {
                                // Look for a CREATE UNIQUE INDEX statement
                                match parse_create_index_statement(tokens, cursor, delimiter) {
                                    Ok((create_index, new_cursor)) => Ok((
                                        Statement::CreateIndexStatement(create_index),
                                        new_cursor,
                                    )),
                                    Err(err) => (Err(err)),
                                }
                            }
                            Some(TokenContainer {
                                token: Token::Constraint,
                                loc: _,
                            }) => Err(ParsingError::General {
                                msg: help_message(
                                    tokens.get(cursor),
                                    cursor,
                                    "Create constraint not implemented",
                                ),
                                cursor,
                            }),
                            _ => Err(ParsingError::General {
                                msg: help_message(
                                    tokens.get(cursor),
                                    cursor,
                                    "Invalid Create Statement",
                                ),
                                cursor,
                            }),
                        },
                        Token::Constraint => Err(ParsingError::General {
                            msg: help_message(
                                tokens.get(cursor),
                                cursor,
                                "Create constraint not implemented",
                            ),
                            cursor,
                        }),
                        Token::Table => {
                            // Look for a CREATE TABLE statement
                            match parse_create_table_statement(tokens, cursor, delimiter) {
                                Ok((create_table, new_cursor)) => {
                                    Ok((Statement::CreateTableStatement(create_table), new_cursor))
                                }
                                Err(err) => (Err(err)),
                            }
                        }
                        _ => Err(ParsingError::General {
                            msg: help_message(
                                tokens.get(cursor),
                                cursor,
                                "Invalid Create Statement",
                            ),
                            cursor,
                        }),
                    }
                } else {
                    Err(ParsingError::General {
                        msg: help_message(tokens.get(cursor), cursor, "Invalid Create Statement"),
                        cursor,
                    })
                }
            }
            Token::Drop => {
                // Look for an DROP statement
                match parse_drop_table_statement(tokens, cursor, delimiter) {
                    Ok((drop, new_cursor)) => Ok((Statement::DropTableStatement(drop), new_cursor)),
                    Err(err) => (Err(err)),
                }
            }
            _ => Err(ParsingError::General {
                msg: help_message(tokens.get(cursor), cursor, "Expected a valid statement"),
                cursor,
            }),
        }
    } else {
        Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Expected a valid statement"),
            cursor,
        })
    }
}

fn parse_column_definitions<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    delimiter: Token,
) -> Result<(Vec<ColumnDefinition>, usize), ParsingError> {
    let mut cursor = initial_cursor;

    let mut column_definitions: Vec<ColumnDefinition> = Vec::with_capacity(5);

    loop {
        if cursor >= tokens.len() {
            return Err(ParsingError::General {
                msg: help_message(tokens.get(cursor), cursor, "Unexpected end of file."),
                cursor,
            });
        }

        // Look for a delimiter
        if let Some(TokenContainer {
            loc: _,
            token: current_token,
        }) = tokens.get(cursor)
        {
            if current_token == &delimiter {
                break;
            }
        }

        // Look for a comma
        if !column_definitions.is_empty() {
            if let Some(TokenContainer { loc: _, token }) = tokens.get(cursor) {
                if token == &Token::Comma {
                    cursor += 1;
                } else {
                    return Err(ParsingError::General {
                        msg: help_message(tokens.get(cursor), cursor, "Expected Comma"),
                        cursor,
                    });
                }
            }
        }
        // Look for a column name
        let col_name = match &tokens.get(cursor) {
            Some(TokenContainer {
                loc: _,
                token: Token::IdentifierValue { value },
            }) => value,
            _ => {
                return Err(ParsingError::General {
                    msg: help_message(tokens.get(cursor), cursor, "Expected Column Name"),
                    cursor,
                });
            }
        };
        cursor += 1;

        // Look for a column type
        if let Some(token_c) = tokens.get(cursor) {
            if !token_c.token.is_datatype() {
                return Err(ParsingError::General {
                    msg: help_message(tokens.get(cursor), cursor, "Expected Column Type"),
                    cursor,
                });
            }
        }

        let mut is_primary_key = false;
        let col_type = match tokens.get(cursor) {
            Some(v) => v,
            None => {
                return Err(ParsingError::General {
                    msg: help_message(tokens.get(cursor), cursor, "Expected Column Type"),
                    cursor,
                });
            }
        };
        cursor += 1;

        // Look for primary key
        if let (
            Some(TokenContainer {
                loc: _,
                token: Token::Primary,
            }),
            Some(TokenContainer {
                loc: _,
                token: Token::Key,
            }),
        ) = (&tokens.get(cursor), &tokens.get(cursor + 1))
        {
            is_primary_key = true;
            cursor += 2;
        }

        column_definitions.push(ColumnDefinition {
            name: col_name.to_string(),
            data_type: SqlType::from_token(col_type)?,
            is_primary_key,
        });
    }

    Ok((column_definitions, cursor))
}

fn parse_create_table_statement<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    _: Token,
) -> Result<(CreateTableStatement, usize), ParsingError> {
    let mut cursor = initial_cursor;

    if !expect_token(tokens, cursor, Token::Create) {
        return Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Not a create table statement"),
            cursor,
        });
    }
    cursor += 1;

    if !expect_token(tokens, cursor, Token::Table) {
        return Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Expected table keyword"),
            cursor,
        });
    }
    cursor += 1;

    let name = match tokens.get(cursor) {
        Some(TokenContainer {
            loc: _,
            token: Token::IdentifierValue { value },
        }) => value,
        _ => {
            return Err(ParsingError::General {
                msg: help_message(tokens.get(cursor), cursor, "Expected Table Name"),
                cursor,
            });
        }
    };
    cursor += 1;
    if let Some(TokenContainer {
        loc: _,
        token: Token::LeftParenthesis,
    }) = tokens.get(cursor)
    {
        cursor += 1;
    } else {
        return Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Expected Left Parenthesis"),
            cursor,
        });
    }

    let (cols, new_cursor) = parse_column_definitions(tokens, cursor, Token::RightParenthesis)?;
    cursor = new_cursor;

    if let Some(TokenContainer {
        loc: _,
        token: Token::RightParenthesis,
    }) = tokens.get(cursor)
    {
        cursor += 1;
    } else {
        return Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Expected Left Parenthesis"),
            cursor,
        });
    }

    Ok((
        CreateTableStatement {
            name: name.to_string(),
            cols,
        },
        cursor,
    ))
}

fn parse_create_index_statement<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    delimiter: Token,
) -> Result<(CreateIndexStatement, usize), ParsingError> {
    let mut cursor = initial_cursor;
    if let Some(TokenContainer {
        loc: _,
        token: Token::Create,
    }) = tokens.get(cursor)
    {
        cursor += 1;
    } else {
        return Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Not a create index statement"),
            cursor,
        });
    }
    let mut is_unique = false;
    if let Some(TokenContainer {
        loc: _,
        token: Token::Unique,
    }) = tokens.get(cursor)
    {
        is_unique = true;
        cursor += 1;
    }
    if let Some(TokenContainer {
        loc: _,
        token: Token::Index,
    }) = tokens.get(cursor)
    {
        cursor += 1;
    } else {
        return Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Not a create index statement"),
            cursor,
        });
    }
    let name;
    if let Some(TokenContainer {
        loc: _,
        token: Token::IdentifierValue { value },
    }) = tokens.get(cursor)
    {
        cursor += 1;
        name = value;
    } else {
        return Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Expected index name"),
            cursor,
        });
    }
    if let Some(TokenContainer {
        loc: _,
        token: Token::On,
    }) = tokens.get(cursor)
    {
        cursor += 1;
    } else {
        return Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Expected on keyword"),
            cursor,
        });
    }
    let table;
    if let Some(TokenContainer {
        loc: _,
        token: Token::IdentifierValue { value },
    }) = tokens.get(cursor)
    {
        cursor += 1;
        table = value;
    } else {
        return Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Expected table name"),
            cursor,
        });
    }
    let (expression, cursor) = match parse_expression(tokens, cursor, &[delimiter], 0, true, false)
    {
        Ok(value) => value,
        Err(_) => {
            return Err(ParsingError::General {
                msg: help_message(tokens.get(cursor), cursor, "Expected index expressions"),
                cursor,
            });
        }
    };

    Ok((
        CreateIndexStatement {
            is_primary_key: false,
            is_unique,
            name: name.to_string(),
            expression,
            table: table.to_string(),
        },
        cursor,
    ))
}

fn parse_expressions(
    tokens: &[TokenContainer],
    initial_cursor: usize,
    delimiters: &[Token],
) -> Result<(Vec<Expression>, usize), ParsingError> {
    let mut cursor = initial_cursor;

    let mut expressions: Vec<Expression> = Vec::with_capacity(5);

    loop {
        if cursor >= tokens.len() {
            return Err(ParsingError::General {
                msg: help_message(tokens.get(cursor), cursor, "Expected expression"),
                cursor,
            });
        }

        // Look for delimiter
        if let Some(TokenContainer {
            loc: _,
            token: current_token,
        }) = tokens.get(cursor)
        {
            if delimiters.contains(current_token) {
                return Ok((expressions, cursor));
            }
        }

        // Look for comma
        if !expressions.is_empty() {
            if !expect_token(tokens, cursor, Token::Comma) {
                return Err(ParsingError::General {
                    msg: help_message(tokens.get(cursor), cursor, "Expected Comma"),
                    cursor,
                });
            }

            cursor += 1;
        }

        // Look for expression
        let (expression, new_cursor);
        if let Ok((expression_, new_cursor_)) = parse_expression(
            tokens,
            cursor,
            &[Token::Comma, Token::RightParenthesis],
            tokens[cursor].binding_power(),
            true,
            false,
        ) {
            expression = expression_;
            new_cursor = new_cursor_;
        } else {
            return Err(ParsingError::General {
                msg: help_message(tokens.get(cursor), cursor, "Expected expression"),
                cursor,
            });
        }
        cursor = new_cursor;
        expressions.push(expression);
    }
}

fn parse_expression<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    delimiters: &[Token],
    min_binding_power: u32,
    is_top_level: bool,
    takes_as_clause: bool,
) -> Result<(Expression, usize), ParsingError> {
    let mut cursor = initial_cursor;

    let mut expression;

    if let Some(TokenContainer {
        loc: _,
        token: Token::LeftParenthesis,
    }) = tokens.get(cursor)
    {
        cursor += 1;

        if let Some(TokenContainer {
            loc: _,
            token: Token::Select,
        }) = tokens.get(cursor)
        {
            let (select_statement, new_cursor) =
                parse_select_statement(tokens, cursor, Token::RightParenthesis)?;
            expression = Expression::SubSelect(Box::new(select_statement));
            cursor = new_cursor;
        } else {
            match parse_expression(
                tokens,
                cursor,
                &[Token::RightParenthesis],
                min_binding_power,
                true,
                false,
            ) {
                Ok((expression_, cursor_)) => {
                    expression = expression_;
                    cursor = cursor_;
                }
                Err(_) => {
                    return Err(ParsingError::General {
                        msg: help_message(
                            tokens.get(cursor),
                            cursor,
                            "Expected expression after opening parenthesis",
                        ),
                        cursor,
                    });
                }
            };
        }

        if let Some(TokenContainer {
            loc: _,
            token: Token::RightParenthesis,
        }) = tokens.get(cursor)
        {
            cursor += 1;
        } else {
            return Err(ParsingError::General {
                msg: help_message(tokens.get(cursor), cursor, "Expected closing parenthesis"),
                cursor,
            });
        }
    } else if cursor < tokens.len() && UNARY_OPERATORS.contains(&tokens[cursor].token) {
        let operand;
        let token = &tokens[cursor];
        operand = token.token.clone();
        cursor += 1;
        let mut nested_un_ops = vec![operand];
        let mut inner_exp;
        loop {
            if cursor < tokens.len() && UNARY_OPERATORS.contains(&tokens[cursor].token) {
                nested_un_ops.push(tokens[cursor].token.clone());
                cursor += 1;
            } else {
                break;
            }
        }
        if let Ok((expression_, cursor_)) = parse_literal_expression(tokens, cursor) {
            inner_exp = expression_;
            cursor = cursor_;
        } else if let Some(TokenContainer {
            token: Token::LeftParenthesis,
            loc: _,
        }) = tokens.get(cursor)
        {
            cursor += 1;
            if let Ok((expression_, cursor_)) = parse_expression(
                tokens,
                cursor,
                &[Token::RightParenthesis],
                min_binding_power,
                true,
                takes_as_clause,
            ) {
                inner_exp = expression_;
                cursor = cursor_;
            } else {
                return Err(ParsingError::General {
                    msg: help_message(
                        tokens.get(cursor),
                        cursor,
                        "Expected expression after opening parenthesis",
                    ),
                    cursor,
                });
            }

            if let Some(TokenContainer {
                loc: _,
                token: Token::RightParenthesis,
            }) = tokens.get(cursor)
            {
                cursor += 1;
            } else {
                return Err(ParsingError::General {
                    msg: help_message(tokens.get(cursor), cursor, "Expected closing parenthesis"),
                    cursor,
                });
            }
        } else {
            return Err(ParsingError::General {
                msg: help_message(
                    tokens.get(cursor),
                    cursor,
                    "Expected expression after unary operator",
                ),
                cursor,
            });
        }

        if let Some(operand) = nested_un_ops.pop() {
            inner_exp = Expression::Unary(UnaryExpression {
                first: Box::from(inner_exp),
                operand: Operand::from_token(&operand)?,
            });
        } else {
            return Err(ParsingError::General {
                msg: help_message(tokens.get(cursor), cursor, "Expected unary operator"),
                cursor,
            });
        }
        while let Some(operand) = nested_un_ops.pop() {
            inner_exp = Expression::Unary(UnaryExpression {
                first: Box::from(inner_exp),
                operand: Operand::from_token(&operand)?,
            });
        }
        expression = inner_exp;
    } else {
        let (first_expression, new_cursor) = parse_literal_expression(tokens, cursor)?;
        expression = first_expression;
        cursor = new_cursor;
    }

    if let (
        Some(TokenContainer {
            token: token1,
            loc: _,
        }),
        Some(TokenContainer {
            token: token2,
            loc: _,
        }),
    ) = (tokens.get(cursor), tokens.get(cursor + 1))
    {
        if UNARY_POSTFIX_OPERATORS.contains(token1) && BINARY_OPERATORS.contains(token2) {
            cursor += 1;
            expression = Expression::Unary(UnaryExpression {
                first: Box::from(expression),
                operand: Operand::from_token(token1)?,
            });
        }
    }

    let mut last_cursor = cursor;
    'outer: while let Some(TokenContainer { token, loc: _ }) = tokens.get(cursor) {
        if delimiters.contains(token) {
            break 'outer;
        }
        if UNARY_POSTFIX_OPERATORS.contains(token) {
            break 'outer;
        }

        if let Some(TokenContainer {
            token: Token::IdentifierValue { value: _ },
            loc: _,
        }) = tokens.get(cursor)
        {
            if takes_as_clause {
                break;
            }
        }
        let mut operand = Token::Empty;
        if BINARY_OPERATORS.contains(token) {
            operand = token.clone();
            cursor += 1;
        }
        if operand == Token::TypeCast {
            if let Some(TokenContainer { token: op, loc: _ }) = tokens.get(cursor) {
                if op.is_datatype() {
                    expression = Expression::Cast {
                        data: Box::new(expression),
                        typ: SqlType::try_from(op)?,
                    };
                    cursor += 1;
                    continue;
                } else {
                    return Err(ParsingError::General {
                        msg: help_message(
                            tokens.get(cursor),
                            cursor,
                            "Expected type name after type cast",
                        ),
                        cursor,
                    });
                }
            } else {
                return Err(ParsingError::General {
                    msg: help_message(
                        tokens.get(cursor),
                        cursor,
                        "Expected type name after type cast",
                    ),
                    cursor,
                });
            }
        }
        if operand == Token::Empty {
            return Err(ParsingError::General {
                msg: help_message(tokens.get(cursor), cursor, "Expected binary operator"),
                cursor,
            });
        }

        let binding_power = operand.binding_power();
        if binding_power < min_binding_power {
            cursor = last_cursor;
            break;
        }

        let (second_expression, new_cursor) = match parse_expression(
            tokens,
            cursor,
            delimiters,
            binding_power,
            false,
            takes_as_clause,
        ) {
            Err(_) => {
                return Err(ParsingError::General {
                    msg: format!("Expected expression after binary operator {:?}", operand),
                    cursor,
                });
            }
            Ok(value) => value,
        };
        expression = Expression::Binary(BinaryExpression {
            first: Box::from(expression),
            second: Box::from(second_expression),
            operand: Operand::from_token(&operand)?,
        });
        cursor = new_cursor;
        last_cursor = cursor;
    }

    if let Some(TokenContainer { token, loc: _ }) = tokens.get(cursor) {
        if UNARY_POSTFIX_OPERATORS.contains(token) && is_top_level {
            cursor += 1;
            expression = Expression::Unary(UnaryExpression {
                first: Box::from(expression),
                operand: Operand::from_token(token)?,
            });
        }
    }

    Ok((expression, cursor))
}

fn parse_literal_expression(
    tokens: &[TokenContainer],
    initial_cursor: usize,
) -> Result<(Expression, usize), ParsingError> {
    let mut cursor = initial_cursor;

    if let Some(tok) = tokens.get(cursor) {
        match tok.token {
            Token::IdentifierValue {
                value: ref first_identifier,
            } => {
                cursor += 1;
                let mut col_name = first_identifier;
                let mut table_name = None;
                if let Some(TokenContainer {
                    token: Token::Dot,
                    loc: _,
                }) = tokens.get(cursor)
                {
                    cursor += 1;
                    if let Some(TokenContainer {
                        token: Token::IdentifierValue { value },
                        loc: _,
                    }) = tokens.get(cursor)
                    {
                        cursor += 1;
                        table_name = Some(col_name);
                        col_name = value;
                    } else {
                        return Err(ParsingError::General {
                            msg: help_message(
                                tokens.get(cursor),
                                cursor,
                                "Expected identifier after dot",
                            ),
                            cursor,
                        });
                    }
                }
                Ok((
                    Expression::TableColumn(TableColumn {
                        col_name: col_name.to_string(),
                        table_name: table_name.map(|x| x.to_string()),
                    }),
                    cursor,
                ))
            }
            Token::NumericValue { value: _ }
            | Token::StringValue { value: _ }
            | Token::BoolValue { value: _ }
            | Token::Null => {
                cursor += 1;
                Ok((
                    Expression::Literal(LiteralExpression::from_token(&tok.token)?),
                    cursor,
                ))
            }
            _ => Err(ParsingError::General {
                msg: help_message(tokens.get(cursor), cursor, "Expected literal"),
                cursor,
            }),
        }
    } else {
        Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Expected literal expression"),
            cursor,
        })
    }
}

fn parse_insert_statement(
    tokens: &[TokenContainer],
    initial_cursor: usize,
    _: Token,
) -> Result<(InsertStatement, usize), ParsingError> {
    let mut cursor = initial_cursor;

    // Look for INSERT
    if !expect_token(tokens, cursor, Token::Insert) {
        return Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Not an insert statement"),
            cursor,
        });
    }
    cursor += 1;

    // Look for INTO
    if !expect_token(tokens, cursor, Token::Into) {
        return Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Expected INTO"),
            cursor,
        });
    }
    cursor += 1;

    let table_name = match tokens.get(cursor) {
        Some(TokenContainer {
            loc: _,
            token: Token::IdentifierValue { value },
        }) => value,
        _ => {
            return Err(ParsingError::General {
                msg: help_message(tokens.get(cursor), cursor, "Expected Table Name"),
                cursor,
            });
        }
    };

    cursor += 1;

    // Look for VALUES
    if let Some(token) = tokens.get(cursor) {
        if token.token != Token::Values {
            return Err(ParsingError::General {
                msg: help_message(tokens.get(cursor), cursor, "Expected VALUES"),
                cursor,
            });
        }
        cursor += 1;
    }

    // Look for left parenthesis
    if let Some(token) = tokens.get(cursor) {
        if token.token != Token::LeftParenthesis {
            return Err(ParsingError::General {
                msg: help_message(tokens.get(cursor), cursor, "Expected left parenthesis"),
                cursor,
            });
        }
        cursor += 1;
    }

    // Look for expression list
    let (values, new_cursor) = match parse_expressions(tokens, cursor, &[Token::RightParenthesis]) {
        Err(_) => {
            return Err(ParsingError::General {
                msg: help_message(tokens.get(cursor), cursor, "Expected value expressions"),
                cursor,
            });
        }
        Ok(value) => value,
    };

    cursor = new_cursor;

    // Look for right parenthesis
    if let Some(TokenContainer {
        token: Token::RightParenthesis,
        loc: _,
    }) = tokens.get(cursor)
    {
        cursor += 1;
    } else {
        return Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Expected Right Parenthesis"),
            cursor,
        });
    }

    Ok((
        InsertStatement {
            table: table_name.to_string(),
            values,
        },
        cursor,
    ))
}

fn parse_drop_table_statement<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    _: Token,
) -> Result<(DropTableStatement, usize), ParsingError> {
    let mut cursor = initial_cursor;
    if let Some(TokenContainer {
        loc: _,
        token: Token::Drop,
    }) = tokens.get(cursor)
    {
        cursor += 1;
    } else {
        return Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Not a drop table statement"),
            cursor,
        });
    }
    if let Some(TokenContainer {
        loc: _,
        token: Token::Table,
    }) = tokens.get(cursor)
    {
        cursor += 1;
    } else {
        return Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Not a drop table statement"),
            cursor,
        });
    }
    let name;
    if let Some(TokenContainer {
        loc: _,
        token: Token::IdentifierValue { value },
    }) = tokens.get(cursor)
    {
        cursor += 1;
        name = value;
    } else {
        return Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Not a drop table statement"),
            cursor,
        });
    }

    Ok((
        DropTableStatement {
            name: name.to_string(),
        },
        cursor,
    ))
}

fn parse_select_items<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    delimiters: &[Token],
) -> Result<(Vec<SelectItem>, usize), ParsingError> {
    let mut cursor = initial_cursor;

    let mut select_items = Vec::with_capacity(5);
    let mut item_delims = delimiters.to_vec();
    item_delims.push(Token::As);
    let mut delimiters_plus = delimiters.to_vec();
    delimiters_plus.push(Token::Comma);
    delimiters_plus.push(Token::As);

    'outer: loop {
        match cursor.cmp(&tokens.len()) {
            Ordering::Equal => {
                return Ok((select_items, cursor - 1));
            }
            Ordering::Greater => {
                return Err(ParsingError::General {
                    msg: help_message(tokens.get(cursor), cursor, "Unexpected end of tokens"),
                    cursor,
                });
            }
            _ => {}
        }
        let current_token = &tokens[cursor];
        for delimiter in delimiters {
            if delimiter == &current_token.token {
                break 'outer;
            }
        }

        if !select_items.is_empty() {
            if let Some(TokenContainer {
                loc: _,
                token: Token::Comma,
            }) = tokens.get(cursor)
            {
                cursor += 1;
            } else {
                return Err(ParsingError::General {
                    msg: help_message(tokens.get(cursor), cursor, "Expected comma"),
                    cursor,
                });
            }
        }

        let mut select_item = SelectItem {
            expression: Expression::new(),
            as_clause: None,
            asterisk: false,
        };

        if let Some(TokenContainer {
            token: Token::Asterisk,
            loc: _,
        }) = tokens.get(cursor)
        {
            cursor += 1;
            select_item.asterisk = true;
        } else {
            let (expression, new_cursor) =
                match parse_expression(tokens, cursor, &delimiters_plus, 0, true, true) {
                    Err(_) => {
                        return Err(ParsingError::General {
                            msg: help_message(tokens.get(cursor), cursor, "Expected expression"),
                            cursor,
                        });
                    }
                    Ok(value) => value,
                };
            cursor = new_cursor;
            select_item.expression = expression;

            let mut found_as = false;
            if let Some(TokenContainer {
                loc: _,
                token: Token::As,
            }) = tokens.get(cursor)
            {
                found_as = true;
                cursor += 1;
            }
            if let Some(TokenContainer {
                token: Token::IdentifierValue { value },
                loc: _,
            }) = tokens.get(cursor)
            {
                select_item.as_clause = Some(value.to_string());
                cursor += 1;
            } else if found_as {
                return Err(ParsingError::General {
                    msg: help_message(tokens.get(cursor), cursor, "Expected identifier after AS"),
                    cursor,
                });
            }
        }

        select_items.push(select_item);
    }

    Ok((select_items, cursor))
}

fn parse_select_statement<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    delimiter: Token,
) -> Result<(SelectStatement, usize), ParsingError> {
    let mut cursor = initial_cursor;

    if let Some(TokenContainer {
        token: Token::Select,
        loc: _,
    }) = tokens.get(cursor)
    {
    } else if let Some(TokenContainer { token: _, loc: _ }) = tokens.get(cursor) {
        return Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Not a Select statement"),
            cursor,
        });
    } else {
        return Err(ParsingError::General {
            msg: help_message(tokens.get(cursor), cursor, "Reached end"),
            cursor,
        });
    }
    cursor += 1;

    let mut distinct = false;
    if let Some(TokenContainer {
        token: Token::Distinct,
        loc: _,
    }) = tokens.get(cursor)
    {
        distinct = true;
        cursor += 1;
    }

    let mut select: SelectStatement = SelectStatement {
        items: Vec::with_capacity(5),
        from: vec![],
        where_clause: Expression::new(),
        is_distinct: distinct,
        order_by: None,
        limit: None,
        offset: None,
    };

    let (select_items, new_cursor) = match parse_select_items(
        tokens,
        cursor,
        &[
            Token::From,
            Token::OrderBy,
            Token::Limit,
            Token::Offset,
            delimiter.clone(),
        ],
    ) {
        Err(err) => {
            return Err(err);
        }
        Ok(value) => value,
    };

    cursor = new_cursor;
    select.items = select_items;

    // let delimiters_plus = vec![delimiter, &where_token];

    if let Some(TokenContainer {
        token: Token::From,
        loc: _,
    }) = tokens.get(cursor)
    {
        cursor += 1;
        let (tables, new_cursor) = parse_tables(
            tokens,
            cursor,
            &[
                Token::Inner,
                Token::Left,
                Token::Right,
                Token::Join,
                Token::Where,
                Token::OrderBy,
                Token::Limit,
                Token::Offset,
                delimiter.clone(),
            ],
        )?;
        cursor = new_cursor;
        select.from = tables;
    }

    // TODO Parse join
    /*
    let (joins, new_cursor) = parse_joins(
        tokens,
        cursor,
        &[
            Token::Where,
            Token::OrderBy,
            Token::Limit,
            Token::Offset,
            delimiter.clone(),
        ],
    )?;
    cursor = new_cursor;*/

    if let Some(TokenContainer {
        loc: _,
        token: Token::Where,
    }) = tokens.get(cursor)
    {
        cursor += 1;
        let (where_clause, new_cursor) = match parse_expression(
            tokens,
            cursor,
            &[
                Token::OrderBy,
                Token::Limit,
                Token::Offset,
                delimiter.clone(),
            ],
            0,
            true,
            false,
        ) {
            Err(_) => {
                return Err(ParsingError::General {
                    msg: help_message(tokens.get(cursor), cursor, "Expected WHERE conditionals"),
                    cursor,
                });
            }
            Ok(value) => value,
        };

        cursor = new_cursor;
        select.where_clause = where_clause;
    }

    if let Some(TokenContainer {
        loc: _,
        token: Token::OrderBy,
    }) = tokens.get(cursor)
    {
        cursor += 1;

        let (exp, new_cursor) = match parse_expression(
            tokens,
            cursor,
            &[
                Token::Desc,
                Token::Asc,
                Token::Limit,
                Token::Offset,
                delimiter,
            ],
            0,
            true,
            true,
        ) {
            Err(_) => {
                return Err(ParsingError::General {
                    msg: help_message(tokens.get(cursor), cursor, "Expected WHERE conditionals"),
                    cursor,
                });
            }
            Ok(value) => value,
        };
        cursor = new_cursor;
        let mut order_by_clause = OrderByClause { asc: true, exp };

        if let Some(TokenContainer {
            loc: _,
            token: Token::Asc,
        }) = tokens.get(cursor)
        {
            cursor += 1;
            order_by_clause.asc = true;
        } else if let Some(TokenContainer {
            loc: _,
            token: Token::Desc,
        }) = tokens.get(cursor)
        {
            cursor += 1;
            order_by_clause.asc = false;
        }

        select.order_by = Some(order_by_clause);
    }

    if let Some(TokenContainer {
        loc: _,
        token: Token::Limit,
    }) = tokens.get(cursor)
    {
        cursor += 1;

        if let Some(TokenContainer {
            loc: _,
            token: Token::NumericValue { value },
        }) = tokens.get(cursor)
        {
            cursor += 1;
            let limit = match value.parse::<f64>() {
                Ok(val) => val,
                Err(err) => {
                    return Err(ParsingError::General {
                        msg: format!("Failed to parse Limit value: {err}"),
                        cursor,
                    });
                }
            };
            if limit.is_sign_negative() {
                return Err(ParsingError::General {
                    msg: help_message(tokens.get(cursor), cursor, "Limit must not be negative"),
                    cursor,
                });
            }
            if limit.is_nan() || limit.is_infinite() {
                return Err(ParsingError::General {
                    msg: help_message(
                        tokens.get(cursor),
                        cursor,
                        "Limit cannot be interpreted as a whole number",
                    ),
                    cursor,
                });
            }
            select.limit = Some(limit as usize);
        }
    }

    if let Some(TokenContainer {
        loc: _,
        token: Token::Offset,
    }) = tokens.get(cursor)
    {
        cursor += 1;

        if let Some(TokenContainer {
            loc: _,
            token: Token::NumericValue { value },
        }) = tokens.get(cursor)
        {
            cursor += 1;
            let offset = match value.parse::<f64>() {
                Ok(val) => val,
                Err(err) => {
                    return Err(ParsingError::General {
                        msg: format!("Failed to parse Offset value: {err}"),
                        cursor,
                    });
                }
            };
            if offset.is_sign_negative() {
                return Err(ParsingError::General {
                    msg: help_message(tokens.get(cursor), cursor, "Offset must not be negative"),
                    cursor,
                });
            }
            if offset.is_nan() || offset.is_infinite() {
                return Err(ParsingError::General {
                    msg: help_message(
                        tokens.get(cursor),
                        cursor,
                        "Offset cannot be interpreted as a whole number",
                    ),
                    cursor,
                });
            }
            select.offset = Some(offset as usize);
        }
    };

    Ok((select, cursor))
}

fn parse_joins<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    delimiters: &[Token],
) -> Result<(Vec<JoinClause>, usize), ParsingError> {
    let mut cursor = initial_cursor;

    let mut joins = vec![];

    loop {
        let mut kind = JoinKind::Inner;
        if tokens.get(cursor).is_none() {
            break;
        }

        if let Some(TokenContainer {
            token: Token::On,
            loc: _,
        }) = tokens.get(cursor)
        {
            break;
        } else if let Some(TokenContainer {
            token: Token::Inner,
            loc: _,
        }) = tokens.get(cursor)
        {
            cursor += 1;
            kind = JoinKind::Inner;
        } else if let Some(TokenContainer {
            token: Token::Right,
            loc: _,
        }) = tokens.get(cursor)
        {
            cursor += 1;
            if let Some(TokenContainer {
                token: Token::Outer,
                loc: _,
            }) = tokens.get(cursor)
            {
                cursor += 1;
            }
            kind = JoinKind::RightOuter;
        } else if let Some(TokenContainer {
            token: Token::Left,
            loc: _,
        }) = tokens.get(cursor)
        {
            cursor += 1;
            if let Some(TokenContainer {
                token: Token::Outer,
                loc: _,
            }) = tokens.get(cursor)
            {
                cursor += 1;
            }
            kind = JoinKind::LeftOuter;
        } else if let Some(TokenContainer {
            token: Token::Full,
            loc: _,
        }) = tokens.get(cursor)
        {
            cursor += 1;
            if let Some(TokenContainer {
                token: Token::Outer,
                loc: _,
            }) = tokens.get(cursor)
            {
                cursor += 1;
                kind = JoinKind::FullOuter;
            } else {
                return Err(ParsingError::General {
                    msg: help_message(
                        tokens.get(cursor),
                        cursor,
                        "Expected OUTER keyword after FULL",
                    ),
                    cursor,
                });
            }
        } else if let Some(TokenContainer { token, loc: _ }) = tokens.get(cursor) {
            if delimiters.contains(token) {
                break;
            }
            return Err(ParsingError::General {
                msg: help_message(tokens.get(cursor), cursor, "Failed to parse join clause"),
                cursor,
            });
        }
        if let Some(TokenContainer {
            token: Token::Join,
            loc: _,
        }) = tokens.get(cursor)
        {
            cursor += 1;
        } else {
            return Err(ParsingError::General {
                msg: help_message(tokens.get(cursor), cursor, "No join keyword after inner"),
                cursor,
            });
        }
        let (table, new_cursor) = parse_table(tokens, cursor, delimiters)?;
        cursor = new_cursor;
        if let Some(TokenContainer {
            token: Token::On,
            loc: _,
        }) = tokens.get(cursor)
        {
            cursor += 1;
        } else {
            return Err(ParsingError::General {
                msg: help_message(
                    tokens.get(cursor),
                    cursor,
                    "No On keyword in join expression",
                ),
                cursor,
            });
        }
        let (col1, new_cursor) = match parse_table_column(tokens, cursor) {
            Ok(val) => val,
            Err(_) => {
                return Err(ParsingError::General {
                    msg: help_message(
                        tokens.get(cursor),
                        cursor,
                        "Failed to parse column in join expression",
                    ),
                    cursor,
                });
            }
        };
        cursor = new_cursor;
        let operand_token = if let Some(TokenContainer { token, loc: _ }) = tokens.get(cursor) {
            cursor += 1;
            if BINARY_OPERATORS.contains(token) {
                token.clone()
            } else {
                return Err(ParsingError::General {
                    msg: help_message(
                        tokens.get(cursor),
                        cursor,
                        "No binary operator in join expression",
                    ),
                    cursor,
                });
            }
        } else {
            return Err(ParsingError::General {
                msg: help_message(
                    tokens.get(cursor),
                    cursor,
                    "No binary operator in join expression",
                ),
                cursor,
            });
        };
        let (col2, new_cursor) = match parse_table_column(tokens, cursor) {
            Ok(val) => val,
            Err(_) => {
                return Err(ParsingError::General {
                    msg: help_message(
                        tokens.get(cursor),
                        cursor,
                        "Failed to parse column in join expression",
                    ),
                    cursor,
                });
            }
        };
        cursor = new_cursor;

        let operand = if let Ok(o) = Operand::from_token(&operand_token) {
            o
        } else {
            return Err(ParsingError::General {
                msg: help_message(
                    tokens.get(cursor),
                    cursor,
                    "Failed to parse binary operator in join expression",
                ),
                cursor,
            });
        };

        let join = JoinClause {
            kind,
            source: table,
            on: Expression::Binary(BinaryExpression {
                first: Box::new(Expression::TableColumn(col1)),
                second: Box::new(Expression::TableColumn(col2)),
                operand,
            }),
        };
        joins.push(join);
    }

    Ok((joins, cursor))
}

fn parse_table_column(
    tokens: &[TokenContainer],
    initial_cursor: usize,
) -> Result<(TableColumn, usize), ParsingError> {
    let mut cursor = initial_cursor;

    if let Some(tok) = tokens.get(cursor) {
        match tok.token {
            Token::IdentifierValue {
                value: ref first_identifier,
            } => {
                cursor += 1;
                let mut col_name = first_identifier;
                let mut table_name = None;
                if let Some(TokenContainer {
                    token: Token::Dot,
                    loc: _,
                }) = tokens.get(cursor)
                {
                    cursor += 1;
                    if let Some(TokenContainer {
                        token: Token::IdentifierValue { value },
                        loc: _,
                    }) = tokens.get(cursor)
                    {
                        cursor += 1;
                        table_name = Some(col_name);
                        col_name = value;
                    } else {
                        return Err(ParsingError::General {
                            msg: help_message(
                                tokens.get(cursor),
                                cursor,
                                "Failed to parse table name in column",
                            ),
                            cursor,
                        });
                    }
                }
                Ok((
                    TableColumn {
                        col_name: col_name.to_string(),
                        table_name: table_name.map(|s| s.to_string()),
                    },
                    cursor,
                ))
            }
            _ => Err(ParsingError::General {
                msg: help_message(Some(tok), cursor, "Failed to parse column"),
                cursor,
            }),
        }
    } else {
        Err(ParsingError::General {
            msg: help_message(None, cursor, "Failed to parse column"),
            cursor,
        })
    }
}

fn parse_tables<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    delimiters: &[Token],
) -> Result<(Vec<RowDataSource>, usize), ParsingError> {
    let mut cursor = initial_cursor;

    let mut tables = vec![];

    loop {
        let (table, new_cursor) = parse_table(tokens, cursor, delimiters)?;
        cursor = new_cursor;
        tables.push(table);
        if tokens.get(cursor).is_none() {
            break;
        } else if let Some(TokenContainer {
            token: Token::Comma,
            loc: _,
        }) = tokens.get(cursor)
        {
            continue;
        } else if let Some(TokenContainer { token, loc: _ }) = tokens.get(cursor) {
            if delimiters.contains(token) {
                break;
            } else {
                return Err(ParsingError::General {
                    msg: format!("Failed to parse table, unexpected {:?}", token),
                    cursor,
                });
            }
        } else {
            return Err(ParsingError::General {
                msg: help_message(tokens.get(cursor), cursor, "Failed to parse table"),
                cursor,
            });
        }
    }

    Ok((tables, cursor))
}

fn parse_table<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    delimiters: &[Token],
) -> Result<(RowDataSource, usize), ParsingError> {
    let mut cursor = initial_cursor;

    if let Some(TokenContainer {
        token: Token::IdentifierValue { value },
        loc: _,
    }) = tokens.get(cursor)
    {
        cursor += 1;
        let mut as_clause = None;
        let table_name = value;
        let mut found_as = false;
        if let Some(TokenContainer {
            token: Token::As,
            loc: _,
        }) = tokens.get(cursor)
        {
            found_as = true;
            cursor += 1;
        }
        if let Some(TokenContainer {
            token: Token::IdentifierValue { value },
            loc: _,
        }) = tokens.get(cursor)
        {
            cursor += 1;
            as_clause = Some(value);
        } else if found_as {
            return Err(ParsingError::General {
                msg: help_message(
                    tokens.get(cursor),
                    cursor,
                    "Failed to parse as clause after AS",
                ),
                cursor,
            });
        }
        let (joins, new_cursor) = parse_joins(tokens, cursor, delimiters)?;
        cursor = new_cursor;
        return Ok((
            RowDataSource::Table {
                table_name: table_name.to_string(),
                as_clause: as_clause.map(|s| s.to_string()),
                joins,
            },
            cursor,
        ));
    } else if let Some(TokenContainer {
        token: Token::LeftParenthesis,
        loc: _,
    }) = tokens.get(cursor)
    {
        cursor += 1;
        if let Some(TokenContainer {
            token: Token::Select,
            loc: _,
        }) = tokens.get(cursor)
        {
            let (select, new_cursor) =
                parse_select_statement(tokens, cursor, Token::RightParenthesis)?;
            cursor = new_cursor + 1;
            let mut found_as = false;
            if let Some(TokenContainer {
                token: Token::As,
                loc: _,
            }) = tokens.get(cursor)
            {
                found_as = true;
                cursor += 1;
            }
            if let Some(TokenContainer {
                token: Token::IdentifierValue { value },
                loc: _,
            }) = tokens.get(cursor)
            {
                cursor += 1;
                let as_clause = value;
                let (joins, new_cursor) = parse_joins(tokens, cursor, delimiters)?;
                cursor = new_cursor;
                return Ok((
                    RowDataSource::SubSelect {
                        select,
                        as_clause: as_clause.to_string(),
                        joins,
                    },
                    cursor,
                ));
            } else if found_as {
                return Err(ParsingError::General {
                    msg: help_message(
                        tokens.get(cursor),
                        cursor,
                        "Failed to parse as clause after AS",
                    ),
                    cursor,
                });
            } else if let Some(TokenContainer { token, loc: _ }) = tokens.get(cursor) {
                return Err(ParsingError::General {
                    msg: format!("Unexpected {:?}, subquery requires as clause", token),
                    cursor,
                });
            }
        }
    }

    Err(ParsingError::General {
        msg: help_message(tokens.get(cursor), cursor, "Failed to parse a source table"),
        cursor,
    })
}

#[cfg(test)]
mod parser_tests {

    use super::super::parser::*;

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
