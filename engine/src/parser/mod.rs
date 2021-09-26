use crate::sql_types::SqlType;

use super::ast::*;
use super::lexer::*;

use lazy_static;
use std::iter::*;

lazy_static! {
    static ref BINARY_OPERATORS: Vec<Token<'static>> = vec![
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
    static ref UNARY_OPERATORS: Vec<Token<'static>> = vec![
        Token::Minus,
        Token::Not,
        Token::FactorialPrefix,
        Token::SquareRoot,
        Token::CubeRoot,
        Token::AbsoluteValue,
        Token::CubeRoot,
        Token::BitwiseNot,
    ];
    static ref UNARY_POSTFIX_OPERATORS: Vec<Token<'static>> = vec![Token::Factorial];
}

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

    pub fn parse(&self, source: &str) -> Result<Ast, ParsingError> {
        let mut tokens = self.lexer.lex(source)?;

        let mut ast = Ast {
            statements: Vec::with_capacity(1),
        };

        let mut cursor: usize = 0;
        let mut first_statement = true;
        while cursor < tokens.len() {
            if !first_statement {
                let mut at_least_one_semicolon = false;
                while expect_token(&mut tokens[cursor..].iter(), cursor, Token::Semicolon) {
                    cursor += 1;
                    at_least_one_semicolon = true;
                }
                if !(first_statement || at_least_one_semicolon) {
                    return Err(ParsingError::Delimiter {
                        msg: help_message(
                            &tokens,
                            cursor,
                            "Expected Semicolon Delimiter between Statements".to_owned(),
                        ),
                        cursor,
                    });
                }
            }
            match parse_statement(&mut tokens, cursor, Token::Semicolon) {
                Ok((statement, new_cursor)) => {
                    cursor = new_cursor;

                    ast.statements.push(statement);
                    first_statement = false;
                }

                Err(err) => {
                    return Err(ParsingError::Delimiter {
                        msg: help_message(&tokens, cursor, err.to_string()),
                        cursor,
                    });
                }
            }
            if cursor == tokens.len() - 1 {
                break;
            }
        }

        Ok(ast)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
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
fn expect_token<'a>(
    tokens: &mut impl Iterator<Item = &'a TokenContainer<'a>>,
    _cursor: usize,
    token: Token,
) -> bool {
    let current_token = match tokens.next() {
        Some(value) => value,
        None => {
            return false;
        }
    };
    token == current_token.token
}

#[inline]
fn help_message(tokens: &[TokenContainer], cursor: usize, msg: String) -> String {
    let token: TokenContainer;
    if cursor == 0 {
        token = tokens[0].clone();
    } else if cursor + 1 < tokens.len() {
        token = tokens[cursor].clone();
    } else {
        token = tokens[cursor - 1].clone();
    }

    format!(
        "[{}, {}]: {}, got {:?}",
        token.loc.line, token.loc.col, msg, token.token,
    )
}

fn parse_statement(
    tokens: &mut Vec<TokenContainer>,
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
            Token::Delete => Err(ParsingError::General {
                msg: "Delete not implemented".to_string(),
                cursor,
            }),
            Token::Update => Err(ParsingError::General {
                msg: "Update not implemented".to_string(),
                cursor,
            }),
            Token::Alter => Err(ParsingError::General {
                msg: "Alter not implemented".to_string(),
                cursor,
            }),
            Token::IdentifierValue { value: _ } => Err(ParsingError::General {
                msg: "Assignment not implemented".to_string(),
                cursor,
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
                                msg: "Create constraint not implemented".to_string(),
                                cursor,
                            }),
                            _ => Err(ParsingError::General {
                                msg: "Invalid Create Statement".to_string(),
                                cursor,
                            }),
                        },
                        Token::Constraint => Err(ParsingError::General {
                            msg: "Create constraint not implemented".to_string(),
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
                            msg: "Invalid Create Statement".to_string(),
                            cursor,
                        }),
                    }
                } else {
                    Err(ParsingError::General {
                        msg: "Invalid Create Statement".to_string(),
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
                msg: help_message(tokens, cursor, "Expected a valid statement".to_string()),
                cursor,
            }),
        }
    } else {
        Err(ParsingError::General {
            msg: help_message(tokens, cursor, "Expected a valid statement".to_string()),
            cursor,
        })
    }
}

fn parse_column_definitions(
    tokens: &[TokenContainer],
    initial_cursor: usize,
    delimiter: Token,
) -> Result<(Vec<ColumnDefinition>, usize), ParsingError> {
    let mut cursor = initial_cursor;

    let mut column_definitions: Vec<ColumnDefinition> = Vec::with_capacity(5);

    loop {
        if cursor >= tokens.len() {
            return Err(ParsingError::General {
                msg: "Unexpected end of file.".to_string(),
                cursor,
            });
        }

        // Look for a delimiter
        let current_token = &tokens[cursor];
        if delimiter == current_token.token {
            break;
        }

        // Look for a comma
        if !column_definitions.is_empty() {
            if tokens[cursor].token != Token::Comma {
                return Err(ParsingError::General {
                    msg: help_message(tokens, cursor, "Expected Comma".to_owned()),
                    cursor,
                });
            }

            cursor += 1;
        }
        // Look for a column name
        let col_name = match &tokens[cursor].token {
            Token::IdentifierValue { value } => value,
            _ => {
                return Err(ParsingError::General {
                    msg: help_message(tokens, cursor, "Expected Column Name".to_owned()),
                    cursor,
                });
            }
        };
        cursor += 1;

        // Look for a column type
        if !tokens[cursor].token.is_datatype() {
            return Err(ParsingError::General {
                msg: help_message(tokens, cursor, "Expected Column Type".to_owned()),
                cursor,
            });
        }

        let col_type = &tokens[cursor];
        cursor += 1;

        // Look for primary key
        let mut is_primary_key = false;
        if cursor + 1 < tokens.len()
            && tokens[cursor].token == Token::Primary
            && tokens[cursor + 1].token == Token::Key
        {
            is_primary_key = true;
            cursor += 2;
        }

        column_definitions.push(ColumnDefinition {
            name: col_name.to_string(),
            data_type: SqlType::from_token(col_type.clone())?,
            is_primary_key,
        });
    }

    Ok((column_definitions, cursor))
}

fn parse_create_table_statement(
    tokens: &mut Vec<TokenContainer>,
    initial_cursor: usize,
    _: Token,
) -> Result<(CreateTableStatement, usize), ParsingError> {
    let mut cursor = initial_cursor;

    if !expect_token(&mut tokens[cursor..].iter(), cursor, Token::Create) {
        return Err(ParsingError::General {
            msg: "Not a create table statement".to_string(),
            cursor,
        });
    }
    cursor += 1;

    if !expect_token(&mut tokens[cursor..].iter(), cursor, Token::Table) {
        return Err(ParsingError::General {
            msg: "Expected table keyword".to_string(),
            cursor,
        });
    }
    cursor += 1;

    let curr_token = tokens[cursor].token.clone();
    let name = match curr_token {
        Token::IdentifierValue { value } => value,
        _ => {
            return Err(ParsingError::General {
                msg: help_message(tokens, cursor, "Expected Table Name".to_owned()),
                cursor,
            });
        }
    };
    cursor += 1;
    if tokens[cursor].token != Token::LeftParenthesis {
        return Err(ParsingError::General {
            msg: help_message(tokens, cursor, "Expected Left Parenthesis".to_owned()),
            cursor,
        });
    }
    cursor += 1;

    let (cols, new_cursor) = parse_column_definitions(tokens, cursor, Token::RightParenthesis)?;
    cursor = new_cursor;

    if tokens[cursor].token != Token::RightParenthesis {
        return Err(ParsingError::General {
            msg: help_message(tokens, cursor, "Expected Right Parenthesis".to_owned()),
            cursor,
        });
    }
    cursor += 1;

    Ok((
        CreateTableStatement {
            name: name.to_string(),
            cols,
        },
        cursor,
    ))
}

fn parse_create_index_statement(
    tokens: &mut Vec<TokenContainer>,
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
            msg: "Not a create index statement".to_string(),
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
            msg: "Not a create index statement".to_string(),
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
        name = value.clone();
    } else {
        return Err(ParsingError::General {
            msg: "Expected index name".to_string(),
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
            msg: "Expected on keyword".to_string(),
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
        table = value.clone();
    } else {
        return Err(ParsingError::General {
            msg: "Expected table name".to_string(),
            cursor,
        });
    }
    let (expression, cursor) = match parse_expression(tokens, cursor, &[delimiter], 0, true, false)
    {
        Some(value) => value,
        None => {
            return Err(ParsingError::General {
                msg: "Expected index expressions".to_string(),
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
) -> Option<(Vec<Expression>, usize)> {
    let mut cursor = initial_cursor;

    let mut expressions: Vec<Expression> = Vec::with_capacity(5);

    loop {
        if cursor >= tokens.len() {
            return None;
        }

        // Look for delimiter
        let current_token = &tokens[cursor];
        for delimiter in delimiters {
            if delimiter == &current_token.token {
                return Some((expressions, cursor));
            }
        }

        // Look for comma
        if !expressions.is_empty() {
            let mut tokens_iter = tokens[cursor..].iter();
            if !expect_token(&mut tokens_iter, cursor, Token::Comma) {
                help_message(tokens, cursor, "Expected Comma".to_owned());
                return None;
            }

            cursor += 1;
        }

        // Look for expression
        let (expression, new_cursor);
        if let Some((expression_, new_cursor_)) = parse_expression(
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
            help_message(tokens, cursor, "Expected expression".to_owned());
            return None;
        }
        cursor = new_cursor;
        expressions.push(expression);
    }
}

fn parse_expression(
    tokens: &[TokenContainer],
    initial_cursor: usize,
    delimiters: &[Token],
    min_binding_power: u32,
    is_top_level: bool,
    takes_as_clause: bool,
) -> Option<(Expression, usize)> {
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
                parse_select_statement(tokens, cursor, Token::RightParenthesis).ok()?;
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
                Some((expression_, cursor_)) => {
                    expression = expression_;
                    cursor = cursor_;
                }
                None => {
                    let x = help_message(
                        tokens,
                        cursor,
                        "Expected expression after opening parenthesis".to_string(),
                    );
                    println!("{}", x);
                    return None;
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
            let x = help_message(tokens, cursor, "Expected closing parenthesis".to_owned());
            println!("{}", x);
            return None;
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
        if let Some((expression_, cursor_)) = parse_literal_expression(tokens, cursor) {
            inner_exp = expression_;
            cursor = cursor_;
        } else if let Some(TokenContainer {
            token: Token::LeftParenthesis,
            loc: _,
        }) = tokens.get(cursor)
        {
            cursor += 1;
            if let Some((expression_, cursor_)) = parse_expression(
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
                let x = help_message(
                    tokens,
                    cursor,
                    "Expected expression after opening parenthesis".to_string(),
                );
                println!("{}", x);
                return None;
            }

            if let Some(TokenContainer {
                loc: _,
                token: Token::RightParenthesis,
            }) = tokens.get(cursor)
            {
                cursor += 1;
            } else {
                let x = help_message(tokens, cursor, "Expected closing parenthesis".to_owned());
                println!("{}", x);
                return None;
            }
        } else {
            return None;
        }

        if let Some(operand) = nested_un_ops.pop() {
            inner_exp = Expression::Unary(UnaryExpression {
                first: Box::from(inner_exp),
                operand: Operand::from_token(&operand)?,
            });
        } else {
            return None;
        }
        while let Some(operand) = nested_un_ops.pop() {
            inner_exp = Expression::Unary(UnaryExpression {
                first: Box::from(inner_exp),
                operand: Operand::from_token(&operand)?,
            });
        }
        expression = inner_exp;
    } else {
        let (first_expression, new_cursor) = match parse_literal_expression(tokens, cursor) {
            None => {
                return None;
            }
            Some(value) => value,
        };
        expression = first_expression;
        cursor = new_cursor;
    }

    if cursor < tokens.len()
        && UNARY_POSTFIX_OPERATORS.contains(&tokens[cursor].token)
        && cursor + 1 < tokens.len()
        && BINARY_OPERATORS.contains(&tokens[cursor + 1].token)
    {
        let token = &tokens[cursor];
        let operand = token.token.clone();

        cursor += 1;
        expression = Expression::Unary(UnaryExpression {
            first: Box::from(expression),
            operand: Operand::from_token(&operand)?,
        });
    }

    let mut last_cursor = cursor;
    'outer: while cursor < tokens.len() {
        if delimiters.contains(&tokens[cursor].token) {
            break 'outer;
        }
        if UNARY_POSTFIX_OPERATORS.contains(&tokens[cursor].token) {
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
        if cursor < tokens.len() && BINARY_OPERATORS.contains(&tokens[cursor].token) {
            let token = &tokens[cursor];
            operand = token.token.clone();
            cursor += 1;
        }
        if operand == Token::TypeCast {
            if cursor < tokens.len() && tokens[cursor].token.is_datatype() {
                expression = Expression::Cast {
                    data: Box::new(expression),
                    typ: SqlType::from_token(tokens[cursor].clone()).ok()?,
                };
                cursor += 1;
                continue;
            } else {
                let x = help_message(tokens, cursor, "Expected type for type cast".to_owned());
                println!("{}", x);
                return None;
            }
        }
        if operand == Token::Empty {
            let x = help_message(tokens, cursor, "Expected binary operator".to_owned());
            println!("{}", x);
            return None;
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
            None => {
                let x = help_message(tokens, cursor, "Expected right operand".to_owned());
                println!("{}", x);
                return None;
            }
            Some(value) => value,
        };
        expression = Expression::Binary(BinaryExpression {
            first: Box::from(expression),
            second: Box::from(second_expression),
            operand: Operand::from_token(&operand)?,
        });
        cursor = new_cursor;
        last_cursor = cursor;
    }

    if cursor < tokens.len()
        && UNARY_POSTFIX_OPERATORS.contains(&tokens[cursor].token)
        && is_top_level
    {
        let token = &tokens[cursor];
        let operand = token.token.clone();

        cursor += 1;
        expression = Expression::Unary(UnaryExpression {
            first: Box::from(expression),
            operand: Operand::from_token(&operand)?,
        });
    }

    Some((expression, cursor))
}

fn parse_literal_expression(
    tokens: &[TokenContainer],
    initial_cursor: usize,
) -> Option<(Expression, usize)> {
    let mut cursor = initial_cursor;

    if let Some(tok) = tokens.get(cursor) {
        match tok.token {
            Token::IdentifierValue {
                value: ref first_identifier,
            } => {
                cursor += 1;
                let mut col_name = first_identifier.clone();
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
                        col_name = value.clone();
                    } else {
                        return None;
                    }
                }
                Some((
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
                Some((
                    Expression::Literal(LiteralExpression::from_token(&tok.token)?),
                    cursor,
                ))
            }
            _ => None,
        }
    } else {
        None
    }
}

fn parse_insert_statement(
    tokens: &mut Vec<TokenContainer>,
    initial_cursor: usize,
    _: Token,
) -> Result<(InsertStatement, usize), ParsingError> {
    let mut cursor = initial_cursor;

    // Look for INSERT
    if !expect_token(&mut tokens[cursor..].iter(), cursor, Token::Insert) {
        return Err(ParsingError::General {
            msg: "Not an insert statement".to_string(),
            cursor,
        });
    }
    cursor += 1;

    // Look for INTO
    if !expect_token(&mut tokens[cursor..].iter(), cursor, Token::Into) {
        return Err(ParsingError::General {
            msg: help_message(tokens, cursor, "Expected INTO".to_owned()),
            cursor,
        });
    }
    cursor += 1;

    let table_name = match &tokens[cursor].token {
        Token::IdentifierValue { value } => value,
        _ => {
            return Err(ParsingError::General {
                msg: help_message(tokens, cursor, "Expected Table Name".to_owned()),
                cursor,
            });
        }
    };

    cursor += 1;

    // Look for VALUES
    if tokens[cursor].token != Token::Values {
        help_message(tokens, cursor, "Expected VALUES".to_owned());
        return Err(ParsingError::General {
            msg: help_message(tokens, cursor, "Expected VALUES".to_owned()),
            cursor,
        });
    }
    cursor += 1;

    // Look for left parenthesis
    if tokens[cursor].token != Token::LeftParenthesis {
        return Err(ParsingError::General {
            msg: help_message(tokens, cursor, "Expected Left Parenthesis".to_owned()),
            cursor,
        });
    }
    cursor += 1;

    // Look for expression list
    let (values, new_cursor) = match parse_expressions(tokens, cursor, &[Token::RightParenthesis]) {
        None => {
            return Err(ParsingError::General {
                msg: help_message(tokens, cursor, "Expected value expressions".to_owned()),
                cursor,
            });
        }
        Some(value) => value,
    };

    cursor = new_cursor;

    // Look for right parenthesis
    if tokens[cursor].token != Token::RightParenthesis {
        return Err(ParsingError::General {
            msg: help_message(tokens, cursor, "Expected Right Parenthesis".to_owned()),
            cursor,
        });
    }
    cursor += 1;

    Ok((
        InsertStatement {
            table: table_name.to_string(),
            values,
        },
        cursor,
    ))
}

fn parse_drop_table_statement(
    tokens: &mut Vec<TokenContainer>,
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
            msg: "Not a drop table statement".to_string(),
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
            msg: "Not a drop table statement".to_string(),
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
        name = value.clone();
    } else {
        return Err(ParsingError::General {
            msg: "Not a drop table statement".to_string(),
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

fn parse_select_items(
    tokens: &[TokenContainer],
    initial_cursor: usize,
    delimiters: &[Token],
) -> Option<(Vec<SelectItem>, usize)> {
    let mut cursor = initial_cursor;

    let mut select_items = Vec::with_capacity(5);
    let mut item_delims = delimiters.to_vec();
    item_delims.push(Token::As);
    let mut delimiters_plus = delimiters.to_vec();
    delimiters_plus.push(Token::Comma);
    delimiters_plus.push(Token::As);

    'outer: loop {
        if cursor == tokens.len() {
            return Some((select_items, cursor - 1));
        } else if cursor > tokens.len() {
            return None;
        }

        let current_token = &tokens[cursor];
        for delimiter in delimiters {
            if delimiter == &current_token.token {
                break 'outer;
            }
        }

        if !select_items.is_empty() {
            if tokens[cursor].token != Token::Comma {
                help_message(tokens, cursor, "Expected comma".to_owned());
                return None;
            }

            cursor += 1;
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
                    None => {
                        let x = help_message(tokens, cursor, "Expected expression".to_owned());
                        println!("{}", x);
                        return None;
                    }
                    Some(value) => value,
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
                let x = help_message(tokens, cursor, "Expected identifier after AS".to_owned());
                println!("{}", x);
                return None;
            }
        }

        select_items.push(select_item);
    }

    Some((select_items, cursor))
}

fn parse_select_statement(
    tokens: &[TokenContainer],
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
            msg: "Not a Select statement".to_string(),
            cursor,
        });
    } else {
        return Err(ParsingError::General {
            msg: "Reached end".to_string(),
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
        None => {
            return Err(ParsingError::General {
                msg: "Expected select items".to_string(),
                cursor,
            });
        }
        Some(value) => value,
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
            &vec![
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
        &vec![
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
            None => {
                return Err(ParsingError::General {
                    msg: help_message(tokens, cursor, "Expected WHERE conditionals".to_owned()),
                    cursor,
                });
            }
            Some(value) => value,
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
            None => {
                return Err(ParsingError::General {
                    msg: help_message(tokens, cursor, "Expected WHERE conditionals".to_owned()),
                    cursor,
                });
            }
            Some(value) => value,
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
                        msg: format!("Failed to parse Limit value: {}", err.to_string()),
                        cursor,
                    });
                }
            };
            if limit.is_sign_negative() {
                return Err(ParsingError::General {
                    msg: "Limit must not be negative".to_string(),
                    cursor,
                });
            }
            if limit.is_nan() || limit.is_infinite() {
                return Err(ParsingError::General {
                    msg: "Limit cannot be interpreted as a whole number".to_string(),
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
                        msg: format!("Failed to parse Offset value: {}", err.to_string()),
                        cursor,
                    });
                }
            };
            if offset.is_sign_negative() {
                return Err(ParsingError::General {
                    msg: "Offset must not be negative".to_string(),
                    cursor,
                });
            }
            if offset.is_nan() || offset.is_infinite() {
                return Err(ParsingError::General {
                    msg: "Offset cannot be interpreted as a whole number".to_string(),
                    cursor,
                });
            }
            select.offset = Some(offset as usize);
        }
    };

    Ok((select, cursor))
}

fn parse_joins(
    tokens: &[TokenContainer],
    initial_cursor: usize,
    delimiters: &[Token],
) -> Result<(Vec<JoinClause>, usize), ParsingError> {
    let mut cursor = initial_cursor;

    let mut joins = vec![];

    loop {
        let mut kind = JoinKind::Inner;
        if tokens.get(cursor).is_none() {
            break;
        } else if let Some(TokenContainer {
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
                    msg: "Expected OUTER keyword after FULL".to_string(),
                    cursor,
                });
            }
        } else if let Some(TokenContainer { token, loc: _ }) = tokens.get(cursor) {
            if delimiters.contains(token) {
                break;
            }
            return Err(ParsingError::General {
                msg: "Failed to parse join clause".to_string(),
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
                msg: "No join keyword after inner".to_string(),
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
                msg: "No On keyword in join expression".to_string(),
                cursor,
            });
        }
        let (col1, new_cursor) = match parse_table_column(tokens, cursor) {
            Some(val) => val,
            None => {
                return Err(ParsingError::General {
                    msg: "Failed to parse column in join expression".to_string(),
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
                    msg: "No binary operator in join expression".to_string(),
                    cursor,
                });
            }
        } else {
            return Err(ParsingError::General {
                msg: "No binary operator in join expression".to_string(),
                cursor,
            });
        };
        let (col2, new_cursor) = match parse_table_column(tokens, cursor) {
            Some(val) => val,
            None => {
                return Err(ParsingError::General {
                    msg: "Failed to parse column in join expression".to_string(),
                    cursor,
                });
            }
        };
        cursor = new_cursor;

        let operand = if let Some(o) = Operand::from_token(&operand_token) {
            o
        } else {
            return Err(ParsingError::General {
                msg: "Failed to parse binary operator in join expression".to_string(),
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
) -> Option<(TableColumn, usize)> {
    let mut cursor = initial_cursor;

    if let Some(tok) = tokens.get(cursor) {
        match tok.token {
            Token::IdentifierValue {
                value: ref first_identifier,
            } => {
                cursor += 1;
                let mut col_name = first_identifier.clone();
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
                        col_name = value.clone();
                    } else {
                        return None;
                    }
                }
                Some((
                    TableColumn {
                        col_name: col_name.to_string(),
                        table_name: table_name.map(|s| s.to_string()),
                    },
                    cursor,
                ))
            }
            _ => None,
        }
    } else {
        None
    }
}

fn parse_tables(
    tokens: &[TokenContainer],
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
                msg: "Failed to parse table".to_string(),
                cursor,
            });
        }
    }

    Ok((tables, cursor))
}

fn parse_table(
    tokens: &[TokenContainer],
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
        let table_name = value.clone();
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
            as_clause = Some(value.clone());
        } else if found_as {
            return Err(ParsingError::General {
                msg: "Failed to parse as clause after AS".to_string(),
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
                let as_clause = value.clone();
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
                    msg: "Failed to parse as clause after AS".to_string(),
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
        msg: "Failed to parse a source table".to_string(),
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
                        table: "users".to_owned(),
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
                                name: "id".to_owned(),
                                data_type: SqlType::Int,
                                is_primary_key: false,
                            },
                            ColumnDefinition {
                                name: "name".to_owned(),
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
                input: "SELECT distinct id, name AS fullname FROM users;",
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
            panic!("{}", err_msg);
        }
    }
}
