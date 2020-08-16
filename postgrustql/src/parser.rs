use super::ast::*;
use super::lexer::*;

use std::iter::*;

fn expect_token<'a>(
    tokens: &mut impl Iterator<Item = &'a TokenContainer>,
    _cursor: u32,
    token: Token,
) -> bool {
    let current_token = match tokens.next() {
        Some(value) => value,
        None => {
            return false;
        }
    };
    return token == current_token.token;
}

fn help_message(tokens: &Vec<TokenContainer>, cursor: u32, msg: String) -> String {
    let token: TokenContainer;
    if cursor == 0 {
        token = tokens[0].clone();
    } else if cursor + 1 < tokens.len() as u32 {
        token = tokens[(cursor) as usize].clone();
    } else {
        token = tokens[(cursor - 1) as usize].clone();
    }

    format!(
        "[{}, {}]: {}, got {:?}",
        token.loc.line, token.loc.col, msg, token.token,
    )
}

pub fn parse(source: &str) -> Result<Ast, String> {
    let lexer = Lexer::new();
    let mut tokens = lexer.lex(source)?;

    let mut ast = Ast { statements: vec![] };

    let mut cursor: u32 = 0;
    while cursor < tokens.len() as u32 {
        match parse_statement(&mut tokens, cursor, Token::Semicolon) {
            Ok((statement, new_cursor)) => {
                cursor = new_cursor;

                ast.statements.push(statement);

                let mut at_least_one_semicolon = false;
                while expect_token(
                    &mut tokens[cursor as usize..].into_iter(),
                    cursor,
                    Token::Semicolon,
                ) {
                    cursor += 1;
                    at_least_one_semicolon = true;
                }

                if !at_least_one_semicolon {
                    return Err(help_message(
                        &tokens,
                        cursor,
                        "Expected Semicolon Delimiter between Statements".to_owned(),
                    ));
                }
            }

            Err(err) => {
                return Err(help_message(&tokens, cursor, err));
            }
        }
    }

    return Ok(ast);
}

fn parse_statement(
    tokens: &mut Vec<TokenContainer>,
    initial_cursor: u32,
    _delimiter: Token,
) -> Result<(Statement, u32), String> {
    let cursor = initial_cursor;

    // Look for a SELECT statement

    match parse_select_statement(tokens, cursor, Token::Semicolon) {
        Ok((select, new_cursor)) => {
            return Ok((Statement::SelectStatement(select), new_cursor));
        }
        Err(_) => (),
    }

    // Look for an INSERT statement

    match parse_insert_statement(tokens, cursor, Token::Semicolon) {
        Ok((insert, new_cursor)) => {
            return Ok((Statement::InsertStatement(insert), new_cursor));
        }
        Err(_) => (),
    }

    // Look for a CREATE statement

    match parse_create_table_statement(tokens, cursor, Token::Semicolon) {
        Ok((create, new_cursor)) => {
            return Ok((Statement::CreateTableStatement(create), new_cursor));
        }
        Err(_) => (),
    }

    Err(help_message(
        tokens,
        cursor,
        "Expected a valid statemt".to_string(),
    ))
}

fn parse_column_definitions(
    tokens: &Vec<TokenContainer>,
    initial_cursor: u32,
    delimiter: Token,
) -> Result<(Vec<ColumnDefinition>, u32), String> {
    let mut cursor = initial_cursor;

    let mut column_definitions: Vec<ColumnDefinition> = vec![];

    loop {
        if cursor >= tokens.len() as u32 {
            return Err("Unexpected end of file.".to_string());
        }

        // Look for a delimiter
        let current_token = &tokens[cursor as usize];
        if delimiter == current_token.token {
            break;
        }

        // Look for a comma
        if column_definitions.len() > 0 {
            if tokens[cursor as usize].token != Token::Comma {
                return Err(help_message(tokens, cursor, "Expected Comma".to_owned()));
            }

            cursor += 1;
        }

        // Look for a column name
        let col_name = match &tokens[cursor as usize].token {
            Token::IdentifierValue { value } => value,
            _ => {
                return Err(help_message(
                    tokens,
                    cursor,
                    "Expected Column Name".to_owned(),
                ));
            }
        };

        cursor += 1;

        // Look for a column type
        if !token_is_datatype(&tokens[cursor as usize].token) {
            return Err(help_message(
                tokens,
                cursor,
                "Expected Column Type".to_owned(),
            ));
        }

        let col_type = &tokens[cursor as usize];
        cursor += 1;

        column_definitions.push(ColumnDefinition {
            name: col_name.clone(),
            data_type: col_type.clone(),
        });
    }

    Ok((column_definitions, cursor))
}

fn parse_create_table_statement(
    tokens: &mut Vec<TokenContainer>,
    initial_cursor: u32,
    _delimiter: Token,
) -> Result<(CreateTableStatement, u32), String> {
    let mut cursor = initial_cursor;

    if !expect_token(
        &mut tokens[cursor as usize..].into_iter(),
        cursor,
        Token::Create,
    ) {
        return Err("Not a create table statement".to_string());
    }
    cursor += 1;

    if !expect_token(
        &mut tokens[cursor as usize..].into_iter(),
        cursor,
        Token::Table,
    ) {
        return Err("Expected table keyword".to_string());
    }
    cursor += 1;

    let curr_token = tokens[cursor as usize].token.clone();
    let name = match curr_token {
        Token::IdentifierValue { value } => value,
        _ => {
            return Err(help_message(
                tokens,
                cursor,
                "Expected Table Name".to_owned(),
            ));
        }
    };
    cursor += 1;
    if tokens[cursor as usize].token != Token::LeftParenthesis {
        return Err(help_message(
            tokens,
            cursor,
            "Expected Left Parenthesis".to_owned(),
        ));
    }
    cursor += 1;

    let (cols, new_cursor) = parse_column_definitions(&tokens, cursor, Token::RightParenthesis)?;
    cursor = new_cursor;

    if tokens[cursor as usize].token != Token::RightParenthesis {
        return Err(help_message(
            tokens,
            cursor,
            "Expected Right Parenthesis".to_owned(),
        ));
    }
    cursor += 1;

    Ok((CreateTableStatement { name, cols }, cursor))
}

fn parse_expressions(
    tokens: &Vec<TokenContainer>,
    initial_cursor: u32,
    delimiters: &Vec<Token>,
) -> Option<(Vec<Expression>, u32)> {
    let mut cursor = initial_cursor;

    let mut expressions: Vec<Expression> = vec![];

    loop {
        if cursor >= tokens.len() as u32 {
            return None;
        }

        // Look for delimiter
        let current_token = &tokens[cursor as usize];
        for delimiter in delimiters {
            if delimiter == &current_token.token {
                return Some((expressions, cursor));
            }
        }

        // Look for comma
        if expressions.len() > 0 {
            let mut tokens_iter = tokens[cursor as usize..].into_iter();
            if !expect_token(&mut tokens_iter, cursor, Token::Comma) {
                help_message(tokens, cursor, "Expected Comma".to_owned());
                return None;
            }

            cursor += 1;
        }

        // Look for expression
        let (expression, new_cursor) = match parse_expression(
            tokens,
            cursor,
            &vec![Token::Comma, Token::RightParenthesis],
            tokens[cursor as usize].binding_power(),
        ) {
            None => {
                help_message(tokens, cursor, "Expected expression".to_owned());
                return None;
            }
            Some(value) => value,
        };

        cursor = new_cursor;
        expressions.push(expression);
    }
}

fn parse_expression(
    tokens: &Vec<TokenContainer>,
    initial_cursor: u32,
    delimiters: &Vec<Token>,
    min_binding_power: u32,
) -> Option<(Expression, u32)> {
    let mut cursor = initial_cursor;

    let mut expression = Expression::new();

    if tokens[cursor as usize].token == Token::LeftParenthesis {
        cursor += 1;

        let mut delimiters_plus = delimiters.clone();
        delimiters_plus.push(Token::RightParenthesis);
        let expression_result =
            parse_expression(tokens, cursor, &delimiters_plus, min_binding_power);
        if expression_result.is_none() {
            help_message(
                tokens,
                cursor,
                "Expected expression after opening parenthesis".to_owned(),
            );
            return None;
        }

        if tokens[cursor as usize].token != Token::RightParenthesis {
            help_message(tokens, cursor, "Expected closing parenthesis".to_owned());
            return None;
        }
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

    let mut last_cursor = cursor;

    'outer: while cursor < tokens.len() as u32 {
        for delimiter in delimiters {
            if tokens[cursor as usize].token == *delimiter {
                break 'outer;
            }
        }

        let binary_operators = vec![
            Token::And,
            Token::Or,
            Token::Equal,
            Token::NotEqual,
            Token::Concat,
            Token::Plus,
            Token::Minus,
            Token::LessThan,
            Token::LessThanOrEqual,
            Token::GreaterThan,
            Token::GreaterThanOrEqual,
        ];

        let mut operand = TokenContainer::new();
        for binary_operator in binary_operators {
            if tokens[cursor as usize].token == binary_operator {
                let token = &tokens[cursor as usize];
                operand = token.clone();
                cursor += 1;
                break;
            }
        }

        if operand.token == Token::Empty {
            help_message(tokens, cursor, "Expected binary operator".to_owned());
            return None;
        }

        let binding_power = operand.binding_power();
        if binding_power < min_binding_power {
            cursor = last_cursor;
            break;
        }

        let (second_expression, new_cursor) =
            match parse_expression(tokens, cursor, delimiters, binding_power) {
                None => {
                    help_message(tokens, cursor, "Expected right operand".to_owned());
                    return None;
                }
                Some(value) => value,
            };
        expression = Expression::Binary(BinaryExpression {
            first: Box::from(expression),
            second: Box::from(second_expression),
            operand,
        });
        cursor = new_cursor;
        last_cursor = cursor;
    }

    return Some((expression, cursor));
}

fn parse_literal_expression(
    tokens: &Vec<TokenContainer>,
    initial_cursor: u32,
) -> Option<(Expression, u32)> {
    let cursor = initial_cursor;

    match tokens[cursor as usize].token {
        Token::IdentifierValue { value: _ }
        | Token::NumericValue { value: _ }
        | Token::StringValue { value: _ } => {
            let token = &tokens[cursor as usize];
            return Some((
                Expression::Literal(LiteralExpression {
                    literal: token.clone(),
                }),
                cursor + 1,
            ));
        }
        _ => (),
    }

    None
}

fn parse_insert_statement(
    tokens: &mut Vec<TokenContainer>,
    initial_cursor: u32,
    _: Token,
) -> Result<(InsertStatement, u32), String> {
    let mut cursor = initial_cursor;

    // Look for INSERT
    if !expect_token(
        &mut tokens[cursor as usize..].into_iter(),
        cursor,
        Token::Insert,
    ) {
        return Err("Not an insert statement".to_string());
    }
    cursor += 1;

    // Look for INTO
    if !expect_token(
        &mut tokens[cursor as usize..].into_iter(),
        cursor,
        Token::Into,
    ) {
        return Err(help_message(tokens, cursor, "Expected INTO".to_owned()));
    }
    cursor += 1;

    let table_name = match &tokens[cursor as usize].token {
        Token::IdentifierValue { value } => value,
        _ => {
            return Err(help_message(
                tokens,
                cursor,
                "Expected Table Name".to_owned(),
            ));
        }
    };

    cursor += 1;

    // Look for VALUES
    if tokens[cursor as usize].token != Token::Values {
        help_message(tokens, cursor, "Expected VALUES".to_owned());
        return Err(help_message(tokens, cursor, "Expected VALUES".to_owned()));
    }
    cursor += 1;

    // Look for left parenthesis
    if tokens[cursor as usize].token != Token::LeftParenthesis {
        return Err(help_message(
            tokens,
            cursor,
            "Expected Left Parenthesis".to_owned(),
        ));
    }
    cursor += 1;

    // Look for expression list
    let (values, new_cursor) =
        match parse_expressions(tokens, cursor, &vec![Token::RightParenthesis]) {
            None => {
                return Err(help_message(
                    tokens,
                    cursor,
                    "Expected value expressions".to_owned(),
                ));
            }
            Some(value) => value,
        };

    cursor = new_cursor;

    // Look for right parenthesis
    if tokens[cursor as usize].token != Token::RightParenthesis {
        return Err(help_message(
            tokens,
            cursor,
            "Expected Right Parenthesis".to_owned(),
        ));
    }
    cursor += 1;

    Ok((
        InsertStatement {
            table: table_name.clone(),
            values,
        },
        cursor,
    ))
}

fn parse_select_items(
    tokens: &mut Vec<TokenContainer>,
    initial_cursor: u32,
    delimiters: &Vec<Token>,
) -> Option<(Vec<SelectItem>, u32)> {
    let mut cursor = initial_cursor;

    let mut select_items = vec![];

    'outer: loop {
        if cursor as usize >= tokens.len() {
            return None;
        }

        let current_token = &tokens[cursor as usize];
        for delimiter in delimiters {
            if delimiter == &current_token.token {
                break 'outer;
            }
        }

        if select_items.len() > 0 {
            if tokens[cursor as usize].token != Token::Comma {
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

        if tokens[cursor as usize].token == Token::Asterisk {
            cursor += 1;
            select_item.asterisk = true;
        } else {
            let mut delimiters_plus = delimiters.to_vec();
            delimiters_plus.push(Token::Comma);
            delimiters_plus.push(Token::As);

            let (expression, new_cursor) =
                match parse_expression(tokens, cursor, &delimiters_plus, 0) {
                    None => {
                        help_message(tokens, cursor, "Expected expression".to_owned());
                        return None;
                    }
                    Some(value) => value,
                };
            cursor = new_cursor;
            select_item.expression = expression;

            if tokens[cursor as usize].token == Token::As {
                cursor += 1;
                let curr_token = tokens[cursor as usize].token.clone();
                let as_name = match curr_token {
                    Token::IdentifierValue { value } => value,
                    _ => {
                        help_message(tokens, cursor, "Expected identifier after AS".to_owned());
                        return None;
                    }
                };

                cursor += 1;
                select_item.as_clause = Some(as_name);
            }
        }

        select_items.push(select_item);
    }

    return Some((select_items, cursor));
}

fn parse_select_statement(
    tokens: &mut Vec<TokenContainer>,
    initial_cursor: u32,
    delimiter: Token,
) -> Result<(SelectStatement, u32), String> {
    let mut cursor = initial_cursor;

    if tokens[cursor as usize].token != Token::Select {
        return Err("Not a select statement".to_string());
    }
    cursor += 1;

    let mut select: SelectStatement = SelectStatement {
        items: vec![],
        from: None,
        where_clause: Expression::new(),
    };

    let (select_items, new_cursor) =
        match parse_select_items(tokens, cursor, &vec![Token::From, delimiter.clone()]) {
            None => {
                return Err("Expected select items".to_string());
            }
            Some(value) => value,
        };

    cursor = new_cursor;
    select.items = select_items;

    // let delimiters_plus = vec![delimiter, &where_token];

    if tokens[cursor as usize].token == Token::From {
        cursor += 1;
        let from_name = match &tokens[cursor as usize].token {
            Token::IdentifierValue { value } => value,
            _ => {
                return Err(help_message(
                    tokens,
                    cursor,
                    "Expected FROM item".to_owned(),
                ));
            }
        };

        // let from_identifier = &tokens[cursor as usize];
        cursor += 1;
        select.from = Some(from_name.clone());
    }

    if tokens[cursor as usize].token == Token::Where {
        cursor += 1;
        let (where_clause, new_cursor) = match parse_expression(tokens, cursor, &vec![delimiter], 0)
        {
            None => {
                return Err(help_message(
                    tokens,
                    cursor,
                    "Expected WHERE conditionals".to_owned(),
                ));
            }
            Some(value) => value,
        };

        cursor = new_cursor;
        select.where_clause = where_clause;
    }

    Ok((select, cursor))
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
