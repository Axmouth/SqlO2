use super::ast::*;
use super::lexer::*;

use lazy_static;
use std::iter::*;

lazy_static! {
    static ref binary_operators: Vec<Token> = vec![
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
    ];
    static ref unary_operators: Vec<Token> = vec![
        Token::Minus,
        Token::Not,
        Token::FactorialPrefix,
        Token::SquareRoot,
        Token::CubeRoot,
        Token::AbsoluteValue,
        Token::CubeRoot,
        Token::BitwiseNot,
    ];
    static ref unary_postfix_operators: Vec<Token> = vec![Token::Factorial];
}

fn expect_token<'a>(
    tokens: &mut impl Iterator<Item = &'a TokenContainer>,
    _cursor: usize,
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

fn help_message(tokens: &Vec<TokenContainer>, cursor: usize, msg: String) -> String {
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

pub fn parse(source: &str) -> Result<Ast, String> {
    let lexer = Lexer::new();
    let mut tokens = lexer.lex(source)?;

    let mut ast = Ast { statements: vec![] };

    let mut cursor: usize = 0;
    let mut first_statement = true;
    while cursor < tokens.len() {
        if first_statement == false {
            let mut at_least_one_semicolon = false;
            while expect_token(&mut tokens[cursor..].into_iter(), cursor, Token::Semicolon) {
                cursor += 1;
                at_least_one_semicolon = true;
            }
            if first_statement == false && at_least_one_semicolon == false {
                return Err(help_message(
                    &tokens,
                    cursor,
                    "Expected Semicolon Delimiter between Statements".to_owned(),
                ));
            }
        }
        match parse_statement(&mut tokens, cursor, Token::Semicolon) {
            Ok((statement, new_cursor)) => {
                cursor = new_cursor;

                ast.statements.push(statement);
                first_statement = false;
            }

            Err(err) => {
                return Err(help_message(&tokens, cursor, err));
            }
        }
        if cursor == tokens.len() - 1 {
            break;
        }
    }

    return Ok(ast);
}

fn parse_statement(
    tokens: &mut Vec<TokenContainer>,
    initial_cursor: usize,
    delimiter: Token,
) -> Result<(Statement, usize), String> {
    let cursor = initial_cursor;

    if let Some(first_token) = tokens.get(cursor) {
        match first_token.token {
            Token::Select => {
                // Look for a SELECT statement

                match parse_select_statement(tokens, cursor, delimiter.clone()) {
                    Ok((select, new_cursor)) => {
                        return Ok((Statement::SelectStatement(select), new_cursor));
                    }
                    Err(err) => (Err(err)),
                }
            }
            Token::Insert => {
                // Look for an INSERT statement

                match parse_insert_statement(tokens, cursor, delimiter.clone()) {
                    Ok((insert, new_cursor)) => {
                        Ok((Statement::InsertStatement(insert), new_cursor))
                    }
                    Err(err) => (Err(err)),
                }
            }
            Token::Delete => Err("Delete not implemented".to_string()),
            Token::Update => Err("Update not implemented".to_string()),
            Token::Alter => Err("Alter not implemented".to_string()),
            Token::Drop => Err("Drop not implemented".to_string()),
            Token::IdentifierValue { value: _ } => Err("Assignment not implemented".to_string()),
            Token::Create => {
                if let Some(first_token) = tokens.get(cursor + 1) {
                    match first_token.token {
                        Token::Index => {
                            // Look for a CREATE INDEX statement

                            match parse_create_index_statement(tokens, cursor, delimiter.clone()) {
                                Ok((create_index, new_cursor)) => {
                                    Ok((Statement::CreateIndexStatement(create_index), new_cursor))
                                }
                                Err(err) => (Err(err)),
                            }
                        }
                        Token::Table => {
                            // Look for a CREATE TABLE statement
                            match parse_create_table_statement(tokens, cursor, delimiter.clone()) {
                                Ok((create_table, new_cursor)) => {
                                    Ok((Statement::CreateTableStatement(create_table), new_cursor))
                                }
                                Err(err) => (Err(err)),
                            }
                        }
                        _ => Err("Invalid Create Statement".to_string()),
                    }
                } else {
                    Err("Invalid Create Statement".to_string())
                }
            }
            _ => Err(help_message(
                tokens,
                cursor,
                "Expected a valid statement".to_string(),
            )),
        }
    } else {
        Err(help_message(
            tokens,
            cursor,
            "Expected a valid statement".to_string(),
        ))
    }
}

fn parse_column_definitions(
    tokens: &Vec<TokenContainer>,
    initial_cursor: usize,
    delimiter: Token,
) -> Result<(Vec<ColumnDefinition>, usize), String> {
    let mut cursor = initial_cursor;

    let mut column_definitions: Vec<ColumnDefinition> = vec![];

    loop {
        if cursor >= tokens.len() {
            return Err("Unexpected end of file.".to_string());
        }

        // Look for a delimiter
        let current_token = &tokens[cursor];
        if delimiter == current_token.token {
            break;
        }

        // Look for a comma
        if column_definitions.len() > 0 {
            if tokens[cursor].token != Token::Comma {
                return Err(help_message(tokens, cursor, "Expected Comma".to_owned()));
            }

            cursor += 1;
        }
        // Look for a column name
        let col_name = match &tokens[cursor].token {
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
        if !token_is_datatype(&tokens[cursor].token) {
            return Err(help_message(
                tokens,
                cursor,
                "Expected Column Type".to_owned(),
            ));
        }

        let col_type = &tokens[cursor];
        cursor += 1;

        // Look for primary key
        let mut is_primary_key = false;
        if cursor + 1 < tokens.len() {
            if &tokens[cursor].token == &Token::Primary && &tokens[cursor + 1].token == &Token::Key
            {
                is_primary_key = true;
                cursor += 2;
            }
        }

        column_definitions.push(ColumnDefinition {
            name: col_name.clone(),
            data_type: col_type.clone(),
            is_primary_key,
        });
    }

    Ok((column_definitions, cursor))
}

fn parse_create_table_statement(
    tokens: &mut Vec<TokenContainer>,
    initial_cursor: usize,
    _: Token,
) -> Result<(CreateTableStatement, usize), String> {
    let mut cursor = initial_cursor;

    if !expect_token(&mut tokens[cursor..].into_iter(), cursor, Token::Create) {
        return Err("Not a create table statement".to_string());
    }
    cursor += 1;

    if !expect_token(&mut tokens[cursor..].into_iter(), cursor, Token::Table) {
        return Err("Expected table keyword".to_string());
    }
    cursor += 1;

    let curr_token = tokens[cursor].token.clone();
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
    if tokens[cursor].token != Token::LeftParenthesis {
        return Err(help_message(
            tokens,
            cursor,
            "Expected Left Parenthesis".to_owned(),
        ));
    }
    cursor += 1;

    let (cols, new_cursor) = parse_column_definitions(&tokens, cursor, Token::RightParenthesis)?;
    cursor = new_cursor;

    if tokens[cursor].token != Token::RightParenthesis {
        return Err(help_message(
            tokens,
            cursor,
            "Expected Right Parenthesis".to_owned(),
        ));
    }
    cursor += 1;

    Ok((CreateTableStatement { name, cols }, cursor))
}

fn parse_create_index_statement(
    tokens: &mut Vec<TokenContainer>,
    initial_cursor: usize,
    delimiter: Token,
) -> Result<(CreateIndexStatement, usize), String> {
    let mut cursor = initial_cursor;
    if let Some(TokenContainer {
        loc: _,
        token: Token::Create,
    }) = tokens.get(cursor)
    {
        cursor += 1;
    } else {
        return Err("Not a create index statement".to_string());
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
        return Err("Not a create index statement".to_string());
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
        return Err("Expected index name".to_string());
    }
    if let Some(TokenContainer {
        loc: _,
        token: Token::On,
    }) = tokens.get(cursor)
    {
        cursor += 1;
    } else {
        return Err("Expected on keyword".to_string());
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
        return Err("Expected table name".to_string());
    }
    let (expression, cursor) = match parse_expression(tokens, cursor, &vec![delimiter], 0) {
        Some(value) => value,
        None => {
            return Err("Expected index expressions".to_string());
        }
    };

    Ok((
        CreateIndexStatement {
            is_primary_key: false,
            is_unique,
            name,
            expression,
            table,
        },
        cursor,
    ))
}

fn parse_expressions(
    tokens: &Vec<TokenContainer>,
    initial_cursor: usize,
    delimiters: &Vec<Token>,
) -> Option<(Vec<Expression>, usize)> {
    let mut cursor = initial_cursor;

    let mut expressions: Vec<Expression> = vec![];

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
        if expressions.len() > 0 {
            let mut tokens_iter = tokens[cursor..].into_iter();
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
            &vec![Token::Comma, Token::RightParenthesis],
            tokens[cursor].binding_power(),
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
    tokens: &Vec<TokenContainer>,
    initial_cursor: usize,
    delimiters: &Vec<Token>,
    min_binding_power: u32,
) -> Option<(Expression, usize)> {
    let mut cursor = initial_cursor;

    let mut expression;

    if let Some(TokenContainer {
        loc: _,
        token: Token::LeftParenthesis,
    }) = tokens.get(cursor)
    {
        cursor += 1;

        //let mut delimiters_plus = delimiters.clone();
        //delimiters_plus.push(Token::RightParenthesis);
        match parse_expression(
            tokens,
            cursor,
            &vec![Token::RightParenthesis],
            min_binding_power,
        ) {
            Some((expression_, cursor_)) => {
                expression = expression_;
                cursor = cursor_;
            }
            None => {
                help_message(
                    tokens,
                    cursor,
                    "Expected expression after opening parenthesis".to_string(),
                );
                return None;
            }
        };

        if let Some(TokenContainer {
            loc: _,
            token: Token::RightParenthesis,
        }) = tokens.get(cursor)
        {
            cursor += 1;
        } else {
            help_message(tokens, cursor, "Expected closing parenthesis".to_owned());
            return None;
        }
    } else if unary_operators.contains(&tokens[cursor].token) {
        let operand;
        let token = &tokens[cursor];
        operand = token.clone();
        cursor += 1;
        let mut nested_un_ops = vec![operand.clone()];
        let mut inner_exp;
        loop {
            if cursor < tokens.len() && unary_operators.contains(&tokens[cursor].token) {
                nested_un_ops.push(tokens[cursor].clone());
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
                &vec![Token::RightParenthesis],
                min_binding_power,
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
                operand,
            });
        } else {
            return None;
        }
        while let Some(operand) = nested_un_ops.pop() {
            inner_exp = Expression::Unary(UnaryExpression {
                first: Box::from(inner_exp),
                operand,
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

    let mut last_cursor = cursor;

    'outer: while cursor < tokens.len() {
        for delimiter in delimiters {
            if tokens[cursor].token == *delimiter {
                break 'outer;
            }
        }

        let mut operand = TokenContainer::new();
        if binary_operators.contains(&tokens[cursor].token) {
            let token = &tokens[cursor];
            operand = token.clone();
            cursor += 1;
        } /*else if unary_postfix_operators.contains(&tokens[cursor].token) {
              let token = &tokens[cursor];
              operand = token.clone();
              cursor += 1;
              expression = Expression::Unary(UnaryExpression {
                  first: Box::from(expression),
                  operand,
              });
              return Some((expression, cursor));
          }*/

        if operand.token == Token::Empty {
            help_message(tokens, cursor, "Expected binary operator".to_owned());
            return None;
        }

        let min_binding_power = operand.binding_power();
        if min_binding_power < min_binding_power {
            cursor = last_cursor;
            break;
        }

        let (second_expression, new_cursor) =
            match parse_expression(tokens, cursor, delimiters, min_binding_power) {
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

fn parse_unary_expression(
    tokens: &Vec<TokenContainer>,
    initial_cursor: usize,
    delimiters: &Vec<Token>,
    min_binding_power: u32,
) -> Option<(Expression, usize)> {
    let mut cursor = initial_cursor;

    let mut expression;

    if let Some(TokenContainer {
        loc: _,
        token: Token::LeftParenthesis,
    }) = tokens.get(cursor)
    {
        cursor += 1;
        println!("{}", "fddddddddddd");

        //let mut delimiters_plus = delimiters.clone();
        //delimiters_plus.push(Token::RightParenthesis);
        if let Some((expression_, cursor_)) = parse_expression(
            tokens,
            cursor,
            &vec![Token::RightParenthesis],
            min_binding_power,
        ) {
            expression = expression_;
            cursor = cursor_;
        } else if let Some((expression_, cursor_)) = parse_unary_expression(
            tokens,
            cursor,
            &vec![Token::RightParenthesis],
            min_binding_power,
        ) {
            expression = expression_;
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

        if tokens[cursor].token != Token::RightParenthesis {
            let x = help_message(tokens, cursor, "Expected closing parenthesis".to_owned());
            println!("{}", x);
            return None;
        }
        cursor += 1;
    } else if unary_operators.contains(&tokens[cursor].token) {
        let operand;
        let token = &tokens[cursor];
        operand = token.clone();
        cursor += 1;
        //let del1 = vec![Token::RightParenthesis];
        //let mut del = delimiters.clone();
        //let delimiters = if let Some(TokenContainer {
        //    token: Token::LeftParenthesis,
        //    loc: _,
        //}) = &tokens.get(cursor + 1)
        //{
        //    &del1
        //} else {
        //    del.append(&mut binary_operators.clone());
        //    &del
        //};
        let (mut expression, cursor) =
            parse_unary_expression(tokens, cursor, &delimiters, min_binding_power)?;
        expression = Expression::Unary(UnaryExpression {
            first: Box::from(expression),
            operand,
        });
        println!("{}", "ffffffff");
        return Some((expression, cursor));
    } else {
        let (first_expression, new_cursor) = match parse_literal_expression(tokens, cursor) {
            None => {
                println!("dero");
                return None;
            }
            Some(value) => value,
        };
        expression = first_expression;
        cursor = new_cursor;
    }

    let mut last_cursor = cursor;

    'outer: while cursor < tokens.len() {
        for delimiter in delimiters {
            if tokens[cursor].token == *delimiter {
                break 'outer;
            }
        }

        let mut operand = TokenContainer::new();
        if unary_postfix_operators.contains(&tokens[cursor].token) {
            let token = &tokens[cursor];
            operand = token.clone();
            cursor += 1;
            expression = Expression::Unary(UnaryExpression {
                first: Box::from(expression),
                operand,
            });
            return Some((expression, cursor));
        }

        if operand.token == Token::Empty {
            let x = help_message(tokens, cursor, "Expected unary operator".to_owned());
            println!("{}", x);
            return None;
        }

        let min_binding_power = operand.binding_power();
        if min_binding_power < min_binding_power {
            cursor = last_cursor;
            break;
        }

        let (second_expression, new_cursor) =
            match parse_expression(tokens, cursor, delimiters, min_binding_power) {
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
            operand,
        });
        cursor = new_cursor;
        last_cursor = cursor;
    }
    println!("{}", "dfdfdf");
    return Some((expression, cursor));
}

fn parse_literal_expression(
    tokens: &Vec<TokenContainer>,
    initial_cursor: usize,
) -> Option<(Expression, usize)> {
    let cursor = initial_cursor;

    if let Some(tok) = tokens.get(cursor) {
        match tok.token {
            Token::IdentifierValue { value: _ }
            | Token::NumericValue { value: _ }
            | Token::StringValue { value: _ }
            | Token::BoolValue { value: _ }
            | Token::Null => {
                let token = &tokens[cursor];
                return Some((
                    Expression::Literal(LiteralExpression {
                        literal: token.clone(),
                    }),
                    cursor + 1,
                ));
            }
            _ => (),
        }
    }

    None
}

fn parse_insert_statement(
    tokens: &mut Vec<TokenContainer>,
    initial_cursor: usize,
    _: Token,
) -> Result<(InsertStatement, usize), String> {
    let mut cursor = initial_cursor;

    // Look for INSERT
    if !expect_token(&mut tokens[cursor..].into_iter(), cursor, Token::Insert) {
        return Err("Not an insert statement".to_string());
    }
    cursor += 1;

    // Look for INTO
    if !expect_token(&mut tokens[cursor..].into_iter(), cursor, Token::Into) {
        return Err(help_message(tokens, cursor, "Expected INTO".to_owned()));
    }
    cursor += 1;

    let table_name = match &tokens[cursor].token {
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
    if tokens[cursor].token != Token::Values {
        help_message(tokens, cursor, "Expected VALUES".to_owned());
        return Err(help_message(tokens, cursor, "Expected VALUES".to_owned()));
    }
    cursor += 1;

    // Look for left parenthesis
    if tokens[cursor].token != Token::LeftParenthesis {
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
    if tokens[cursor].token != Token::RightParenthesis {
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
    initial_cursor: usize,
    delimiters: &Vec<Token>,
) -> Option<(Vec<SelectItem>, usize)> {
    let mut cursor = initial_cursor;

    let mut select_items = vec![];

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

        if select_items.len() > 0 {
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

        if tokens[cursor].token == Token::Asterisk {
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

            if let Some(TokenContainer {
                loc: _,
                token: Token::As,
            }) = tokens.get(cursor)
            {
                cursor += 1;
                let curr_token = tokens[cursor].token.clone();
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
    initial_cursor: usize,
    delimiter: Token,
) -> Result<(SelectStatement, usize), String> {
    let mut cursor = initial_cursor;

    if tokens[cursor].token != Token::Select {
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

    if tokens[cursor].token == Token::From {
        cursor += 1;
        let from_name = match &tokens[cursor].token {
            Token::IdentifierValue { value } => value,
            _ => {
                return Err(help_message(
                    tokens,
                    cursor,
                    "Expected FROM item".to_owned(),
                ));
            }
        };

        // let from_identifier = &tokens[cursor ];
        cursor += 1;
        select.from = Some(from_name.clone());
    }

    if let Some(TokenContainer {
        loc: _,
        token: Token::Where,
    }) = tokens.get(cursor)
    {
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
