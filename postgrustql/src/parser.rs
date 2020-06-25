use super::ast::*;
use super::lexer::*;

use std::iter::*;

fn token_from_keyword(k: Keyword) -> Token {
    return Token {
        kind: TokenKind::KeywordKind,
        value: k.to_owned(),
        loc: TokenLocation { line: 0, col: 0 },
    };
}

fn token_from_symbol(s: Symbol) -> Token {
    return Token {
        kind: TokenKind::SymbolKind,
        value: s.to_owned(),
        loc: TokenLocation { line: 0, col: 0 },
    };
}

fn expect_token<'a>(
    tokens: &mut impl Iterator<Item = &'a Token>,
    _cursor: u32,
    token: Token,
) -> bool {
    let current_token = tokens.next();
    if current_token.is_none() {
        return false;
    }

    token.equals(current_token.unwrap())
}

fn help_message(tokens: &Vec<Token>, cursor: u32, msg: String) -> String {
    let token: Token;
    if cursor == 0 {
        token = tokens[0].clone();
    } else if cursor + 1 < tokens.len() as u32 {
        token = tokens[(cursor) as usize].clone();
    } else {
        token = tokens[(cursor - 1) as usize].clone();
    }

    format!(
        "[{}, {}]: {}, got {}",
        token.loc.line, token.loc.col, msg, token.value
    )
}

pub fn parse(source: &str) -> Result<Ast, String> {
    let mut tokens;
    let lexer = Lexer::new();
    match lexer.lex(source) {
        Ok(result) => {
            tokens = result;
        }
        Err(err) => {
            return Err(err);
        }
    }

    let mut a = Ast { statements: vec![] };

    let mut cursor: u32 = 0;
    while cursor < tokens.len() as u32 {
        let (statement, new_cursor) =
            parse_statement(&mut tokens, cursor, token_from_symbol(SEMICOLON_SYMBOL));
        match statement {
            Some(statement) => {
                cursor = new_cursor;

                a.statements.push(statement);

                let mut at_least_one_semicolon = false;
                while expect_token(
                    &mut tokens[cursor as usize..].into_iter(),
                    cursor,
                    token_from_symbol(SEMICOLON_SYMBOL),
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

            None => {
                return Err(help_message(
                    &tokens,
                    cursor,
                    "Expected Statement".to_owned(),
                ));
            }
        }
    }

    return Ok(a);
}

fn parse_statement(
    tokens: &mut Vec<Token>,
    initial_cursor: u32,
    _delimiter: Token,
) -> (Option<Statement>, u32) {
    let cursor = initial_cursor;
    let semicolon_token = token_from_symbol(SEMICOLON_SYMBOL);

    // Look for a SELECT statement
    let (select, new_cursor) = parse_select_statement(tokens, cursor, &semicolon_token);

    match select {
        Some(select) => {
            return (Some(Statement::SelectStatement(select)), new_cursor);
        }
        _ => {}
    }

    // Look for an INSERT statement
    let (insert, new_cursor) = parse_insert_statement(tokens, cursor, &semicolon_token);

    match insert {
        Some(insert) => {
            return (Some(Statement::InsertStatement(insert)), new_cursor);
        }
        _ => {}
    }

    // Look for a CREATE statement
    let create = parse_create_table_statement(tokens, cursor, &semicolon_token);

    match create {
        Ok((create, new_cursor)) => {
            return (Some(Statement::CreateTableStatement(create)), new_cursor);
        }
        _ => {}
    }

    (None, initial_cursor)
}

fn parse_column_definitions(
    tokens: &Vec<Token>,
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
        if delimiter.equals(&current_token) {
            break;
        }

        // Look for a comma
        if column_definitions.len() > 0 {
            if tokens[cursor as usize].kind != TokenKind::SymbolKind
                || tokens[cursor as usize].value != COMMA_SYMBOL
            {
                return Err(help_message(tokens, cursor, "Expected Comma".to_owned()));
            }

            cursor += 1;
        }

        // Look for a column name
        if tokens[cursor as usize].kind != TokenKind::IdentifierKind {
            return Err(help_message(
                tokens,
                cursor,
                "Expected Column Name".to_owned(),
            ));
        }

        let col_name = &tokens[cursor as usize];
        cursor += 1;

        // Look for a column type
        if tokens[cursor as usize].kind != TokenKind::KeywordKind {
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
    tokens: &mut Vec<Token>,
    initial_cursor: u32,
    _delimiter: &Token,
) -> Result<(CreateTableStatement, u32), String> {
    let mut cursor = initial_cursor;

    if !expect_token(
        &mut tokens[cursor as usize..].into_iter(),
        cursor,
        token_from_keyword(CREATE_KEYWORD),
    ) {
        return Err("Not a create table statement".to_string());
    }
    cursor += 1;

    if !expect_token(
        &mut tokens[cursor as usize..].into_iter(),
        cursor,
        token_from_keyword(TABLE_KEYWORD),
    ) {
        return Err("Expected table keyword".to_string());
    }
    cursor += 1;

    if &tokens[cursor as usize].kind != &TokenKind::IdentifierKind {
        return Err(help_message(
            tokens,
            cursor,
            "Expected Table Name".to_owned(),
        ));
    }

    let name = &tokens[cursor as usize];
    cursor += 1;
    if tokens[cursor as usize].kind != TokenKind::SymbolKind
        || tokens[cursor as usize].value != LEFT_PARENTHESIS_SYMBOL
    {
        return Err(help_message(
            tokens,
            cursor,
            "Expected Left Parenthesis".to_owned(),
        ));
    }
    cursor += 1;

    let (cols, new_cursor) =
        parse_column_definitions(&tokens, cursor, token_from_symbol(RIGHT_PARENTHESIS_SYMBOL))?;
    cursor = new_cursor;

    if tokens[cursor as usize].kind != TokenKind::SymbolKind
        || tokens[cursor as usize].value != RIGHT_PARENTHESIS_SYMBOL
    {
        return Err(help_message(
            tokens,
            cursor,
            "Expected Right Parenthesis".to_owned(),
        ));
    }
    cursor += 1;

    Ok((
        CreateTableStatement {
            name: name.clone(),
            cols,
        },
        cursor,
    ))
}

fn parse_expressions(
    tokens: &Vec<Token>,
    initial_cursor: u32,
    delimiters: &Vec<Token>,
) -> Option<(Vec<Expression>, u32)> {
    let mut cursor = initial_cursor;

    let mut expressions: Vec<Expression> = vec![];

    // let mut token_iter = tokens[initial_cursor as usize..].into_iter();

    loop {
        //break;
        if cursor >= tokens.len() as u32 {
            return None;
        }

        // Look for delimiter
        let current_token = &tokens[cursor as usize];
        for delimiter in delimiters {
            if delimiter.equals(&current_token) {
                return Some((expressions, cursor));
            }
        }

        // Look for comma
        if expressions.len() > 0 {
            let mut tokens_iter = tokens[cursor as usize..].into_iter();
            if !expect_token(&mut tokens_iter, cursor, token_from_symbol(COMMA_SYMBOL)) {
                help_message(tokens, cursor, "Expected Comma".to_owned());
                return None;
            }

            cursor += 1;
        }

        // Look for expression
        let expression_parse_result = parse_expression(
            tokens,
            cursor,
            &vec![
                &token_from_symbol(COMMA_SYMBOL),
                &token_from_symbol(RIGHT_PARENTHESIS_SYMBOL),
            ],
            tokens[cursor as usize].binding_power(),
        );
        if expression_parse_result.is_none() {
            help_message(tokens, cursor, "Expected expression".to_owned());
            return None;
        }

        let (expression, new_cursor) = expression_parse_result.unwrap();
        cursor = new_cursor;
        expressions.push(expression);
    }
}

fn parse_expression(
    tokens: &Vec<Token>,
    initial_cursor: u32,
    delimiters: &Vec<&Token>,
    min_binding_power: u32,
) -> Option<(Expression, u32)> {
    let mut cursor = initial_cursor;

    let mut expression = Expression::new();

    if tokens[cursor as usize].kind == TokenKind::SymbolKind
        && tokens[cursor as usize].value == LEFT_PARENTHESIS_SYMBOL
    {
        cursor += 1;

        let right_parenthesis_token = token_from_symbol(RIGHT_PARENTHESIS_SYMBOL);
        let mut delimiters_plus = delimiters.clone();
        delimiters_plus.push(&right_parenthesis_token);
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

        if tokens[cursor as usize].kind != TokenKind::SymbolKind
            || tokens[cursor as usize].value != RIGHT_PARENTHESIS_SYMBOL
        {
            help_message(tokens, cursor, "Expected closing parenthesis".to_owned());
            return None;
        }
    } else {
        let literal_expression_result = parse_literal_expression(tokens, cursor);

        if literal_expression_result.is_none() {
            return None;
        }

        let (first_expression, new_cursor) = literal_expression_result.unwrap();
        expression = first_expression;
        cursor = new_cursor;
    }

    let mut last_cursor = cursor;

    'outer: while cursor < tokens.len() as u32 {
        for delimiter in delimiters {
            if tokens[cursor as usize].kind == delimiter.kind
                && tokens[cursor as usize].value == delimiter.value
            {
                break 'outer;
            }
        }

        let binary_operators = vec![
            token_from_keyword(AND_KEYWORD),
            token_from_keyword(OR_KEYWORD),
            token_from_symbol(EQUAL_SYMBOL),
            token_from_symbol(NOT_EQUAL_SYMBOL),
            token_from_symbol(CONCAT_SYMBOL),
            token_from_symbol(PLUS_SYMBOL),
            token_from_symbol(MINUS_SYMBOL),
            token_from_symbol(LESS_THAN_SYMBOL),
            token_from_symbol(LESS_THAN_OR_EQUAL_SYMBOL),
            token_from_symbol(GREATER_THAN_SYMBOL),
            token_from_symbol(GREATER_THAN_OR_EQUAL_SYMBOL),
        ];

        let mut operand = Token::new();
        for binary_operator in binary_operators {
            if tokens[cursor as usize].kind == binary_operator.kind
                && tokens[cursor as usize].value == binary_operator.value
            {
                let token = &tokens[cursor as usize];
                operand = token.clone();
                cursor += 1;
                break;
            }
        }

        if operand.kind == TokenKind::Empty {
            help_message(tokens, cursor, "Expected binary operator".to_owned());
            return None;
        }

        let binding_power = operand.binding_power();
        if binding_power < min_binding_power {
            cursor = last_cursor;
            break;
        }

        let second_expression_result = parse_expression(tokens, cursor, delimiters, binding_power);
        if second_expression_result.is_none() {
            help_message(tokens, cursor, "Expected right operand".to_owned());
            return None;
        }
        let (second_expression, new_cursor) = second_expression_result.unwrap();
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

fn parse_literal_expression(tokens: &Vec<Token>, initial_cursor: u32) -> Option<(Expression, u32)> {
    let cursor = initial_cursor;

    let kinds = vec![
        TokenKind::IdentifierKind,
        TokenKind::NumericKind,
        TokenKind::StringKind,
    ];

    for kind in kinds {
        if tokens[cursor as usize].kind == kind {
            let token = &tokens[cursor as usize];
            return Some((
                Expression::Literal(LiteralExpression {
                    literal: token.clone(),
                }),
                cursor + 1,
            ));
        }
    }

    None
}

fn parse_insert_statement(
    tokens: &mut Vec<Token>,
    initial_cursor: u32,
    _: &Token,
) -> (Option<InsertStatement>, u32) {
    let mut cursor = initial_cursor;

    // Look for INSERT
    if !expect_token(
        &mut tokens[cursor as usize..].into_iter(),
        cursor,
        token_from_keyword(INSERT_KEYWORD),
    ) {
        return (None, initial_cursor);
    }
    cursor += 1;

    // Look for INTO
    if !expect_token(
        &mut tokens[cursor as usize..].into_iter(),
        cursor,
        token_from_keyword(INTO_KEYWORD),
    ) {
        help_message(tokens, cursor, "Expected INTO".to_owned());
        return (None, initial_cursor);
    }
    cursor += 1;

    if tokens[cursor as usize].kind != TokenKind::IdentifierKind {
        help_message(tokens, cursor, "Expected Table Name".to_owned());
        return (None, initial_cursor);
    }

    let table = &tokens[cursor as usize];
    cursor += 1;

    // Look for VALUES
    if tokens[cursor as usize].kind != TokenKind::KeywordKind
        || tokens[cursor as usize].value != VALUES_KEYWORD
    {
        help_message(tokens, cursor, "Expected VALUES".to_owned());
        return (None, initial_cursor);
    }
    cursor += 1;

    // Look for left parenthesis
    if tokens[cursor as usize].kind != TokenKind::SymbolKind
        || tokens[cursor as usize].value != LEFT_PARENTHESIS_SYMBOL
    {
        help_message(tokens, cursor, "Expected Left Parenthesis".to_owned());
        return (None, initial_cursor);
    }
    cursor += 1;

    // Look for expression list
    let values_result = parse_expressions(
        tokens,
        cursor,
        &vec![token_from_symbol(RIGHT_PARENTHESIS_SYMBOL)],
    );
    if values_result.is_none() {
        return (None, initial_cursor);
    }
    let (values, new_cursor) = values_result.unwrap();
    cursor = new_cursor;

    // Look for right parenthesis
    if tokens[cursor as usize].kind != TokenKind::SymbolKind
        || tokens[cursor as usize].value != RIGHT_PARENTHESIS_SYMBOL
    {
        help_message(tokens, cursor, "Expected Right Parenthesis".to_owned());
        return (None, initial_cursor);
    }
    cursor += 1;

    (
        Some(InsertStatement {
            table: table.clone(),
            values,
        }),
        cursor,
    )
}

fn parse_select_items(
    tokens: &mut Vec<Token>,
    initial_cursor: u32,
    delimiters: &Vec<&Token>,
) -> Option<(Vec<SelectItem>, u32)> {
    let mut cursor = initial_cursor;

    let mut select_items = vec![];

    'outer: loop {
        if cursor as usize >= tokens.len() {
            return None;
        }

        let current_token = &tokens[cursor as usize];
        for delimiter in delimiters {
            if delimiter.equals(&current_token) {
                break 'outer;
            }
        }

        if select_items.len() > 0 {
            if tokens[cursor as usize].kind != TokenKind::SymbolKind
                || tokens[cursor as usize].value != COMMA_SYMBOL
            {
                help_message(tokens, cursor, "Expected comma".to_owned());
                return None;
            }

            cursor += 1;
        }

        let mut select_item = SelectItem {
            expression: Expression::new(),
            as_clause: Token::new(),
            asterisk: false,
        };

        if tokens[cursor as usize].kind == TokenKind::SymbolKind
            && tokens[cursor as usize].value == ASTERISK_SYMBOL
        {
            cursor += 1;
            select_item.asterisk = true;
        } else {
            let as_token = &token_from_keyword(AS_KEYWORD);

            let mut delimiters_plus = delimiters.to_vec();
            let comma_token = token_from_symbol(COMMA_SYMBOL);
            delimiters_plus.push(&comma_token);
            delimiters_plus.push(&as_token);

            let expression_result = parse_expression(tokens, cursor, &delimiters_plus, 0);
            if expression_result.is_none() {
                help_message(tokens, cursor, "Expected expression".to_owned());
                return None;
            }

            let (expression, new_cursor) = expression_result.unwrap();
            cursor = new_cursor;
            select_item.expression = expression;

            if tokens[cursor as usize].kind == TokenKind::KeywordKind
                && tokens[cursor as usize].value == AS_KEYWORD
            {
                cursor += 1;
                if tokens[cursor as usize].kind != TokenKind::IdentifierKind {
                    help_message(tokens, cursor, "Expected identifier after AS".to_owned());
                    return None;
                }

                let identifier = &tokens[cursor as usize];

                cursor += 1;
                select_item.as_clause = identifier.clone();
            }
        }

        select_items.push(select_item);
    }

    return Some((select_items, cursor));
}

fn parse_select_statement(
    tokens: &mut Vec<Token>,
    initial_cursor: u32,
    delimiter: &Token,
) -> (Option<SelectStatement>, u32) {
    let mut cursor = initial_cursor;

    if tokens[cursor as usize].kind != TokenKind::KeywordKind
        || tokens[cursor as usize].value[0..1] != SELECT_KEYWORD[0..1]
        || tokens[cursor as usize].value != SELECT_KEYWORD
    {
        return (None, initial_cursor);
    }
    cursor += 1;

    let mut select: SelectStatement = SelectStatement {
        items: vec![],
        from: Token::new(),
        where_clause: Expression::new(),
    };

    let from_token = token_from_keyword(FROM_KEYWORD);

    let parse_select_items_result =
        parse_select_items(tokens, cursor, &vec![&from_token, &delimiter]);

    if parse_select_items_result.is_none() {
        return (None, initial_cursor);
    }

    let (select_items, new_cursor) = parse_select_items_result.unwrap();
    cursor = new_cursor;
    select.items = select_items;

    // let delimiters_plus = vec![delimiter, &where_token];

    if tokens[cursor as usize].kind == TokenKind::KeywordKind
        && tokens[cursor as usize].value == FROM_KEYWORD
    {
        cursor += 1;
        if tokens[cursor as usize].kind != TokenKind::IdentifierKind {
            help_message(tokens, cursor, "Expected FROM item".to_owned());
            return (None, initial_cursor);
        }

        let from_identifier = &tokens[cursor as usize];
        cursor += 1;
        select.from = from_identifier.clone();
    }

    if tokens[cursor as usize].kind == TokenKind::KeywordKind
        && tokens[cursor as usize].value == WHERE_KEYWORD
    {
        cursor += 1;
        let where_clause_result = parse_expression(tokens, cursor, &vec![&delimiter], 0);
        if where_clause_result.is_none() {
            help_message(tokens, cursor, "Expected WHERE conditionals".to_owned());
            return (None, initial_cursor);
        }
        let (where_clause, new_cursor) = where_clause_result.unwrap();
        cursor = new_cursor;
        select.where_clause = where_clause;
    }

    (Some(select), cursor)
}

#[cfg(test)]
mod parser_tests {
    use super::super::lexer::*;
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
