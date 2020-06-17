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
    pub item: Vec<Expression>,
    pub from: Token,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Expression {
    pub literal: Token,
    pub kind: ExpressionKind,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum ExpressionKind {
    LiteralKind,
}

fn token_from_keyword(k: Keyword) -> Token {
    return Token {
        kind: TokenKind::KeywordKind,
        value: k.to_owned(),
        loc: Location { line: 0, col: 0 },
    };
}

fn token_from_symbol(s: Symbol) -> Token {
    return Token {
        kind: TokenKind::KeywordKind,
        value: s.to_owned(),
        loc: Location { line: 0, col: 0 },
    };
}

fn expect_token(tokens: &Vec<Token>, cursor: u32, token: Token) -> bool {
    if cursor >= tokens.len() as u32 {
        return false;
    }
    return token.value == (tokens[cursor as usize].value);
}

fn help_message(tokens: &Vec<Token>, cursor: u32, msg: String) {
    let token: Token;
    if cursor < tokens.len() as u32 {
        token = tokens[cursor as usize].clone();
    } else {
        token = tokens[(cursor - 1) as usize].clone();
    }

    println!(
        "[{}, {}]: {}, got {}",
        token.loc.line, token.loc.col, msg, token.value
    );
}

pub fn parse(source: &str) -> Result<Ast, String> {
    let tokens;
    match lex(source) {
        Ok(result) => {
            tokens = result;
            for tok in &tokens {
                // println!("{} {:?}", tok.value, tok.kind);
            }
        }
        Err(err) => {
            return Err(err);
        }
    }

    let mut a = Ast { statements: vec![] };

    let mut cursor: u32 = 0;
    while cursor < tokens.len() as u32 {
        let (statement, new_cursor) =
            parse_statement(&tokens, cursor, token_from_symbol(SEMICOLON_SYMBOL));
        match statement {
            Some(statement) => {
                cursor = new_cursor;

                a.statements.push(statement);

                let mut at_least_one_semicolon = false;
                while expect_token(&tokens, cursor, token_from_symbol(SEMICOLON_SYMBOL)) {
                    cursor += 1;
                    at_least_one_semicolon = true;
                }

                if !at_least_one_semicolon {
                    help_message(
                        &tokens,
                        cursor,
                        "Expected Semicolon Delimiter between Statements".to_owned(),
                    );
                    return Err("Missing Semicolon between statements.".to_owned());
                }
            }
            None => {
                help_message(&tokens, cursor, "Expected Statement".to_owned());
                return Err("Failed to Parse, Expected statement.".to_owned());
            }
        }
    }

    return Ok(a);
}

fn parse_statement(
    tokens: &Vec<Token>,
    initial_cursor: u32,
    _delimiter: Token,
) -> (Option<Statement>, u32) {
    let cursor = initial_cursor;

    // Look for a SELECT statement
    let semicolon_token = token_from_symbol(SEMICOLON_SYMBOL);
    let (select, new_cursor) = parse_select_statement(tokens, cursor, semicolon_token.clone());

    match select {
        Some(select) => {
            return (Some(Statement::SelectStatement(select)), new_cursor);
        }
        _ => {}
    }

    // Look for an INSERT statement
    let (insert, new_cursor) = parse_insert_statement(tokens, cursor, semicolon_token.clone());

    match insert {
        Some(insert) => {
            return (Some(Statement::InsertStatement(insert)), new_cursor);
        }
        _ => {}
    }

    // Look for a CREATE statement
    let (create, new_cursor) =
        parse_create_table_statement(tokens, cursor, semicolon_token.clone());

    match create {
        Some(create) => {
            return (Some(Statement::CreateTableStatement(create)), new_cursor);
        }
        _ => {}
    }

    return (None, initial_cursor);
}

fn parse_token(tokens: &Vec<Token>, initial_cursor: u32, kind: TokenKind) -> Option<(Token, u32)> {
    let cursor = initial_cursor;

    if cursor >= tokens.len() as u32 {
        return None;
    }

    let current_token = &tokens[cursor as usize];
    if current_token.kind == kind {
        return Some((current_token.clone(), cursor + 1));
    }

    return None;
}

fn parse_column_definitions(
    tokens: &Vec<Token>,
    initial_cursor: u32,
    delimiter: Token,
) -> Option<(Vec<ColumnDefinition>, u32)> {
    let mut cursor = initial_cursor;

    let mut column_definitions: Vec<ColumnDefinition> = vec![];

    loop {
        if cursor >= tokens.len() as u32 {
            return None;
        }

        // Look for a delimiter
        // let current = tokens[cursor as usize].clone();
        // if delimiter.value == current.value {
        if expect_token(tokens, cursor, delimiter.clone()) {
            break;
        }

        // Look for a comma
        if column_definitions.len() > 0 {
            if !expect_token(tokens, cursor, token_from_symbol(COMMA_SYMBOL)) {
                help_message(tokens, cursor, "Expected Comma".to_owned());
                return None;
            }

            cursor += 1;
        }

        // Look for a column name
        let col_name_result = parse_token(tokens, cursor, TokenKind::IdentifierKind);

        if col_name_result.is_none() {
            help_message(tokens, cursor, "Expected Column Name".to_owned());
            return None;
        }

        let (col_name, new_cursor) = col_name_result.unwrap();
        cursor = new_cursor;

        // Look for a column type
        let col_type_result = parse_token(tokens, cursor, TokenKind::KeywordKind);

        if col_type_result.is_none() {
            help_message(tokens, cursor, "Expected Column Type".to_owned());
            return None;
        }

        let (col_type, new_cursor) = col_type_result.unwrap();
        cursor = new_cursor;

        column_definitions.push(ColumnDefinition {
            name: col_name,
            data_type: col_type,
        });
    }

    return Some((column_definitions, cursor));
}

fn parse_create_table_statement(
    tokens: &Vec<Token>,
    initial_cursor: u32,
    _delimiter: Token,
) -> (Option<CreateTableStatement>, u32) {
    let mut cursor = initial_cursor;

    if !expect_token(tokens, cursor, token_from_keyword(CREATE_KEYWORD)) {
        return (None, initial_cursor);
    }
    cursor += 1;

    if !expect_token(tokens, cursor, token_from_keyword(TABLE_KEYWORD)) {
        return (None, initial_cursor);
    }
    cursor += 1;

    let name_result = parse_token(tokens, cursor, TokenKind::IdentifierKind);

    if name_result.is_none() {
        help_message(tokens, cursor, "Expected Table Name".to_owned());
        return (None, initial_cursor);
    }

    let (name, new_cursor) = name_result.unwrap();
    cursor = new_cursor;
    if !expect_token(tokens, cursor, token_from_symbol(LEFT_PARENTHESIS_SYMBOL)) {
        help_message(tokens, cursor, "Expected Left Parenthesis".to_owned());
        return (None, initial_cursor);
    }
    cursor += 1;

    let cols_result =
        parse_column_definitions(tokens, cursor, token_from_symbol(RIGHT_PARENTHESIS_SYMBOL));

    if cols_result.is_none() {
        return (None, initial_cursor);
    }

    let (cols, new_cursor) = cols_result.unwrap();
    cursor = new_cursor;

    if !expect_token(tokens, cursor, token_from_symbol(RIGHT_PARENTHESIS_SYMBOL)) {
        help_message(tokens, cursor, "Expected Right Parenthesis".to_owned());
        return (None, initial_cursor);
    }
    cursor += 1;

    (Some(CreateTableStatement { name, cols }), cursor)
}

fn parse_expressions(
    tokens: &Vec<Token>,
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
        let current_token = tokens[cursor as usize].clone();
        for delimiter in delimiters {
            if *delimiter.value == current_token.value {
                return Some((expressions, cursor));
            }
        }

        // Look for comma
        if expressions.len() > 0 {
            if !expect_token(tokens, cursor, token_from_symbol(COMMA_SYMBOL)) {
                help_message(tokens, cursor, "Expected Comma".to_owned());
                return None;
            }

            cursor += 1;
        }

        // Look for expression
        let expression_parse_result =
            parse_expression(tokens, cursor, token_from_symbol(COMMA_SYMBOL));
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
    _: Token,
) -> Option<(Expression, u32)> {
    let cursor = initial_cursor;

    let kinds = vec![
        TokenKind::IdentifierKind,
        TokenKind::NumericKind,
        TokenKind::StringKind,
    ];

    for kind in kinds {
        let token_result = parse_token(tokens, cursor, kind);

        if token_result.is_some() {
            let (token, new_cursor) = token_result.unwrap();
            return Some((
                Expression {
                    literal: token,
                    kind: ExpressionKind::LiteralKind,
                },
                new_cursor,
            ));
        }
    }

    None
}

fn parse_insert_statement(
    tokens: &Vec<Token>,
    initial_cursor: u32,
    _: Token,
) -> (Option<InsertStatement>, u32) {
    let mut cursor = initial_cursor;

    // Look for INSERT
    if !expect_token(tokens, cursor, token_from_keyword(INSERT_KEYWORD)) {
        return (None, initial_cursor);
    }
    cursor += 1;

    // Look for INTO
    if !expect_token(tokens, cursor, token_from_keyword(INTO_KEYWORD)) {
        help_message(tokens, cursor, "Expected INTO".to_owned());
        return (None, initial_cursor);
    }
    cursor += 1;

    let table_result = parse_token(tokens, cursor, TokenKind::IdentifierKind);

    if table_result.is_none() {
        help_message(tokens, cursor, "Expected Table Name".to_owned());
        return (None, initial_cursor);
    }

    let (table, new_cursor) = table_result.unwrap();
    cursor = new_cursor;

    // Look for VALUES
    if !expect_token(tokens, cursor, token_from_keyword(VALUES_KEYWORD)) {
        help_message(tokens, cursor, "Expected VALUES".to_owned());
        return (None, initial_cursor);
    }
    cursor += 1;

    // Look for left parenthesis
    if !expect_token(tokens, cursor, token_from_symbol(LEFT_PARENTHESIS_SYMBOL)) {
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
    if !expect_token(tokens, cursor, token_from_symbol(RIGHT_PARENTHESIS_SYMBOL)) {
        help_message(tokens, cursor, "Expected Right Parenthesis".to_owned());
        return (None, initial_cursor);
    }
    cursor += 1;

    (Some(InsertStatement { table, values }), cursor)
}

fn parse_select_statement(
    tokens: &Vec<Token>,
    initial_cursor: u32,
    delimiter: Token,
) -> (Option<SelectStatement>, u32) {
    let mut cursor = initial_cursor;

    if !expect_token(tokens, cursor, token_from_keyword(SELECT_KEYWORD)) {
        return (None, initial_cursor);
    }
    cursor += 1;

    let mut select: SelectStatement = SelectStatement {
        item: vec![],
        from: delimiter.clone(),
    };

    let expressions_result = parse_expressions(
        tokens,
        cursor,
        &vec![token_from_keyword(FROM_KEYWORD), delimiter.clone()],
    );

    if expressions_result.is_none() {
        return (None, initial_cursor);
    }

    let (expressions, new_cursor) = expressions_result.unwrap();
    select.item = expressions;
    cursor = new_cursor;

    if expect_token(tokens, cursor, token_from_keyword(FROM_KEYWORD)) {
        cursor += 1;

        let from_result = parse_token(tokens, cursor, TokenKind::IdentifierKind);

        if from_result.is_none() {
            help_message(tokens, cursor, "Expected FROM".to_owned());
            return (None, initial_cursor);
        }

        let (from, new_cursor) = from_result.unwrap();
        select.from = from;
        cursor = new_cursor;
    }

    (Some(select), cursor)
}
