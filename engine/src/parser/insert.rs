use super::*;

pub fn parse_insert_statement(
    tokens: &[TokenContainer],
    initial_cursor: usize,
    _: Token,
) -> Result<(InsertStatement, usize), ParsingError> {
    let mut cursor = initial_cursor;

    // Look for INSERT
    if !expect_token(tokens, cursor, Token::Insert) {
        parse_err!(tokens, cursor, "Not an Insert Statement");
    }
    cursor += 1;

    // Look for INTO
    if !expect_token(tokens, cursor, Token::Into) {
        parse_err!(tokens, cursor, "Expected INTO");
    }
    cursor += 1;

    let table_name = match tokens.get(cursor) {
        Some(TokenContainer {
            loc: _,
            token: Token::IdentifierValue { value },
        }) => value,
        _ => {
            parse_err!(tokens, cursor, "Expected Table Name");
        }
    };

    cursor += 1;

    // Look for VALUES
    if let Some(token) = tokens.get(cursor) {
        if token.token != Token::Values {
            parse_err!(tokens, cursor, "Expected VALUES");
        }
        cursor += 1;
    }

    // Look for left parenthesis
    if let Some(token) = tokens.get(cursor) {
        if token.token != Token::LeftParenthesis {
            parse_err!(tokens, cursor, "Expected Left parenthesis");
        }
        cursor += 1;
    }

    // Look for expression list
    let (values, new_cursor) = parse_expressions(tokens, cursor, &[Token::RightParenthesis])?;

    cursor = new_cursor;

    // Look for right parenthesis
    if let Some(TokenContainer {
        token: Token::RightParenthesis,
        loc: _,
    }) = tokens.get(cursor)
    {
        cursor += 1;
    } else {
        parse_err!(tokens, cursor, "Expected Right Parenthesis");
    }

    Ok((
        InsertStatement {
            table: table_name.to_string(),
            values,
        },
        cursor,
    ))
}
