use super::*;

pub fn parse_insert_statement(
    tokens: &[TokenContainer],
    initial_cursor: usize,
    _: Token,
) -> Result<(InsertStatement, usize), ParsingError> {
    let mut cursor = initial_cursor;

    // Look for INSERT
    if !expect_token(tokens, &mut cursor, &Token::Insert) {
        ret_parse_err!(tokens, cursor, "Not an Insert Statement");
    }

    // Look for INTO
    if !expect_token(tokens, &mut cursor, &Token::Into) {
        ret_parse_err!(tokens, cursor, "Expected INTO");
    }

    let table_name = expect_identifier(tokens, &mut cursor).ok_or(parse_err!(
        tokens,
        cursor,
        "Expected Table Name"
    ))?;

    // Look for VALUES
    if !expect_token(tokens, &mut cursor, &Token::Values) {
        ret_parse_err!(tokens, cursor, "Expected VALUES");
    }

    // Look for left parenthesis
    if !expect_token(tokens, &mut cursor, &Token::LeftParenthesis) {
        ret_parse_err!(tokens, cursor, "Expected Left parenthesis");
    }

    // Look for expression list
    let (values, new_cursor) = parse_expressions(tokens, cursor, &[Token::RightParenthesis])?;

    cursor = new_cursor;

    // Look for right parenthesis
    if !expect_token(tokens, &mut cursor, &Token::RightParenthesis) {
        ret_parse_err!(tokens, cursor, "Expected Right Parenthesis");
    }

    Ok((
        InsertStatement {
            table: table_name.to_string(),
            values,
        },
        cursor,
    ))
}
