use super::*;

pub fn parse_create_table_statement<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    _: Token,
) -> Result<(CreateTableStatement, usize), ParsingError> {
    let mut cursor = initial_cursor;

    if !expect_token(tokens, &mut cursor, &Token::Create) {
        ret_parse_err!(tokens, cursor, "Not a Create Table Statement");
    }

    if !expect_token(tokens, &mut cursor, &Token::Table) {
        ret_parse_err!(tokens, cursor, "Expected TABLE Keyword");
    }

    let table_name = expect_identifier(tokens, &mut cursor).ok_or(parse_err!(
        tokens,
        cursor,
        "Expected Table Name"
    ))?;

    if !expect_token(tokens, &mut cursor, &Token::LeftParenthesis) {
        ret_parse_err!(tokens, cursor, "Expected Left Parenthesis");
    }

    let (cols, new_cursor) = parse_column_definitions(tokens, cursor, Token::RightParenthesis)?;
    cursor = new_cursor;

    Ok((
        CreateTableStatement {
            name: table_name.to_string(),
            cols,
        },
        cursor,
    ))
}
