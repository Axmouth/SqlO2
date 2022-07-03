use super::*;

pub fn parse_create_table_statement<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    _: Token,
) -> Result<(CreateTableStatement, usize), ParsingError> {
    let mut cursor = initial_cursor;

    if !expect_token(tokens, cursor, Token::Create) {
        parse_err!(tokens, cursor, "Not a Create Table Statement");
    }
    cursor += 1;

    if !expect_token(tokens, cursor, Token::Table) {
        parse_err!(tokens, cursor, "Expected TABLE Keyword");
    }
    cursor += 1;

    let name = match tokens.get(cursor) {
        Some(TokenContainer {
            loc: _,
            token: Token::IdentifierValue { value },
        }) => value,
        _ => {
            parse_err!(tokens, cursor, "Expected Table Name");
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
        parse_err!(tokens, cursor, "Expected Left Parenthesis");
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
        parse_err!(tokens, cursor, "Expected Left Parenthesis");
    }

    Ok((
        CreateTableStatement {
            name: name.to_string(),
            cols,
        },
        cursor,
    ))
}
