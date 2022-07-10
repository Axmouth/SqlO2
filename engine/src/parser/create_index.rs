use super::*;

pub fn parse_create_index_statement<'a>(
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
        ret_parse_err!(tokens, cursor, "Not a Create Index Statement");
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
        ret_parse_err!(tokens, cursor, "Not a Create Index Statement");
    }
    let name = if let Some(TokenContainer {
        loc: _,
        token: Token::IdentifierValue { value },
    }) = tokens.get(cursor)
    {
        cursor += 1;
        value
    } else {
        ret_parse_err!(tokens, cursor, "Expected Index Name");
    };
    if let Some(TokenContainer {
        loc: _,
        token: Token::On,
    }) = tokens.get(cursor)
    {
        cursor += 1;
    } else {
        ret_parse_err!(tokens, cursor, "Expected ON Keyword");
    }
    let table = if let Some(TokenContainer {
        loc: _,
        token: Token::IdentifierValue { value },
    }) = tokens.get(cursor)
    {
        cursor += 1;
        value
    } else {
        ret_parse_err!(tokens, cursor, "Expected Table Name");
    };
    let (expression, cursor) = parse_expression(tokens, cursor, &[delimiter], 0, true, false)?;

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
