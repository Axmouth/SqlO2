use super::*;

pub fn parse_drop_table_statement<'a>(
    tokens: &'a [TokenContainer],
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
        ret_parse_err!(tokens, cursor, "Not a Drop Table Statement");
    }
    if let Some(TokenContainer {
        loc: _,
        token: Token::Table,
    }) = tokens.get(cursor)
    {
        cursor += 1;
    } else {
        ret_parse_err!(tokens, cursor, "Not a Drop Table Statement");
    }
    let name = if let Some(TokenContainer {
        loc: _,
        token: Token::IdentifierValue { value },
    }) = tokens.get(cursor)
    {
        cursor += 1;
        value
    } else {
        ret_parse_err!(tokens, cursor, "Not a Drop Table Statement");
    };

    Ok((
        DropTableStatement {
            name: name.to_string(),
        },
        cursor,
    ))
}
