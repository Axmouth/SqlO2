use super::*;

pub fn parse_tables<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    delimiters: &[Token],
) -> Result<(Vec<RowDataSource>, usize), ParsingError> {
    let mut cursor = initial_cursor;

    let mut tables = vec![];

    loop {
        let (table, new_cursor) = parse_table(tokens, cursor, delimiters)?;
        cursor = new_cursor;
        tables.push(table);
        if tokens.get(cursor).is_none() {
            break;
        } else if let Some(TokenContainer {
            token: Token::Comma,
            loc: _,
        }) = tokens.get(cursor)
        {
            continue;
        } else if let Some(TokenContainer { token, loc: _ }) = tokens.get(cursor) {
            if delimiters.contains(token) {
                break;
            } else {
                ret_parse_err!(
                    tokens,
                    cursor,
                    &format!("Failed to parse table, unexpected {:?}", token)
                );
            }
        } else {
            ret_parse_err!(tokens, cursor, "Failed to parse Table");
        }
    }

    Ok((tables, cursor))
}
