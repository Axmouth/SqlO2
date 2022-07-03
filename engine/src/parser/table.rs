use super::*;

pub fn parse_table<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    delimiters: &[Token],
) -> Result<(RowDataSource, usize), ParsingError> {
    let mut cursor = initial_cursor;

    if let Some(TokenContainer {
        token: Token::IdentifierValue { value },
        loc: _,
    }) = tokens.get(cursor)
    {
        cursor += 1;
        let mut as_clause = None;
        let table_name = value;
        let mut found_as = false;
        if let Some(TokenContainer {
            token: Token::As,
            loc: _,
        }) = tokens.get(cursor)
        {
            found_as = true;
            cursor += 1;
        }
        if let Some(TokenContainer {
            token: Token::IdentifierValue { value },
            loc: _,
        }) = tokens.get(cursor)
        {
            cursor += 1;
            as_clause = Some(value);
        } else if found_as {
            parse_err!(tokens, cursor, "Failed to parse As clause after AS");
        }
        let (joins, new_cursor) = parse_joins(tokens, cursor, delimiters)?;
        cursor = new_cursor;
        return Ok((
            RowDataSource::Table {
                table_name: table_name.to_string(),
                as_clause: as_clause.map(|s| s.to_string()),
                joins,
            },
            cursor,
        ));
    } else if let Some(TokenContainer {
        token: Token::LeftParenthesis,
        loc: _,
    }) = tokens.get(cursor)
    {
        cursor += 1;
        if let Some(TokenContainer {
            token: Token::Select,
            loc: _,
        }) = tokens.get(cursor)
        {
            let (select, new_cursor) =
                parse_select_statement(tokens, cursor, Token::RightParenthesis)?;
            cursor = new_cursor + 1;
            let mut found_as = false;
            if let Some(TokenContainer {
                token: Token::As,
                loc: _,
            }) = tokens.get(cursor)
            {
                found_as = true;
                cursor += 1;
            }
            if let Some(TokenContainer {
                token: Token::IdentifierValue { value },
                loc: _,
            }) = tokens.get(cursor)
            {
                cursor += 1;
                let as_clause = value;
                let (joins, new_cursor) = parse_joins(tokens, cursor, delimiters)?;
                cursor = new_cursor;
                return Ok((
                    RowDataSource::SubSelect {
                        select,
                        as_clause: as_clause.to_string(),
                        joins,
                    },
                    cursor,
                ));
            } else if found_as {
                parse_err!(tokens, cursor, "Failed to parse As clause after AS");
            } else if let Some(TokenContainer { token, loc: _ }) = tokens.get(cursor) {
                parse_err!(
                    tokens,
                    cursor,
                    &format!("Unexpected {:?}, subquery requires as clause", token)
                );
            }
        }
    }

    parse_err!(tokens, cursor, "Failed to parse a source Table");
}
