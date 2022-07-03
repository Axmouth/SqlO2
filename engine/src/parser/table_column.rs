use super::*;

pub fn parse_table_column(
    tokens: &[TokenContainer],
    initial_cursor: usize,
) -> Result<(TableColumn, usize), ParsingError> {
    let mut cursor = initial_cursor;

    if let Some(tok) = tokens.get(cursor) {
        match tok.token {
            Token::IdentifierValue {
                value: ref first_identifier,
            } => {
                cursor += 1;
                let mut col_name = first_identifier;
                let mut table_name = None;
                if let Some(TokenContainer {
                    token: Token::Dot,
                    loc: _,
                }) = tokens.get(cursor)
                {
                    cursor += 1;
                    if let Some(TokenContainer {
                        token: Token::IdentifierValue { value },
                        loc: _,
                    }) = tokens.get(cursor)
                    {
                        cursor += 1;
                        table_name = Some(col_name);
                        col_name = value;
                    } else {
                        parse_err!(tokens, cursor, "Failed to parse Table name in Column");
                    }
                }
                Ok((
                    TableColumn {
                        col_name: col_name.to_string(),
                        table_name: table_name.map(|s| s.to_string()),
                    },
                    cursor,
                ))
            }
            _ => parse_err!(tokens, cursor, "Failed to parse Column"),
        }
    } else {
        parse_err!(tokens, cursor, "Failed to parse Column");
    }
}
