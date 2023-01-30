use super::*;

pub fn parse_column_definitions<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    delimiter: Token,
) -> Result<(Vec<ColumnDefinition>, usize), ParsingError> {
    let mut cursor = initial_cursor;

    let mut column_definitions: Vec<ColumnDefinition> = Vec::with_capacity(5);

    loop {
        if cursor >= tokens.len() {
            ret_parse_err!(tokens, cursor, "Unexpected end of input");
        }

        // Look for a delimiter
        if expect_token(tokens, &mut cursor, &delimiter) {
                break;
        }

        // Look for a comma
        if !column_definitions.is_empty() && !expect_token(tokens, &mut cursor, &Token::Comma) {
            ret_parse_err!(tokens, cursor, "Expected Comma");
        }

        // Look for a column name
        let col_name = match &tokens.get(cursor) {
            Some(TokenContainer {
                loc: _,
                token: Token::IdentifierValue { value },
            }) => value,
            _ => {
                ret_parse_err!(tokens, cursor, "Expected Column Name");
            }
        };
        cursor += 1;

        // Look for a column type
        if let Some(token_c) = tokens.get(cursor) {
            if !token_c.token.is_datatype() {
                ret_parse_err!(tokens, cursor, "Expected Column Type");
            }
        }

        let mut is_primary_key = false;
        let col_type = match tokens.get(cursor) {
            Some(v) => v,
            None => {
                ret_parse_err!(tokens, cursor, "Expected Column Type");
            }
        };
        cursor += 1;

        // Look for primary key
        // TODO: expect_tokens(..)
        if let (
            Some(TokenContainer {
                loc: _,
                token: Token::Primary,
            }),
            Some(TokenContainer {
                loc: _,
                token: Token::Key,
            }),
        ) = (&tokens.get(cursor), &tokens.get(cursor + 1))
        {
            is_primary_key = true;
            cursor += 2;
        }

        column_definitions.push(ColumnDefinition {
            name: col_name.to_string(),
            data_type: SqlType::from_token(col_type, cursor)?,
            is_primary_key,
        });
    }

    Ok((column_definitions, cursor))
}
