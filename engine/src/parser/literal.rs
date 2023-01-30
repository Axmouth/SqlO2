use super::*;

pub fn parse_literal_expression(
    tokens: &[TokenContainer],
    initial_cursor: usize,
) -> Result<(Expression, usize), ParsingError> {
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
                        ret_parse_err!(tokens, cursor, "Expected Identifier after dot");
                    }
                }
                Ok((
                    Expression::TableColumn(TableColumn {
                        col_name: col_name.to_string(),
                        table_name: table_name.map(|x| x.to_string()),
                    }),
                    cursor,
                ))
            }
            Token::NumericValue { value: _ }
            | Token::StringValue { value: _ }
            | Token::BoolValue { value: _ }
            | Token::Null => {
                cursor += 1;
                Ok((
                    Expression::Literal(LiteralExpression::from_token(&tok.token, cursor)?),
                    cursor,
                ))
            }
            _ => {
                ret_parse_err!(tokens, cursor, "Expected Literal")
            }
        }
    } else {
        ret_parse_err!(tokens, cursor, "Expected Literal Expression");
    }
}
