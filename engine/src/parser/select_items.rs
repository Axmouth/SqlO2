use super::*;

pub fn parse_select_items<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    delimiters: &[Token],
) -> Result<(Vec<SelectItem>, usize), ParsingError> {
    let mut cursor = initial_cursor;

    let mut select_items = Vec::with_capacity(5);
    let mut item_delims = delimiters.to_vec();
    item_delims.push(Token::As);
    let mut delimiters_plus = delimiters.to_vec();
    delimiters_plus.push(Token::Comma);
    delimiters_plus.push(Token::As);

    'outer: loop {
        match cursor.cmp(&tokens.len()) {
            Ordering::Equal => {
                return Ok((select_items, cursor - 1));
            }
            Ordering::Greater => {
                ret_parse_err!(tokens, cursor, "Unexpected end of tokens");
            }
            _ => {}
        }
        let current_token = &tokens[cursor];
        if delimiters.contains(&current_token.token) {
            break 'outer;
        }

        if !select_items.is_empty() && !expect_token(tokens, &mut cursor, &Token::Comma) {
            ret_parse_err!(tokens, cursor, "Expected comma1");
        }

        let mut select_item = SelectItem {
            expression: Expression::new(),
            as_clause: None,
            asterisk: false,
        };

        if expect_token(tokens, &mut cursor, &Token::Asterisk) {
            select_item.asterisk = true;
        } else {
            let (expression, new_cursor) =
                parse_expression(tokens, cursor, &delimiters_plus, 0, true, true)?;
            cursor = new_cursor;
            select_item.expression = expression;

            let mut found_as = false;
            if expect_token(tokens, &mut cursor, &Token::As) {
                found_as = true;
            }

            if let Some(value) = expect_identifier(tokens, &mut cursor) {
                select_item.as_clause = Some(value.to_string());
            } else if found_as {
                ret_parse_err!(tokens, cursor, "Expected Identifier after AS");
            }
        }

        select_items.push(select_item);
    }

    Ok((select_items, cursor))
}
