use super::*;

pub fn parse_expressions(
    tokens: &[TokenContainer],
    initial_cursor: usize,
    delimiters: &[Token],
) -> Result<(Vec<Expression>, usize), ParsingError> {
    let mut cursor = initial_cursor;

    let mut expressions: Vec<Expression> = Vec::with_capacity(5);

    loop {
        if cursor >= tokens.len() {
            parse_err!(tokens, cursor, "Expected Expression");
        }

        // Look for delimiter
        if let Some(TokenContainer {
            loc: _,
            token: current_token,
        }) = tokens.get(cursor)
        {
            if delimiters.contains(current_token) {
                return Ok((expressions, cursor));
            }
        }

        // Look for comma
        if !expressions.is_empty() {
            if !expect_token(tokens, cursor, Token::Comma) {
                parse_err!(tokens, cursor, "Expected Comma");
            }

            cursor += 1;
        }

        // Look for expression
        let (expression, new_cursor) = parse_expression(
            tokens,
            cursor,
            &[Token::Comma, Token::RightParenthesis],
            tokens[cursor].binding_power(),
            true,
            false,
        )?;
        cursor = new_cursor;
        expressions.push(expression);
    }
}
