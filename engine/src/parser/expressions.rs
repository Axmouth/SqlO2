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
            ret_parse_err!(tokens, cursor, "Expected Expression");
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
        if !expressions.is_empty() && !expect_token(tokens, &mut cursor, &Token::Comma) {
            ret_parse_err!(tokens, cursor, "Expected Comma");
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
