use super::*;

pub fn parse_joins<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    delimiters: &[Token],
) -> Result<(Vec<JoinClause>, usize), ParsingError> {
    let mut cursor = initial_cursor;

    let mut joins = vec![];

    loop {
        let mut kind = JoinKind::Inner;
        if tokens.get(cursor).is_none() {
            break;
        }

        if let Some(TokenContainer {
            token: Token::On,
            loc: _,
        }) = tokens.get(cursor)
        {
            break;
        } else if let Some(TokenContainer {
            token: Token::Inner,
            loc: _,
        }) = tokens.get(cursor)
        {
            cursor += 1;
            kind = JoinKind::Inner;
        } else if let Some(TokenContainer {
            token: Token::Right,
            loc: _,
        }) = tokens.get(cursor)
        {
            cursor += 1;
            if let Some(TokenContainer {
                token: Token::Outer,
                loc: _,
            }) = tokens.get(cursor)
            {
                cursor += 1;
            }
            kind = JoinKind::RightOuter;
        } else if let Some(TokenContainer {
            token: Token::Left,
            loc: _,
        }) = tokens.get(cursor)
        {
            cursor += 1;
            if let Some(TokenContainer {
                token: Token::Outer,
                loc: _,
            }) = tokens.get(cursor)
            {
                cursor += 1;
            }
            kind = JoinKind::LeftOuter;
        } else if let Some(TokenContainer {
            token: Token::Full,
            loc: _,
        }) = tokens.get(cursor)
        {
            cursor += 1;
            if let Some(TokenContainer {
                token: Token::Outer,
                loc: _,
            }) = tokens.get(cursor)
            {
                cursor += 1;
                kind = JoinKind::FullOuter;
            } else {
                ret_parse_err!(tokens, cursor, "Expected OUTER Keyword after FULL");
            }
        } else if let Some(TokenContainer { token, loc: _ }) = tokens.get(cursor) {
            if delimiters.contains(token) {
                break;
            }
            ret_parse_err!(tokens, cursor, "Failed to parse Join Clause");
        }
        if let Some(TokenContainer {
            token: Token::Join,
            loc: _,
        }) = tokens.get(cursor)
        {
            cursor += 1;
        } else {
            ret_parse_err!(tokens, cursor, "No JOIN Keyword after INNER");
        }
        let (table, new_cursor) = parse_table(tokens, cursor, delimiters)?;
        cursor = new_cursor;
        if let Some(TokenContainer {
            token: Token::On,
            loc: _,
        }) = tokens.get(cursor)
        {
            cursor += 1;
        } else {
            ret_parse_err!(tokens, cursor, "No ON keyword in Join Expression");
        }
        let (col1, new_cursor) = parse_table_column(tokens, cursor)?;
        cursor = new_cursor;
        let operand_token = if let Some(TokenContainer { token, loc: _ }) = tokens.get(cursor) {
            cursor += 1;
            if BINARY_OPERATORS.contains(token) {
                token.clone()
            } else {
                ret_parse_err!(tokens, cursor, "No Binary Operator in Join Expression");
            }
        } else {
            ret_parse_err!(tokens, cursor, "No Binary Operator in Join Expression");
        };
        let (col2, new_cursor) = parse_table_column(tokens, cursor)?;
        cursor = new_cursor;

        let operand = if let Some(o) = BinaryOperand::from_token(&operand_token) {
            o
        } else {
            ret_parse_err!(
                tokens,
                cursor,
                "Failed to parse Binary Operator in Join Expression"
            );
        };

        let join = JoinClause {
            kind,
            source: table,
            on: Expression::Binary(BinaryExpression {
                first: Box::new(Expression::TableColumn(col1)),
                second: Box::new(Expression::TableColumn(col2)),
                operand,
            }),
        };
        joins.push(join);
    }

    Ok((joins, cursor))
}
