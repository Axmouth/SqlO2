use super::*;

pub fn parse_select_statement<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    delimiter: Token,
) -> Result<(SelectStatement, usize), ParsingError> {
    let mut cursor = initial_cursor;

    // TODO: refactor
    if let Some(TokenContainer {
        token: Token::Select,
        loc: _,
    }) = tokens.get(cursor)
    {
    } else if let Some(TokenContainer { token: _, loc: _ }) = tokens.get(cursor) {
        ret_parse_err!(tokens, cursor, "Not a Select statement");
    } else {
        ret_parse_err!(tokens, cursor, "Reached end of input");
    }
    cursor += 1;

    let mut distinct = false;
    if let Some(TokenContainer {
        token: Token::Distinct,
        loc: _,
    }) = tokens.get(cursor)
    {
        distinct = true;
        cursor += 1;
    }

    let mut select: SelectStatement = SelectStatement {
        items: Vec::with_capacity(5),
        from: vec![],
        where_clause: Expression::new(),
        is_distinct: distinct,
        order_by: None,
        limit: None,
        offset: None,
    };

    let (select_items, new_cursor) = parse_select_items(
        tokens,
        cursor,
        &[
            Token::From,
            Token::OrderBy,
            Token::Limit,
            Token::Offset,
            delimiter.clone(),
        ],
    )?;

    cursor = new_cursor;
    select.items = select_items;

    // let delimiters_plus = vec![delimiter, &where_token];

    if let Some(TokenContainer {
        token: Token::From,
        loc: _,
    }) = tokens.get(cursor)
    {
        cursor += 1;
        let (tables, new_cursor) = parse_tables(
            tokens,
            cursor,
            &[
                Token::Inner,
                Token::Left,
                Token::Right,
                Token::Join,
                Token::Where,
                Token::OrderBy,
                Token::Limit,
                Token::Offset,
                delimiter.clone(),
            ],
        )?;
        cursor = new_cursor;
        select.from = tables;
    }

    // TODO Parse join
    /*
    let (joins, new_cursor) = parse_joins(
        tokens,
        cursor,
        &[
            Token::Where,
            Token::OrderBy,
            Token::Limit,
            Token::Offset,
            delimiter.clone(),
        ],
    )?;
    cursor = new_cursor;*/

    if let Some(TokenContainer {
        loc: _,
        token: Token::Where,
    }) = tokens.get(cursor)
    {
        cursor += 1;
        let (where_clause, new_cursor) = parse_expression(
            tokens,
            cursor,
            &[
                Token::OrderBy,
                Token::Limit,
                Token::Offset,
                delimiter.clone(),
            ],
            0,
            true,
            false,
        )?;

        cursor = new_cursor;
        select.where_clause = where_clause;
    }

    if let Some(TokenContainer {
        loc: _,
        token: Token::OrderBy,
    }) = tokens.get(cursor)
    {
        cursor += 1;

        let (exp, new_cursor) = parse_expression(
            tokens,
            cursor,
            &[
                Token::Desc,
                Token::Asc,
                Token::Limit,
                Token::Offset,
                delimiter,
            ],
            0,
            true,
            true,
        )?;
        cursor = new_cursor;
        let mut order_by_clause = OrderByClause { asc: true, exp };

        if let Some(TokenContainer {
            loc: _,
            token: Token::Asc,
        }) = tokens.get(cursor)
        {
            cursor += 1;
            order_by_clause.asc = true;
        } else if let Some(TokenContainer {
            loc: _,
            token: Token::Desc,
        }) = tokens.get(cursor)
        {
            cursor += 1;
            order_by_clause.asc = false;
        }

        select.order_by = Some(order_by_clause);
    }

    if let Some(TokenContainer {
        loc: _,
        token: Token::Limit,
    }) = tokens.get(cursor)
    {
        cursor += 1;

        if let Some(TokenContainer {
            loc: _,
            token: Token::NumericValue { value },
        }) = tokens.get(cursor)
        {
            cursor += 1;
            let limit = match value.parse::<f64>() {
                Ok(val) => val,
                Err(err) => {
                    ret_parse_err!(
                        tokens,
                        cursor,
                        &format!("Failed to parse Limit value: {err}")
                    );
                }
            };
            if limit.is_sign_negative() {
                ret_parse_err!(tokens, cursor, "Limit must not be negative");
            }
            if limit.is_nan() || limit.is_infinite() {
                ret_parse_err!(
                    tokens,
                    cursor,
                    "Limit cannot be interpreted as a whole number"
                );
            }
            select.limit = Some(limit as usize);
        }
    }

    if let Some(TokenContainer {
        loc: _,
        token: Token::Offset,
    }) = tokens.get(cursor)
    {
        cursor += 1;

        if let Some(TokenContainer {
            loc: _,
            token: Token::NumericValue { value },
        }) = tokens.get(cursor)
        {
            cursor += 1;
            let offset = match value.parse::<f64>() {
                Ok(val) => val,
                Err(err) => {
                    ret_parse_err!(
                        tokens,
                        cursor,
                        &format!("Failed to parse Offset value: {err}")
                    );
                }
            };
            if offset.is_sign_negative() {
                ret_parse_err!(tokens, cursor, "Offset must not be negative");
            }
            if offset.is_nan() || offset.is_infinite() {
                ret_parse_err!(
                    tokens,
                    cursor,
                    "Limit cannot be interpreted as a whole number"
                );
            }
            select.offset = Some(offset as usize);
        }
    };

    Ok((select, cursor))
}
