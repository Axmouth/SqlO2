use super::*;

pub fn parse_expression<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    delimiters: &[Token],
    min_binding_power: u32,
    is_top_level: bool,
    takes_as_clause: bool,
) -> Result<(Expression, usize), ParsingError> {
    let mut cursor = initial_cursor;

    let mut expression;

    if let Some(TokenContainer {
        loc: _,
        token: Token::LeftParenthesis,
    }) = tokens.get(cursor)
    {
        cursor += 1;

        if let Some(TokenContainer {
            loc: _,
            token: Token::Select,
        }) = tokens.get(cursor)
        {
            let (select_statement, new_cursor) =
                parse_select_statement(tokens, cursor, Token::RightParenthesis)?;
            expression = Expression::SubSelect(Box::new(select_statement));
            cursor = new_cursor;
        } else {
            (expression, cursor) = parse_expression(
                tokens,
                cursor,
                &[Token::RightParenthesis],
                min_binding_power,
                true,
                false,
            )?;
        }

        if let Some(TokenContainer {
            loc: _,
            token: Token::RightParenthesis,
        }) = tokens.get(cursor)
        {
            cursor += 1;
        } else {
            parse_err!(tokens, cursor, "Expected closing Parenthesis");
        }
    } else if cursor < tokens.len() && UNARY_OPERATORS.contains(&tokens[cursor].token) {
        let token = &tokens[cursor];
        let operand = token.token.clone();
        cursor += 1;
        let mut nested_un_ops = vec![operand];
        let mut inner_exp;
        loop {
            if cursor < tokens.len() && UNARY_OPERATORS.contains(&tokens[cursor].token) {
                nested_un_ops.push(tokens[cursor].token.clone());
                cursor += 1;
            } else {
                break;
            }
        }
        match parse_literal_expression(tokens, cursor) {
            Ok((expression_, cursor_)) => {
                inner_exp = expression_;
                cursor = cursor_;
            }
            Err(err) => {
                if let Some(TokenContainer {
                    token: Token::LeftParenthesis,
                    loc: _,
                }) = tokens.get(cursor)
                {
                    cursor += 1;
                    let (expression_, cursor_) = parse_expression(
                        tokens,
                        cursor,
                        &[Token::RightParenthesis],
                        min_binding_power,
                        true,
                        takes_as_clause,
                    )?;
                    inner_exp = expression_;
                    cursor = cursor_;

                    if let Some(TokenContainer {
                        loc: _,
                        token: Token::RightParenthesis,
                    }) = tokens.get(cursor)
                    {
                        cursor += 1;
                    } else {
                        parse_err!(tokens, cursor, "Expected closing Parenthesis");
                    }
                } else {
                    return Err(err);
                }
            }
        }

        if let Some(operand) = nested_un_ops.pop() {
            inner_exp = Expression::Unary(UnaryExpression {
                first: Box::from(inner_exp),
                operand: Operand::from_token(&operand, cursor)?,
            });
        } else {
            parse_err!(tokens, cursor, "Expected Unary Operator");
        }
        while let Some(operand) = nested_un_ops.pop() {
            inner_exp = Expression::Unary(UnaryExpression {
                first: Box::from(inner_exp),
                operand: Operand::from_token(&operand, cursor)?,
            });
        }
        expression = inner_exp;
    } else {
        let (first_expression, new_cursor) = parse_literal_expression(tokens, cursor)?;
        expression = first_expression;
        cursor = new_cursor;
    }

    if let (
        Some(TokenContainer {
            token: token1,
            loc: _,
        }),
        Some(TokenContainer {
            token: token2,
            loc: _,
        }),
    ) = (tokens.get(cursor), tokens.get(cursor + 1))
    {
        if UNARY_POSTFIX_OPERATORS.contains(token1) && BINARY_OPERATORS.contains(token2) {
            cursor += 1;
            expression = Expression::Unary(UnaryExpression {
                first: Box::from(expression),
                operand: Operand::from_token(token1, cursor)?,
            });
        }
    }

    let mut last_cursor = cursor;
    'outer: while let Some(TokenContainer { token, loc: _ }) = tokens.get(cursor) {
        if delimiters.contains(token) {
            break 'outer;
        }
        if UNARY_POSTFIX_OPERATORS.contains(token) {
            break 'outer;
        }

        if let Some(TokenContainer {
            token: Token::IdentifierValue { value: _ },
            loc: _,
        }) = tokens.get(cursor)
        {
            if takes_as_clause {
                break;
            }
        }
        let mut operand_tok = Token::Empty;
        if BINARY_OPERATORS.contains(token) {
            operand_tok = token.clone();
            cursor += 1;
        }
        if operand_tok == Token::TypeCast {
            if let Some(TokenContainer { token: op, loc: _ }) = tokens.get(cursor) {
                if op.is_datatype() {
                    // Make sure expression is cast before applying unary operator
                    if let Expression::Unary(UnaryExpression { first, operand }) = expression {
                        expression = Expression::Unary(UnaryExpression {
                            first: Box::from(Expression::Cast {
                                data: first,
                                typ: SqlType::try_from((op, cursor))?,
                            }),
                            operand,
                        });
                    } else {
                        expression = Expression::Cast {
                            data: Box::new(expression),
                            typ: SqlType::try_from((op, cursor))?,
                        };
                    }

                    cursor += 1;
                    continue;
                } else {
                    parse_err!(tokens, cursor, "Expected Type Name after Type Cast");
                }
            } else {
                parse_err!(tokens, cursor, "Expected Type Name after Type Cast");
            }
        }
        if operand_tok == Token::Empty {
            parse_err!(tokens, cursor, "Expected Binary Operator");
        }

        let binding_power = operand_tok.binding_power();
        if binding_power < min_binding_power {
            cursor = last_cursor;
            break;
        }

        let (mut second_expression, new_cursor) = parse_expression(
            tokens,
            cursor,
            delimiters,
            binding_power,
            false,
            takes_as_clause,
        )?;
        let operand = Operand::from_token(&operand_tok, cursor)?;
        cursor = new_cursor;

        if let Some(TokenContainer { token, loc: _ }) = tokens.get(cursor) {
            if UNARY_POSTFIX_OPERATORS.contains(token) {
                cursor += 1;
                second_expression = Expression::Unary(UnaryExpression {
                    first: Box::from(second_expression),
                    operand: Operand::from_token(token, cursor)?,
                });
            }
        }

        expression = Expression::Binary(BinaryExpression {
            first: Box::from(expression),
            second: Box::from(second_expression),
            operand,
        });
        last_cursor = cursor;
    }

    if let Some(TokenContainer { token, loc: _ }) = tokens.get(cursor) {
        if UNARY_POSTFIX_OPERATORS.contains(token) && is_top_level {
            cursor += 1;
            expression = Expression::Unary(UnaryExpression {
                first: Box::from(expression),
                operand: Operand::from_token(token, cursor)?,
            });
        }
    }

    Ok((expression, cursor))
}
