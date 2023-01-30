use super::*;

// TODO: Split different paths based on expression type?
// TODO: Document better
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
            ret_parse_err!(tokens, cursor, "Expected closing Parenthesis");
        }
    } else if let (true, Some(operand)) = (
        cursor < tokens.len(),
        UnaryOperand::from_token(&tokens[cursor].token),
    ) {
        cursor += 1;
        let mut nested_un_ops = vec![operand];
        let mut inner_exp;
        loop {
            if let (true, Some(operand)) = (
                cursor < tokens.len(),
                UnaryOperand::from_token(&tokens[cursor].token),
            ) {
                nested_un_ops.push(operand);
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
                if expect_token(tokens, &mut cursor, &Token::LeftParenthesis) {
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

                    if !expect_token(tokens, &mut cursor, &Token::RightParenthesis) {
                        ret_parse_err!(tokens, cursor, "Expected closing Parenthesis");
                    }
                } else {
                    return Err(err);
                }
            }
        }

        if let Some(operand) = nested_un_ops.pop() {
            inner_exp = Expression::Unary(UnaryExpression {
                first: Box::from(inner_exp),
                operand,
            });
        } else {
            ret_parse_err!(tokens, cursor, "Expected Unary Operator");
        }
        while let Some(operand) = nested_un_ops.pop() {
            inner_exp = Expression::Unary(UnaryExpression {
                first: Box::from(inner_exp),
                operand,
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
        if let (Some(operand), true) = (
            UnaryOperand::postfix_from_token(token1),
            BINARY_OPERATORS.contains(token2),
        ) {
            cursor += 2;
            expression = Expression::Unary(UnaryExpression {
                first: Box::from(expression),
                operand,
            });
        }
    }

    let mut last_cursor = cursor;
    'outer: while let Some(TokenContainer { token, loc: _ }) = tokens.get(cursor) {
        if delimiters.contains(token) {
            break 'outer;
        }

        // Makes sure that if there are postfix unary ops, they are applied in the current expression before continuing.
        if let Some(operand) = UnaryOperand::postfix_from_token(token) {
            if !expression.is_empty() {
                expression = Expression::Unary(UnaryExpression {
                    first: Box::from(expression),
                    operand,
                });
                cursor += 1;
                last_cursor = cursor;
                continue;
            } else {
                ret_parse_err!(tokens, cursor, "Expected Expression");
            }
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
                    ret_parse_err!(tokens, cursor, "Expected Type for Cast");
                }
            } else {
                ret_parse_err!(tokens, cursor, "Unexpected end of input");
            }
        }
        if operand_tok == Token::Empty {
            ret_parse_err!(tokens, cursor, "Expected Binary Operator");
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
        let operand = BinaryOperand::from_token(&operand_tok).ok_or(ParsingError::Internal {
            msg: format!("Unrecognized token: {:?}", operand_tok),
            cursor,
        })?;
        cursor = new_cursor;

        if let Some(TokenContainer { token, loc: _ }) = tokens.get(cursor) {
            if let Some(operand) = UnaryOperand::from_token(token) {
                cursor += 1;
                second_expression = Expression::Unary(UnaryExpression {
                    first: Box::from(second_expression),
                    operand,
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
        if let (Some(operand), true) = (UnaryOperand::postfix_from_token(token), is_top_level) {
            cursor += 1;
            expression = Expression::Unary(UnaryExpression {
                first: Box::from(expression),
                operand,
            });
        }
    }

    Ok((expression, cursor))
}
