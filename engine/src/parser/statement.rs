use super::*;

pub fn parse_statement<'a>(
    tokens: &'a [TokenContainer],
    initial_cursor: usize,
    delimiter: Token,
) -> Result<(Statement, usize), ParsingError> {
    let cursor = initial_cursor;

    if let Some(first_token) = tokens.get(cursor) {
        match first_token.token {
            Token::Select => {
                // Look for a SELECT statement
                match parse_select_statement(tokens, cursor, delimiter) {
                    Ok((select, new_cursor)) => {
                        Ok((Statement::SelectStatement(select), new_cursor))
                    }
                    Err(err) => Err(err),
                }
            }
            Token::Insert => {
                // Look for an INSERT statement
                match parse_insert_statement(tokens, cursor, delimiter) {
                    Ok((insert, new_cursor)) => {
                        Ok((Statement::InsertStatement(insert), new_cursor))
                    }
                    Err(err) => Err(err),
                }
            }
            Token::Delete => ret_parse_err!(tokens, cursor, Internal, "Delete not implemented"),
            Token::Update => ret_parse_err!(tokens, cursor, Internal, "Update not implemented"),
            Token::Alter => ret_parse_err!(tokens, cursor, Internal, "Alter not implemented"),
            Token::IdentifierValue { value: _ } => {
                ret_parse_err!(tokens, cursor, Internal, "Assignment not implemented")
            }
            Token::Create => {
                if let Some(first_token) = tokens.get(cursor + 1) {
                    match first_token.token {
                        Token::Index => {
                            // Look for a CREATE INDEX statement
                            match parse_create_index_statement(tokens, cursor, delimiter) {
                                Ok((create_index, new_cursor)) => {
                                    Ok((Statement::CreateIndexStatement(create_index), new_cursor))
                                }
                                Err(err) => (Err(err)),
                            }
                        }
                        Token::Unique => match tokens.get(cursor + 2) {
                            Some(TokenContainer {
                                token: Token::Index,
                                loc: _,
                            }) => {
                                // Look for a CREATE UNIQUE INDEX statement
                                match parse_create_index_statement(tokens, cursor, delimiter) {
                                    Ok((create_index, new_cursor)) => Ok((
                                        Statement::CreateIndexStatement(create_index),
                                        new_cursor,
                                    )),
                                    Err(err) => (Err(err)),
                                }
                            }
                            Some(TokenContainer {
                                token: Token::Constraint,
                                loc: _,
                            }) => {
                                ret_parse_err!(tokens, cursor, "Create Constraint not implemented")
                            }
                            _ => ret_parse_err!(tokens, cursor, "Invalid Create Statement"),
                        },
                        Token::Constraint => {
                            ret_parse_err!(tokens, cursor, "Create Constraint not implemented")
                        }
                        Token::Table => {
                            // Look for a CREATE TABLE statement
                            match parse_create_table_statement(tokens, cursor, delimiter) {
                                Ok((create_table, new_cursor)) => {
                                    Ok((Statement::CreateTableStatement(create_table), new_cursor))
                                }
                                Err(err) => (Err(err)),
                            }
                        }
                        _ => ret_parse_err!(tokens, cursor, "Invalid Create Statement"),
                    }
                } else {
                    ret_parse_err!(tokens, cursor, "Invalid Create Statement");
                }
            }
            Token::Drop => {
                // Look for an DROP statement
                match parse_drop_table_statement(tokens, cursor, delimiter) {
                    Ok((drop, new_cursor)) => Ok((Statement::DropTableStatement(drop), new_cursor)),
                    Err(err) => (Err(err)),
                }
            }
            _ => ret_parse_err!(tokens, cursor, "Expected a valid Statement"),
        }
    } else {
        ret_parse_err!(tokens, cursor, "Expected a valid Statement");
    }
}
