#[cfg(test)]
mod lexer_tests {
    use super::super::super::lib::lexer::*;
    use super::super::super::lib::parser::*;

    struct ParseTest {
        ast: Ast,
        input: &'static str,
    }

    #[test]
    fn test_parse() {
        let parse_tests = vec![
            ParseTest {
                input: "INSERT INTO users VALUES (105, 'George');",
                ast: Ast {
                    statements: vec![Statement::InsertStatement(InsertStatement {
                        table: Token {
                            loc: Location { col: 12, line: 0 },
                            kind: TokenKind::IdentifierKind,
                            value: "users".to_owned(),
                        },
                        values: vec![
                            Expression {
                                kind: ExpressionKind::LiteralKind,
                                literal: Token {
                                    loc: Location { col: 26, line: 0 },
                                    kind: TokenKind::NumericKind,
                                    value: "105".to_owned(),
                                },
                            },
                            Expression {
                                kind: ExpressionKind::LiteralKind,
                                literal: Token {
                                    loc: Location { col: 32, line: 0 },
                                    kind: TokenKind::StringKind,
                                    value: "George".to_owned(),
                                },
                            },
                        ],
                    })],
                },
            },
            ParseTest {
                input: "CREATE TABLE users (id INT, name TEXT);",
                ast: Ast {
                    statements: vec![Statement::CreateTableStatement(CreateTableStatement {
                        name: Token {
                            loc: Location { col: 13, line: 0 },
                            kind: TokenKind::IdentifierKind,
                            value: "users".to_owned(),
                        },
                        cols: vec![
                            ColumnDefinition {
                                name: Token {
                                    loc: Location { col: 20, line: 0 },
                                    kind: TokenKind::IdentifierKind,
                                    value: "id".to_owned(),
                                },
                                data_type: Token {
                                    loc: Location { col: 23, line: 0 },
                                    kind: TokenKind::KeywordKind,
                                    value: INT_KEYWORD.to_owned(),
                                },
                            },
                            ColumnDefinition {
                                name: Token {
                                    loc: Location { col: 28, line: 0 },
                                    kind: TokenKind::IdentifierKind,
                                    value: "name".to_owned(),
                                },
                                data_type: Token {
                                    loc: Location { col: 33, line: 0 },
                                    kind: TokenKind::KeywordKind,
                                    value: TEXT_KEYWORD.to_owned(),
                                },
                            },
                        ],
                    })],
                },
            },
        ];

        let mut found_faults = false;
        let mut err_msg = "\n".to_owned();

        for test in parse_tests {
            print!("(Parser) Testing: {}", test.input);

            let ast_result = parse(test.input);

            if ast_result.is_err() {
                found_faults = true;
                err_msg.push_str(ast_result.err().unwrap().as_str());
                continue;
            }

            let ast = ast_result.ok().unwrap();

            assert_eq!(ast, test.ast);
        }

        if found_faults {
            panic!(err_msg);
        }
    }
}
