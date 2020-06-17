#[cfg(test)]
mod lexer_tests {
    use super::super::super::lib::lexer::*;

    struct LexerTest {
        expected_result: bool,
        expected_value: &'static str,
        value: &'static str,
    }

    fn run_lexer_tests(lexer: Lexer, tests: Vec<LexerTest>, lexer_name: &str) {
        let mut found_faults = false;
        let mut error_msg: String = "\n".to_owned();
        for test in tests {
            let lex_result = lexer(
                test.value,
                Cursor {
                    pointer: 0,
                    loc: Location { col: 0, line: 0 },
                },
            );
            let produced_result;
            if lex_result.is_some() {
                let (res, _cur) = lex_result.unwrap();
                produced_result = true;
                if res.value != test.expected_value {
                    found_faults = true;
                    error_msg.push_str(
                        format!(
                            "({}): Expected to find value:\n`{}`\n\
for following value:\n`{}`\nbut got:\n`{}`\n instead\n\n",
                            lexer_name, test.expected_value, test.value, res.value
                        )
                        .as_str(),
                    );
                }
            } else {
                produced_result = false;
            }

            if produced_result != test.expected_result {
                found_faults = true;
                if test.expected_result {
                    error_msg.push_str(
                        format!(
                            "({}): Expected to find a result for following value,\
 but it didn't:\n`{}`\n\n",
                            lexer_name, test.value
                        )
                        .as_str(),
                    );
                } else {
                    error_msg.push_str(
                        format!(
                            "({}): Expected to fail finding a result for following value,\
 but it returned one:\n`{}`\n\n",
                            lexer_name, test.value
                        )
                        .as_str(),
                    );
                }
            }
        }

        if found_faults {
            panic!(error_msg);
        }
    }

    #[test]
    fn test_token_lex_numeric() {
        let numeric_tests = vec![
            // true
            LexerTest {
                expected_result: true,
                value: "105",
                expected_value: "105",
            },
            LexerTest {
                expected_result: true,
                value: "105 ",
                expected_value: "105",
            },
            LexerTest {
                expected_result: true,
                value: "123.",
                expected_value: "123.",
            },
            LexerTest {
                expected_result: true,
                value: "123.145",
                expected_value: "123.145",
            },
            LexerTest {
                expected_result: true,
                value: "1e5",
                expected_value: "1e5",
            },
            LexerTest {
                expected_result: true,
                value: "1.e21",
                expected_value: "1.e21",
            },
            LexerTest {
                expected_result: true,
                value: "1.1e2",
                expected_value: "1.1e2",
            },
            LexerTest {
                expected_result: true,
                value: "1.1e-2",
                expected_value: "1.1e-2",
            },
            LexerTest {
                expected_result: true,
                value: "1.1e+2",
                expected_value: "1.1e+2",
            },
            LexerTest {
                expected_result: true,
                value: "1e-1",
                expected_value: "1e-1",
            },
            LexerTest {
                expected_result: true,
                value: ".1",
                expected_value: ".1",
            },
            LexerTest {
                expected_result: true,
                value: "4.",
                expected_value: "4.",
            },
            // false
            LexerTest {
                expected_result: false,
                value: "e4",
                expected_value: "",
            },
            LexerTest {
                expected_result: false,
                value: "1..",
                expected_value: "",
            },
            LexerTest {
                expected_result: false,
                value: "1ee4",
                expected_value: "",
            },
            LexerTest {
                expected_result: false,
                value: " 1",
                expected_value: "",
            },
        ];

        run_lexer_tests(lex_numeric, numeric_tests, "lex_numeric");
    }

    #[test]
    fn test_token_lex_string() {
        let string_tests = vec![
            LexerTest {
                expected_result: true,
                value: "'abc'",
                expected_value: "abc",
            },
            LexerTest {
                expected_result: true,
                value: "'a'",
                expected_value: "a",
            },
            LexerTest {
                expected_result: true,
                value: "'a''b'",
                expected_value: "a'b",
            },
            LexerTest {
                expected_result: false,
                value: "a",
                expected_value: "",
            },
            LexerTest {
                expected_result: false,
                value: "",
                expected_value: "",
            },
            LexerTest {
                expected_result: false,
                value: "'",
                expected_value: "",
            },
            LexerTest {
                expected_result: false,
                value: " 'bpp'",
                expected_value: "",
            },
        ];

        run_lexer_tests(lex_string, string_tests, "lex_string");
    }

    #[test]
    fn test_token_lex_symbol() {
        let symbol_tests = vec![
            // false
            LexerTest {
                expected_result: true,
                value: "= ",
                expected_value: "=",
            },
            LexerTest {
                expected_result: true,
                value: "||",
                expected_value: "||",
            },
            LexerTest {
                expected_result: true,
                value: ",",
                expected_value: ",",
            },
            // false
            LexerTest {
                expected_result: false,
                value: "a",
                expected_value: "a",
            },
        ];

        run_lexer_tests(lex_symbol, symbol_tests, "lex_symbol");
    }

    #[test]
    fn test_token_lex_identifier() {
        let identifier_tests = vec![
            // true
            LexerTest {
                expected_result: true,
                value: "a",
                expected_value: "a",
            },
            LexerTest {
                expected_result: true,
                value: "abc",
                expected_value: "abc",
            },
            LexerTest {
                expected_result: true,
                value: "abc ",
                expected_value: "abc",
            },
            LexerTest {
                expected_result: true,
                value: "abc ",
                expected_value: "abc",
            },
            LexerTest {
                expected_result: true,
                value: "a9$",
                expected_value: "a9$",
            },
            LexerTest {
                expected_result: true,
                value: "userName",
                expected_value: "username",
            },
            LexerTest {
                expected_result: true,
                value: "\"userName\"",
                expected_value: "userName",
            },
            // false
            LexerTest {
                expected_result: false,
                value: "\"",
                expected_value: "",
            },
            LexerTest {
                expected_result: false,
                value: "_sddfdff",
                expected_value: "",
            },
            LexerTest {
                expected_result: false,
                value: "9dfdfd",
                expected_value: "",
            },
            LexerTest {
                expected_result: false,
                value: " abc",
                expected_value: "",
            },
        ];

        run_lexer_tests(lex_identifier, identifier_tests, "lex_identifier");
    }

    #[test]
    fn test_token_lex_keyword() {
        let keyword_tests = vec![
            // true
            LexerTest {
                expected_result: true,
                value: "select ",
                expected_value: "select",
            },
            LexerTest {
                expected_result: true,
                value: "from",
                expected_value: "from",
            },
            LexerTest {
                expected_result: true,
                value: "as",
                expected_value: "as",
            },
            LexerTest {
                expected_result: true,
                value: "SELECT",
                expected_value: "select",
            },
            LexerTest {
                expected_result: true,
                value: "into",
                expected_value: "into",
            },
            // false
            LexerTest {
                expected_result: false,
                value: " from",
                expected_value: "from",
            },
            LexerTest {
                expected_result: false,
                value: "fdfd",
                expected_value: "",
            },
        ];

        run_lexer_tests(lex_keyword, keyword_tests, "lex_keyword");
    }

    struct LexTest {
        valid: bool,
        input: &'static str,
        tokens: Vec<Token>,
    }

    #[test]
    fn test_lex() {
        let lex_tests = vec![
            LexTest {
                valid: true,
                input: "select a",
                tokens: vec![
                    Token {
                        loc: Location { col: 0, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: SELECT_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: Location { col: 7, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "a".to_owned(),
                    },
                ],
            }, /*
               LexTest {
                   valid: true,
                   input: "select true",
                   tokens: vec![
                       Token {
                           loc: Location { col: 0, line: 0 },
                           kind: TokenKind::KeywordKind,
                           value: select_keyword.to_owned(),
                       },
                       Token {
                           loc: Location { col: 7, line: 0 },
                           kind: TokenKind::BoolKind,
                           value: "true".to_owned(),
                       },
                   ],
               },*/
            LexTest {
                valid: true,
                input: "select 1",
                tokens: vec![
                    Token {
                        loc: Location { col: 0, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: SELECT_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: Location { col: 7, line: 0 },
                        kind: TokenKind::NumericKind,
                        value: "1".to_owned(),
                    },
                ],
            },
            LexTest {
                valid: true,
                input: "select 'foo' || 'bar';",
                tokens: vec![
                    Token {
                        loc: Location { col: 0, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: SELECT_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: Location { col: 7, line: 0 },
                        kind: TokenKind::StringKind,
                        value: "foo".to_owned(),
                    },
                    Token {
                        loc: Location { col: 13, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: CONCAT_SYMBOL.to_owned(),
                    },
                    Token {
                        loc: Location { col: 16, line: 0 },
                        kind: TokenKind::StringKind,
                        value: "bar".to_owned(),
                    },
                    Token {
                        loc: Location { col: 21, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: SEMICOLON_SYMBOL.to_owned(),
                    },
                ],
            },
            LexTest {
                valid: true,
                input: "CREATE TABLE u (id INT, name TEXT)",
                tokens: vec![
                    Token {
                        loc: Location { col: 0, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: CREATE_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: Location { col: 7, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: TABLE_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: Location { col: 13, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "u".to_owned(),
                    },
                    Token {
                        loc: Location { col: 15, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: LEFT_PARENTHESIS_SYMBOL.to_owned(),
                    },
                    Token {
                        loc: Location { col: 16, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "id".to_owned(),
                    },
                    Token {
                        loc: Location { col: 19, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: INT_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: Location { col: 22, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: COMMA_SYMBOL.to_owned(),
                    },
                    Token {
                        loc: Location { col: 24, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "name".to_owned(),
                    },
                    Token {
                        loc: Location { col: 29, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: TEXT_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: Location { col: 33, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: RIGHT_PARENTHESIS_SYMBOL.to_owned(),
                    },
                ],
            },
            LexTest {
                valid: true,
                input: "insert into users values (545, 232)",
                tokens: vec![
                    Token {
                        loc: Location { col: 0, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: INSERT_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: Location { col: 7, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: INTO_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: Location { col: 12, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "users".to_owned(),
                    },
                    Token {
                        loc: Location { col: 18, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: VALUES_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: Location { col: 25, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: LEFT_PARENTHESIS_SYMBOL.to_owned(),
                    },
                    Token {
                        loc: Location { col: 26, line: 0 },
                        kind: TokenKind::NumericKind,
                        value: "545".to_owned(),
                    },
                    Token {
                        loc: Location { col: 30, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: COMMA_SYMBOL.to_owned(),
                    },
                    Token {
                        loc: Location { col: 32, line: 0 },
                        kind: TokenKind::NumericKind,
                        value: "232".to_owned(),
                    },
                    Token {
                        loc: Location { col: 36, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: RIGHT_PARENTHESIS_SYMBOL.to_owned(),
                    },
                ],
            },
            LexTest {
                valid: true,
                input: "SELECT id FROM users;",
                tokens: vec![
                    Token {
                        loc: Location { col: 0, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: SELECT_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: Location { col: 7, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "id".to_owned(),
                    },
                    Token {
                        loc: Location { col: 10, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: FROM_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: Location { col: 15, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "users".to_owned(),
                    },
                    Token {
                        loc: Location { col: 20, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: SEMICOLON_SYMBOL.to_owned(),
                    },
                ],
            },
            LexTest {
                valid: true,
                input: "SELECT id, name FROM users;",
                tokens: vec![
                    Token {
                        loc: Location { col: 0, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: SELECT_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: Location { col: 7, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "id".to_owned(),
                    },
                    Token {
                        loc: Location { col: 9, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: COMMA_SYMBOL.to_owned(),
                    },
                    Token {
                        loc: Location { col: 11, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "name".to_owned(),
                    },
                    Token {
                        loc: Location { col: 16, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: FROM_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: Location { col: 21, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "users".to_owned(),
                    },
                    Token {
                        loc: Location { col: 26, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: SEMICOLON_SYMBOL.to_owned(),
                    },
                ],
            },
        ];

        let mut found_faults = false;
        let mut err_msg = "\n".to_owned();

        for test in lex_tests {
            let lex_result = lex(test.input);

            match lex_result {
                Ok(result) => {
                    if !test.valid {
                        found_faults = true;
                        err_msg.push_str(
                            &format!(
                                "For Query:\n`{}`\n a failure was expected, but it passed\n",
                                test.input
                            )
                            .to_owned(),
                        );
                    }
                    if result.len() != test.tokens.len() {
                        found_faults = true;
                        err_msg.push_str(
                            &format!(
                                "For Query `{}` a result with `{}`\
                            tokens was expected, but one with `{}` was received\n",
                                test.input,
                                test.tokens.len(),
                                result.len(),
                            )
                            .to_owned(),
                        );
                    } else {
                        for i in 0..test.tokens.len() {
                            let test_token = &result[i];
                            let expected_token = &test.tokens[i];

                            if test_token.kind != expected_token.kind {
                                found_faults = true;
                                err_msg.push_str(
                                &format!(
                                    "For Query `{}` the token at position {} was expected to have a kind of `{:?}`\
, but one with kind `{:?}` was received\n",
                                    test.input,
                                    i,
                                    expected_token.kind,
                                    test_token.kind
                                )
                                .to_owned(),
                            );
                            }

                            if test_token.value != expected_token.value {
                                found_faults = true;
                                err_msg.push_str(
                                &format!(
                                    "For Query `{}` the token at position {} was expected to have a value of `{}`\
, but one with value `{}` was received\n",
                                    test.input,
                                    i,
                                    expected_token.value,
                                    test_token.value
                                )
                                .to_owned(),
                            );
                            }

                            if test_token.loc != expected_token.loc {
                                found_faults = true;
                                err_msg.push_str(
                                &format!(
                                    "For Query `{}` the token at position {} was expected to have a location of `{}:{}`\
, but one with location `{}:{}` was received\n",
                                    test.input,
                                    i,
                                    expected_token.loc.col,
                                    expected_token.loc.line,
                                    test_token.loc.col,
                                    test_token.loc.line,
                                )
                                .to_owned(),
                            );
                            }
                        }
                    }
                }
                Err(err) => {
                    found_faults = true;
                    if test.valid {
                        err_msg.push_str(err.as_str());
                    }
                }
            }
        }

        if found_faults {
            panic!(err_msg);
        }
    }
}
