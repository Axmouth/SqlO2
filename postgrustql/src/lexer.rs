// location of the token in source code
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct TokenLocation {
    pub line: u32,
    pub col: u32,
}

impl TokenLocation {
    pub fn new() -> Self {
        TokenLocation { col: 0, line: 0 }
    }
    pub fn new_with_col_and_line(col: u32, line: u32) -> Self {
        TokenLocation { col, line }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Token {
    pub value: String,
    pub kind: TokenKind,
    pub loc: TokenLocation,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Cursor {
    pub pointer: u32,
    pub loc: TokenLocation,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum TokenKind {
    KeywordKind,
    SymbolKind,
    IdentifierKind,
    StringKind,
    NumericKind,
    BoolKind,
    NullKind,
    Empty,
}

// for storing SQL reserved keywords
pub type Keyword = &'static str;

pub const WHERE_KEYWORD: Keyword = "where";
pub const SELECT_KEYWORD: Keyword = "select";
pub const FROM_KEYWORD: Keyword = "from";
pub const AS_KEYWORD: Keyword = "as";
pub const TABLE_KEYWORD: Keyword = "table";
pub const CREATE_KEYWORD: Keyword = "create";
pub const INSERT_KEYWORD: Keyword = "insert";
pub const INTO_KEYWORD: Keyword = "into";
pub const VALUES_KEYWORD: Keyword = "values";
pub const INT_KEYWORD: Keyword = "int";
pub const TEXT_KEYWORD: Keyword = "text";
pub const DROP_KEYWORD: Keyword = "drop";
pub const BOOL_KEYWORD: Keyword = "boolean";
pub const AND_KEYWORD: Keyword = "and";
pub const OR_KEYWORD: Keyword = "or";
pub const TRUE_KEYWORD: Keyword = "true";
pub const FALSE_KEYWORD: Keyword = "false";
pub const UNIQUE_KEYWORD: Keyword = "unique";
pub const INDEX_KEYWORD: Keyword = "index";
pub const ON_KEYWORD: Keyword = "on";
pub const PRIMARY_KEYWORD: Keyword = "primary";
pub const NULL_KEYWORD: Keyword = "null";

// for storing SQL syntax
pub type Symbol = &'static str;

pub const SEMICOLON_SYMBOL: Symbol = ";";
pub const ASTERISK_SYMBOL: Symbol = "*";
pub const COMMA_SYMBOL: Symbol = ",";
pub const LEFT_PARENTHESIS_SYMBOL: Symbol = "(";
pub const RIGHT_PARENTHESIS_SYMBOL: Symbol = ")";
pub const EQUAL_SYMBOL: Symbol = "=";
pub const NOT_EQUAL_SYMBOL: Symbol = "<>";
pub const NOT_EQUAL_SYMBOL_2: Symbol = "!=";
pub const CONCAT_SYMBOL: Symbol = "||";
pub const PLUS_SYMBOL: Symbol = "+";
pub const MINUS_SYMBOL: Symbol = "-";
pub const LESS_THAN_SYMBOL: Symbol = "<";
pub const LESS_THAN_OR_EQUAL_SYMBOL: Symbol = "<=";
pub const GREATER_THAN_SYMBOL: Symbol = ">";
pub const GREATER_THAN_OR_EQUAL_SYMBOL: Symbol = ">=";

impl Token {
    pub fn new() -> Self {
        Token {
            kind: TokenKind::Empty,
            value: "".to_owned(),
            loc: TokenLocation { line: 0, col: 0 },
        }
    }

    pub fn new_with_kind_and_value(kind: TokenKind, value: String) -> Self {
        Token {
            kind,
            value,
            loc: TokenLocation::new(),
        }
    }

    pub fn new_with_col_and_line(col: u32, line: u32) -> Self {
        Token {
            kind: TokenKind::Empty,
            value: "".to_owned(),
            loc: TokenLocation::new_with_col_and_line(col, line),
        }
    }

    pub fn new_with_all(kind: TokenKind, value: String, col: u32, line: u32) -> Self {
        Token {
            kind,
            value,
            loc: TokenLocation::new_with_col_and_line(col, line),
        }
    }

    pub fn equals(&self, other: &Self) -> bool {
        self.value == other.value && self.kind == other.kind
    }

    pub fn binding_power(&self) -> u32 {
        match self.kind {
            TokenKind::KeywordKind if self.value == AND_KEYWORD => {
                return 1;
            }
            TokenKind::KeywordKind if self.value == OR_KEYWORD => {
                return 1;
            }

            TokenKind::SymbolKind if self.value == EQUAL_SYMBOL => {
                return 2;
            }
            TokenKind::SymbolKind if self.value == NOT_EQUAL_SYMBOL => {
                return 2;
            }

            TokenKind::SymbolKind if self.value == LESS_THAN_SYMBOL => {
                return 3;
            }
            TokenKind::SymbolKind if self.value == GREATER_THAN_SYMBOL => {
                return 3;
            }

            // For some reason these are grouped separately
            TokenKind::SymbolKind if self.value == LESS_THAN_OR_EQUAL_SYMBOL => {
                return 4;
            }
            TokenKind::SymbolKind if self.value == GREATER_THAN_OR_EQUAL_SYMBOL => {
                return 4;
            }

            TokenKind::SymbolKind if self.value == CONCAT_SYMBOL => {
                return 5;
            }
            TokenKind::SymbolKind if self.value == PLUS_SYMBOL => {
                return 5;
            }
            TokenKind::SymbolKind if self.value == MINUS_SYMBOL => {
                return 5;
            }

            _ => {
                return 0;
            }
        };
    }
}

pub type LexerFn = fn(&Lexer, &str, Cursor) -> Option<(Token, Cursor)>;

pub struct Lexer {
    // Syntax that should be kept
    symbols: Vec<String>,
    keywords: Vec<String>,
}

impl Lexer {
    pub fn new() -> Self {
        Lexer {
            symbols: vec![
                EQUAL_SYMBOL.to_owned(),
                NOT_EQUAL_SYMBOL.to_owned(),
                NOT_EQUAL_SYMBOL_2.to_owned(),
                LESS_THAN_SYMBOL.to_owned(),
                LESS_THAN_OR_EQUAL_SYMBOL.to_owned(),
                GREATER_THAN_SYMBOL.to_owned(),
                GREATER_THAN_OR_EQUAL_SYMBOL.to_owned(),
                CONCAT_SYMBOL.to_owned(),
                PLUS_SYMBOL.to_owned(),
                MINUS_SYMBOL.to_owned(),
                COMMA_SYMBOL.to_owned(),
                LEFT_PARENTHESIS_SYMBOL.to_owned(),
                RIGHT_PARENTHESIS_SYMBOL.to_owned(),
                SEMICOLON_SYMBOL.to_owned(),
                ASTERISK_SYMBOL.to_owned(),
            ],
            keywords: vec![
                SELECT_KEYWORD.to_owned(),
                INSERT_KEYWORD.to_owned(),
                VALUES_KEYWORD.to_owned(),
                TABLE_KEYWORD.to_owned(),
                CREATE_KEYWORD.to_owned(),
                DROP_KEYWORD.to_owned(),
                WHERE_KEYWORD.to_owned(),
                FROM_KEYWORD.to_owned(),
                INTO_KEYWORD.to_owned(),
                TEXT_KEYWORD.to_owned(),
                BOOL_KEYWORD.to_owned(),
                INT_KEYWORD.to_owned(),
                AND_KEYWORD.to_owned(),
                OR_KEYWORD.to_owned(),
                AS_KEYWORD.to_owned(),
                TRUE_KEYWORD.to_owned(),
                FALSE_KEYWORD.to_owned(),
                UNIQUE_KEYWORD.to_owned(),
                INDEX_KEYWORD.to_owned(),
                ON_KEYWORD.to_owned(),
                PRIMARY_KEYWORD.to_owned(),
                NULL_KEYWORD.to_owned(),
            ],
        }
    }

    // lex splits an input string into a list of tokens. This process
    // can be divided into following tasks:
    //
    // 1. Instantiating a cursor with pointing to the start of the string
    //
    // 2. Execute all the lexers in series.
    //
    // 3. If any of the lexer generate a token then add the token to the
    // token slice, update the cursor and restart the process from the new
    pub fn lex(&self, source: &str) -> Result<Vec<Token>, String> {
        let mut tokens = vec![];
        let mut cur: Cursor = Cursor {
            pointer: 0,
            loc: TokenLocation { line: 0, col: 0 },
        };

        'lex: while cur.pointer < source.len() as u32 {
            let lexers: &[LexerFn] = &[
                Lexer::lex_keyword,
                Lexer::lex_symbol,
                Lexer::lex_numeric,
                Lexer::lex_identifier,
                Lexer::lex_string,
            ];

            for lex_fn in lexers {
                let token_result = lex_fn(self, source, cur.clone());
                if token_result.is_some() {
                    let (token, new_cursor) = token_result.unwrap();
                    cur = new_cursor;

                    // Omit empty tokens for valid, but empty syntax like newlines
                    if token.kind != TokenKind::Empty {
                        tokens.push(token);
                    }
                    continue 'lex;
                }
            }

            let mut hint = "".to_owned();

            if tokens.len() > 0 {
                hint = "after ".to_owned();
                hint.push_str(&tokens[tokens.len() - 1].value[..]);
            }
            let loc = get_location_from_cursor(source, cur.pointer);
            let error = format!("Unable to lex token {}, at {}:{}", hint, loc.line, loc.col);
            return Err(error);
        }

        Ok(tokens)
    }

    pub fn lex_numeric(&self, source: &str, ic: Cursor) -> Option<(Token, Cursor)> {
        let mut cur = ic.clone();

        let mut period_found = false;
        let mut exp_marker_found = false;

        let mut char_iter = source[cur.pointer as usize..].chars().peekable();

        while let Some(c) = char_iter.next() {
            cur.loc.col += 1;

            let is_digit = is_char_digit(c);
            let is_period = c == '.';
            let is_exp_marker = c == 'e';

            // Must start with a digit or period
            if cur.pointer == ic.pointer {
                if !is_digit && !is_period {
                    return None;
                }

                period_found = is_period;
                cur.pointer += 1;
                continue;
            }

            if is_period {
                if period_found {
                    return None;
                }

                period_found = true;
                cur.pointer += 1;
                continue;
            }

            if is_exp_marker && char_iter.peek().is_some() {
                if exp_marker_found {
                    return None;
                }

                // No periods allowed after expMarker
                period_found = true;
                exp_marker_found = true;

                // expMarker must be followed by digits
                if cur.pointer == (source.len() - 1) as u32 {
                    return None;
                }

                let c_next = *char_iter.peek().unwrap();
                if c_next == '-' || c_next == '+' {
                    cur.pointer += 1;
                    cur.loc.col += 1;
                    char_iter.next();
                }

                cur.pointer += 1;
                continue;
            }

            if !is_digit {
                break;
            }

            cur.pointer += 1;
        }

        if cur.pointer == ic.pointer {
            return None;
        }
        Some((
            Token {
                value: source[ic.pointer as usize..cur.pointer as usize].to_owned(),
                loc: ic.loc,
                kind: TokenKind::NumericKind,
            },
            cur,
        ))
    }

    // lexCharacterDelimited looks through a source string starting at the
    // given cursor to find a start- and end- delimiter. The delimiter can
    // be escaped be preceeding the delimiter with itself.
    pub fn lex_character_delimited(
        &self,
        source: &str,
        ic: Cursor,
        delimiter: char,
        kind: TokenKind,
    ) -> Option<(Token, Cursor)> {
        let mut cur = ic.clone();

        if source[cur.pointer as usize..].len() == 0 {
            return None;
        }

        if get_chat_at(source, cur.pointer as usize) != delimiter {
            return None;
        }

        cur.loc.col += 1;
        cur.pointer += 1;

        let mut value: String = "".to_owned();

        let mut char_iter = source[cur.pointer as usize..].chars().peekable();

        while let Some(c) = char_iter.next() {
            if c == delimiter {
                cur.pointer += 1;
                cur.loc.col += 1;
                // SQL escapes are via double characters, not backslash.
                if char_iter.peek().is_none() || *char_iter.peek().unwrap() != delimiter {
                    return Some((
                        Token {
                            value,
                            loc: ic.loc,
                            kind,
                        },
                        cur,
                    ));
                } else if *char_iter.peek().unwrap() == delimiter {
                    char_iter.next();
                } else {
                    value.push(delimiter);
                }
            }

            value.push(c);
            cur.loc.col += 1;
            cur.pointer += 1;
        }

        None
    }

    pub fn lex_string(&self, source: &str, ic: Cursor) -> Option<(Token, Cursor)> {
        return self.lex_character_delimited(source, ic, '\'', TokenKind::StringKind);
    }

    // longestMatch iterates through a source string starting at the given
    // cursor to find the longest matching substring among the provided
    // options
    pub fn longest_match(&self, source: &str, ic: Cursor, options: &Vec<String>) -> String {
        let mut text_match: String = "".to_owned();
        let cur = ic.clone();

        let rest_of_text = source[cur.pointer as usize..].to_lowercase();
        for option in options {
            if option.len() > text_match.len() && rest_of_text.starts_with(option) {
                text_match = option.to_string();
            }
        }
        return text_match;
    }

    pub fn lex_symbol(&self, source: &str, ic: Cursor) -> Option<(Token, Cursor)> {
        let c = get_chat_at(source, ic.pointer as usize);
        let mut cur = ic.clone();

        // Will get overwritten later if not an ignored syntax
        cur.pointer += 1;
        cur.loc.col += 1;

        match c {
            // Syntax that should be thrown away
            '\n' => {
                cur.loc.line += 1;
                cur.loc.col = 0;
            }
            '\t' => {}
            ' ' => {
                return Some((
                    Token {
                        kind: TokenKind::Empty,
                        loc: TokenLocation { line: 0, col: 0 },
                        value: "".to_owned(),
                    },
                    cur,
                ));
            }
            _ => {}
        }

        // Use `ic`, not `cur`
        let mut symbol_match = self.longest_match(source, ic.clone(), &self.symbols);
        // Unknown character
        if symbol_match == "" {
            return None;
        }

        cur.pointer = ic.pointer + symbol_match.len() as u32;
        cur.loc.col = ic.loc.col + symbol_match.len() as u32;

        // != is rewritten as <>: https://www.postgresql.org/docs/9.5/functions-comparison.html
        if symbol_match == NOT_EQUAL_SYMBOL_2.to_owned() {
            symbol_match = NOT_EQUAL_SYMBOL.to_owned();
        }

        Some((
            Token {
                value: symbol_match,
                loc: ic.loc,
                kind: TokenKind::SymbolKind,
            },
            cur,
        ))
    }

    pub fn lex_keyword(&self, source: &str, ic: Cursor) -> Option<(Token, Cursor)> {
        let mut cur = ic.clone();

        let keyword_match = self.longest_match(source, ic.clone(), &self.keywords);
        if keyword_match == "" {
            return None;
        }
        cur.pointer = ic.pointer + keyword_match.len() as u32;
        cur.loc.col = ic.loc.col + keyword_match.len() as u32;

        let mut kind = TokenKind::KeywordKind;

        if keyword_match == TRUE_KEYWORD.to_owned() || keyword_match == FALSE_KEYWORD.to_owned() {
            kind = TokenKind::BoolKind;
        }

        if keyword_match == NULL_KEYWORD.to_owned() {
            kind = TokenKind::NullKind;
        }

        Some((
            Token {
                value: keyword_match,
                loc: ic.loc,
                kind,
            },
            cur,
        ))
    }

    pub fn lex_identifier(&self, source: &str, ic: Cursor) -> Option<(Token, Cursor)> {
        // Handle separately if is a double-quoted identifier
        let token_result =
            self.lex_character_delimited(source, ic.clone(), '"', TokenKind::IdentifierKind);
        if token_result.is_some() {
            return token_result;
        }

        let mut cur = ic.clone();
        let c = get_chat_at(source, ic.pointer as usize);

        // Other characters count too, but ignoring non-ascii for now
        if !is_char_alphabetical(c) {
            return None;
        }

        cur.pointer += 1;
        cur.loc.col += 1;

        let mut value: String = format!("{}", c);

        for c in source[cur.pointer as usize..].to_owned().chars() {
            // Other characters count too, big ignoring non-ascii for now
            if is_char_valid_for_identifier(c) {
                value.push(c);
                cur.pointer += 1;
                cur.loc.col += 1;
                continue;
            }

            break;
        }

        if value.len() == 0 {
            return None;
        }

        Some((
            Token {
                // Unquoted identifiers are case-insensitive
                value: value.to_lowercase(),
                loc: ic.loc,
                kind: TokenKind::IdentifierKind,
            },
            cur,
        ))
    }
}

pub fn get_location_from_cursor(source: &str, cursor: u32) -> TokenLocation {
    let rev_pos = source[..(cursor + 1) as usize]
        .chars()
        .rev()
        .collect::<String>()
        .find('\n');
    let mut col = cursor;
    match rev_pos {
        Some(rev_pos) => {
            col = (source[..(cursor + 1) as usize].len() - rev_pos) as u32;
        }
        _ => {}
    }
    TokenLocation {
        col,
        line: source[..(cursor + 1) as usize].matches('\n').count() as u32,
    }
}

fn get_chat_at(source: &str, position: usize) -> char {
    source[position..(position + 1)].chars().nth(0).unwrap()
}

fn is_char_alphabetical(c: char) -> bool {
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
}

fn is_char_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn is_char_valid_for_identifier(c: char) -> bool {
    is_char_alphabetical(c) || is_char_digit(c) || c == '$' || c == '_'
}

#[cfg(test)]
mod lexer_tests {
    use super::super::lexer::*;

    struct LexerTest {
        expected_result: bool,
        expected_value: &'static str,
        value: &'static str,
    }

    fn run_lexer_tests(lex_fn: LexerFn, tests: Vec<LexerTest>, lexer_name: &str) {
        let lexer = Lexer::new();
        let mut found_faults = false;
        let mut error_msg: String = "\n".to_owned();
        for test in tests {
            let lex_result = lex_fn(
                &lexer,
                test.value,
                Cursor {
                    pointer: 0,
                    loc: TokenLocation { col: 0, line: 0 },
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
                            "({}): Expected to find value `{}`\
for following value `{}` but got `{}` instead\n\n",
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
 but it didn't `{}`\n\n",
                            lexer_name, test.value
                        )
                        .as_str(),
                    );
                } else {
                    error_msg.push_str(
                        format!(
                            "({}): Expected to fail finding a result for following value,\
 but it returned one `{}`\n\n",
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

        run_lexer_tests(Lexer::lex_numeric, numeric_tests, "lex_numeric");
    }

    #[test]
    fn test_token_lex_string() {
        let string_tests = vec![
            // true
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
                value: "'a b'",
                expected_value: "a b",
            },
            LexerTest {
                expected_result: true,
                value: "'a b  c '",
                expected_value: "a b  c ",
            },
            LexerTest {
                expected_result: true,
                value: "'a b '''' c'",
                expected_value: "a b '' c",
            },
            LexerTest {
                expected_result: true,
                value: "'a''b'",
                expected_value: "a'b",
            },
            // false
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

        run_lexer_tests(Lexer::lex_string, string_tests, "lex_string");
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

        run_lexer_tests(Lexer::lex_symbol, symbol_tests, "lex_symbol");
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

        run_lexer_tests(Lexer::lex_identifier, identifier_tests, "lex_identifier");
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

        run_lexer_tests(Lexer::lex_keyword, keyword_tests, "lex_keyword");
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
                        loc: TokenLocation { col: 0, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: SELECT_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 7, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "a".to_owned(),
                    },
                ],
            },
            LexTest {
                valid: true,
                input: "select true",
                tokens: vec![
                    Token {
                        loc: TokenLocation { col: 0, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: SELECT_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 7, line: 0 },
                        kind: TokenKind::BoolKind,
                        value: "true".to_owned(),
                    },
                ],
            },
            LexTest {
                valid: true,
                input: "select 1",
                tokens: vec![
                    Token {
                        loc: TokenLocation { col: 0, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: SELECT_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 7, line: 0 },
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
                        loc: TokenLocation { col: 0, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: SELECT_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 7, line: 0 },
                        kind: TokenKind::StringKind,
                        value: "foo".to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 13, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: CONCAT_SYMBOL.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 16, line: 0 },
                        kind: TokenKind::StringKind,
                        value: "bar".to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 21, line: 0 },
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
                        loc: TokenLocation { col: 0, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: CREATE_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 7, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: TABLE_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 13, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "u".to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 15, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: LEFT_PARENTHESIS_SYMBOL.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 16, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "id".to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 19, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: INT_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 22, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: COMMA_SYMBOL.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 24, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "name".to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 29, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: TEXT_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 33, line: 0 },
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
                        loc: TokenLocation { col: 0, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: INSERT_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 7, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: INTO_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 12, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "users".to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 18, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: VALUES_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 25, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: LEFT_PARENTHESIS_SYMBOL.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 26, line: 0 },
                        kind: TokenKind::NumericKind,
                        value: "545".to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 30, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: COMMA_SYMBOL.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 32, line: 0 },
                        kind: TokenKind::NumericKind,
                        value: "232".to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 36, line: 0 },
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
                        loc: TokenLocation { col: 0, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: SELECT_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 7, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "id".to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 10, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: FROM_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 15, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "users".to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 20, line: 0 },
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
                        loc: TokenLocation { col: 0, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: SELECT_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 7, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "id".to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 9, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: COMMA_SYMBOL.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 11, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "name".to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 16, line: 0 },
                        kind: TokenKind::KeywordKind,
                        value: FROM_KEYWORD.to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 21, line: 0 },
                        kind: TokenKind::IdentifierKind,
                        value: "users".to_owned(),
                    },
                    Token {
                        loc: TokenLocation { col: 26, line: 0 },
                        kind: TokenKind::SymbolKind,
                        value: SEMICOLON_SYMBOL.to_owned(),
                    },
                ],
            },
        ];

        let mut found_faults = false;
        let mut err_msg = "\n".to_owned();

        for test in lex_tests {
            let lexer = Lexer::new();
            let lex_result = lexer.lex(test.input);

            match lex_result {
                Ok(result) => {
                    if !test.valid {
                        found_faults = true;
                        err_msg.push_str(
                            &format!(
                                "For Input `{}` a failure was expected, but it passed\n\n",
                                test.input
                            )
                            .to_owned(),
                        );
                    }
                    if result.len() != test.tokens.len() {
                        found_faults = true;
                        err_msg.push_str(
                            &format!(
                                "For Input `{}` a result with `{}` tokens was expected, but one with `{}` was received\n\n",
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
                                    "For Input `{}` the token at position {} was expected to have a kind of `{:?}`\
, but one with kind `{:?}` was received\n\n",
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
                                    "For Input `{}` the token at position {} was expected to have a value of `{}`\
, but one with value `{}` was received\n\n",
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
                                    "For Input `{}` the token at position {} was expected to have a location of `{}:{}`\
, but one with location `{}:{}` was received\n\n",
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
