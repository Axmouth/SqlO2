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
pub enum TokenKind {
    String,
    Identifier,
    Keyword,
    Symbol,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct TokenContainer {
    pub token: Token,
    pub loc: TokenLocation,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Cursor {
    pub pointer: u32,
    pub loc: TokenLocation,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Token {
    // Keywords
    As,
    From,
    Int,
    Into,
    Values,
    Insert,
    Select,
    Create,
    Where,
    Table,
    Text,
    Drop,
    Bool,
    And,
    Or,
    True,
    False,
    Unique,
    Index,
    On,
    Primary,
    Null,

    // Symbols
    Semicolon,
    Asterisk,
    Comma,
    LeftParenthesis,
    RightParenthesis,
    Equal,
    NotEqual,
    Concat,
    Plus,
    Minus,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,

    IdentifierValue { value: String },
    StringValue { value: String },
    NumericValue { value: String },
    BoolValue { value: bool },
    Empty,
}

pub fn token_is_symbol(token: Token) -> bool {
    match token {
        Token::Semicolon
        | Token::Asterisk
        | Token::Comma
        | Token::LeftParenthesis
        | Token::RightParenthesis
        | Token::Equal
        | Token::NotEqual
        | Token::Concat
        | Token::Plus
        | Token::Minus
        | Token::LessThan
        | Token::LessThanOrEqual
        | Token::GreaterThan
        | Token::GreaterThanOrEqual => {
            return true;
        }
        _ => {}
    }

    false
}

pub fn token_is_keyword(token: Token) -> bool {
    match token {
        Token::As
        | Token::From
        | Token::Int
        | Token::Into
        | Token::Values
        | Token::Insert
        | Token::Select
        | Token::Create
        | Token::Where
        | Token::Table
        | Token::Text
        | Token::Drop
        | Token::Bool
        | Token::And
        | Token::Or
        | Token::True
        | Token::False
        | Token::Unique
        | Token::Index
        | Token::On
        | Token::Primary
        | Token::Null => {
            return true;
        }
        _ => {}
    }

    false
}

pub fn token_is_datatype(token: &Token) -> bool {
    match token {
        Token::Int | Token::Text | Token::Bool => {
            return true;
        }
        _ => {}
    }

    false
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

impl TokenContainer {
    pub fn get_id_name(&self) -> Option<&String> {
        if let Token::IdentifierValue { value } = &self.token {
            return Some(&value);
        } else {
            return None;
        }
    }

    pub fn new() -> Self {
        TokenContainer {
            token: Token::Empty,
            loc: TokenLocation { line: 0, col: 0 },
        }
    }

    pub fn new_with_kind_and_value(kind: Token, _value: String) -> Self {
        TokenContainer {
            token: kind,
            loc: TokenLocation::new(),
        }
    }

    pub fn new_with_col_and_line(col: u32, line: u32) -> Self {
        TokenContainer {
            token: Token::Empty,
            loc: TokenLocation::new_with_col_and_line(col, line),
        }
    }

    pub fn new_with_all(kind: Token, _value: String, col: u32, line: u32) -> Self {
        TokenContainer {
            token: kind,
            loc: TokenLocation::new_with_col_and_line(col, line),
        }
    }

    pub fn equals(&self, other: &Self) -> bool {
        self.token == other.token
    }

    pub fn binding_power(&self) -> u32 {
        match self.token {
            Token::And => {
                return 1;
            }
            Token::Or => {
                return 1;
            }

            Token::Equal => {
                return 2;
            }
            Token::NotEqual => {
                return 2;
            }

            Token::LessThan => {
                return 3;
            }
            Token::GreaterThan => {
                return 3;
            }

            // For some reason these are grouped separately
            Token::LessThanOrEqual => {
                return 4;
            }
            Token::GreaterThanOrEqual => {
                return 4;
            }

            Token::Concat => {
                return 5;
            }
            Token::Plus => {
                return 5;
            }
            Token::Minus => {
                return 5;
            }

            _ => {
                return 0;
            }
        };
    }
}

pub type LexerFn = fn(&Lexer, &str, Cursor) -> Option<(TokenContainer, Cursor)>;

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
    pub fn lex(&self, source: &str) -> Result<Vec<TokenContainer>, String> {
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
                match lex_fn(self, source, cur.clone()) {
                    Some((token, new_cursor)) => {
                        cur = new_cursor;

                        // Omit empty tokens for valid, but empty syntax like newlines
                        if token.token != Token::Empty {
                            tokens.push(token);
                        }
                        continue 'lex;
                    }
                    None => (),
                };
            }

            let mut hint = "".to_owned();

            if tokens.len() > 0 {
                hint = "after ".to_owned();
                hint.push_str(format!("{:?}", &tokens[tokens.len() - 1].token).as_str());
            }
            let loc = get_location_from_cursor(source, cur.pointer);
            let error = format!("Unable to lex token {}, at {}:{}", hint, loc.line, loc.col);
            return Err(error);
        }

        Ok(tokens)
    }

    pub fn lex_numeric(&self, source: &str, ic: Cursor) -> Option<(TokenContainer, Cursor)> {
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

                // exp_marker must be followed by digits
                if cur.pointer == (source.len() - 1) as u32 {
                    return None;
                }

                let c_next = *match char_iter.peek() {
                    None => {
                        return None;
                    }
                    Some(value) => value,
                };
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
            TokenContainer {
                loc: ic.loc,
                token: Token::NumericValue {
                    value: source[ic.pointer as usize..cur.pointer as usize].to_owned(),
                },
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
    ) -> Option<(TokenContainer, Cursor)> {
        let mut cur = ic.clone();

        if source[cur.pointer as usize..].len() == 0 {
            return None;
        }

        let first_char = match get_chat_at(source, cur.pointer as usize) {
            None => {
                return None;
            }
            Some(value) => value,
        };

        if first_char != delimiter {
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
                match char_iter.peek() {
                    None => {
                        return Some((
                            TokenContainer {
                                loc: ic.loc,
                                token: if kind == TokenKind::String {
                                    Token::StringValue { value }
                                } else {
                                    Token::IdentifierValue { value }
                                },
                            },
                            cur,
                        ));
                    }
                    Some(char) => {
                        if *char != delimiter {
                            return Some((
                                TokenContainer {
                                    loc: ic.loc,
                                    token: if kind == TokenKind::String {
                                        Token::StringValue { value }
                                    } else {
                                        Token::IdentifierValue { value }
                                    },
                                },
                                cur,
                            ));
                        } else if *char == delimiter {
                            char_iter.next();
                        } else {
                            value.push(delimiter);
                        }
                    }
                }
            }

            value.push(c);
            cur.loc.col += 1;
            cur.pointer += 1;
        }

        None
    }

    pub fn lex_string(&self, source: &str, ic: Cursor) -> Option<(TokenContainer, Cursor)> {
        return self.lex_character_delimited(source, ic, '\'', TokenKind::String);
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

    pub fn lex_symbol(&self, source: &str, ic: Cursor) -> Option<(TokenContainer, Cursor)> {
        let c = match get_chat_at(source, ic.pointer as usize) {
            None => {
                return None;
            }
            Some(value) => value,
        };
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
                    TokenContainer {
                        token: Token::Empty,
                        loc: TokenLocation { line: 0, col: 0 },
                    },
                    cur,
                ));
            }
            _ => {}
        }

        // Use `ic`, not `cur`
        let symbol_match = self.longest_match(source, ic.clone(), &self.symbols);
        // Unknown character
        if symbol_match == "" {
            return None;
        }
        let kind;
        // != is rewritten as <>: https://www.postgresql.org/docs/9.5/functions-comparison.html
        match symbol_match.as_str() {
            COMMA_SYMBOL => kind = Token::Comma,
            EQUAL_SYMBOL => kind = Token::Equal,
            NOT_EQUAL_SYMBOL | NOT_EQUAL_SYMBOL_2 => kind = Token::NotEqual,
            ASTERISK_SYMBOL => kind = Token::Asterisk,
            LEFT_PARENTHESIS_SYMBOL => kind = Token::LeftParenthesis,
            RIGHT_PARENTHESIS_SYMBOL => kind = Token::RightParenthesis,
            LESS_THAN_SYMBOL => kind = Token::LessThan,
            LESS_THAN_OR_EQUAL_SYMBOL => kind = Token::LessThanOrEqual,
            GREATER_THAN_SYMBOL => kind = Token::GreaterThan,
            GREATER_THAN_OR_EQUAL_SYMBOL => kind = Token::GreaterThanOrEqual,
            PLUS_SYMBOL => kind = Token::Plus,
            MINUS_SYMBOL => kind = Token::Minus,
            SEMICOLON_SYMBOL => kind = Token::Semicolon,
            CONCAT_SYMBOL => kind = Token::Concat,
            _ => {
                return None;
            }
        }

        cur.pointer = ic.pointer + symbol_match.len() as u32;
        cur.loc.col = ic.loc.col + symbol_match.len() as u32;

        Some((
            TokenContainer {
                loc: ic.loc,
                token: kind,
            },
            cur,
        ))
    }

    pub fn lex_keyword(&self, source: &str, ic: Cursor) -> Option<(TokenContainer, Cursor)> {
        let mut cur = ic.clone();

        let keyword_match = self.longest_match(source, ic.clone(), &self.keywords);
        if keyword_match == "" {
            return None;
        }
        cur.pointer = ic.pointer + keyword_match.len() as u32;
        cur.loc.col = ic.loc.col + keyword_match.len() as u32;

        let mut kind;
        match keyword_match.as_str() {
            SELECT_KEYWORD => kind = Token::Select,
            FROM_KEYWORD => kind = Token::From,
            WHERE_KEYWORD => kind = Token::Where,
            AND_KEYWORD => kind = Token::And,
            OR_KEYWORD => kind = Token::Or,
            AS_KEYWORD => kind = Token::As,
            TRUE_KEYWORD => kind = Token::True,
            FALSE_KEYWORD => kind = Token::False,
            NULL_KEYWORD => kind = Token::Null,
            ON_KEYWORD => kind = Token::On,
            TEXT_KEYWORD => kind = Token::Text,
            INT_KEYWORD => kind = Token::Int,
            BOOL_KEYWORD => kind = Token::Bool,
            INSERT_KEYWORD => kind = Token::Insert,
            VALUES_KEYWORD => kind = Token::Values,
            PRIMARY_KEYWORD => kind = Token::Primary,
            UNIQUE_KEYWORD => kind = Token::Unique,
            INDEX_KEYWORD => kind = Token::Index,
            INTO_KEYWORD => kind = Token::Into,
            CREATE_KEYWORD => kind = Token::Create,
            TABLE_KEYWORD => kind = Token::Table,
            DROP_KEYWORD => kind = Token::Drop,
            _ => {
                return None;
            }
        }

        if keyword_match == TRUE_KEYWORD.to_owned() || keyword_match == FALSE_KEYWORD.to_owned() {
            kind = Token::BoolValue {
                value: if keyword_match == TRUE_KEYWORD.to_owned() {
                    true
                } else {
                    false
                },
            };
        }

        if keyword_match == NULL_KEYWORD.to_owned() {
            kind = Token::Null;
        }

        Some((
            TokenContainer {
                loc: ic.loc,
                token: kind,
            },
            cur,
        ))
    }

    pub fn lex_identifier(&self, source: &str, ic: Cursor) -> Option<(TokenContainer, Cursor)> {
        // Handle separately if is a double-quoted identifier
        let token_result =
            self.lex_character_delimited(source, ic.clone(), '"', TokenKind::Identifier);
        if token_result.is_some() {
            return token_result;
        }

        let mut cur = ic.clone();
        let c = match get_chat_at(source, ic.pointer as usize) {
            None => {
                return None;
            }
            Some(value) => value,
        };

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
            TokenContainer {
                // Unquoted identifiers are case-insensitive
                loc: ic.loc,
                token: Token::IdentifierValue {
                    value: value.to_lowercase(),
                },
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

fn get_chat_at(source: &str, position: usize) -> Option<char> {
    source[position..(position + 1)].chars().nth(0)
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
        expected_value: Token,
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
            match lex_result {
                Some((res, _cur)) => {
                    produced_result = true;
                    if res.token != test.expected_value {
                        found_faults = true;
                        error_msg.push_str(
                            format!(
                                "({}): Expected to find value `{:?}`\
    for following value `{}` but got `{:?}` instead\n\n",
                                lexer_name, test.expected_value, test.value, res.token
                            )
                            .as_str(),
                        );
                    }
                }
                None => {
                    produced_result = false;
                }
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
                expected_value: Token::NumericValue {
                    value: "105".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "105 ",
                expected_value: Token::NumericValue {
                    value: "105".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "123.",
                expected_value: Token::NumericValue {
                    value: "123.".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "123.145",
                expected_value: Token::NumericValue {
                    value: "123.145".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "1e5",
                expected_value: Token::NumericValue {
                    value: "1e5".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "1.e21",
                expected_value: Token::NumericValue {
                    value: "1.e21".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "1.1e2",
                expected_value: Token::NumericValue {
                    value: "1.1e2".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "1.1e-2",
                expected_value: Token::NumericValue {
                    value: "1.1e-2".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "1.1e+2",
                expected_value: Token::NumericValue {
                    value: "1.1e+2".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "1e-1",
                expected_value: Token::NumericValue {
                    value: "1e-1".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: ".1",
                expected_value: Token::NumericValue {
                    value: ".1".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "4.",
                expected_value: Token::NumericValue {
                    value: "4.".to_owned(),
                },
            },
            // false
            LexerTest {
                expected_result: false,
                value: "e4",
                expected_value: Token::Empty,
            },
            LexerTest {
                expected_result: false,
                value: "1..",
                expected_value: Token::Empty,
            },
            LexerTest {
                expected_result: false,
                value: "1ee4",
                expected_value: Token::Empty,
            },
            LexerTest {
                expected_result: false,
                value: " 1",
                expected_value: Token::Empty,
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
                expected_value: Token::StringValue {
                    value: "abc".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "'a'",
                expected_value: Token::StringValue {
                    value: "a".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "'a b'",
                expected_value: Token::StringValue {
                    value: "a b".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "'a b  c '",
                expected_value: Token::StringValue {
                    value: "a b  c ".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "'a b '''' c'",
                expected_value: Token::StringValue {
                    value: "a b '' c".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "'a''b'",
                expected_value: Token::StringValue {
                    value: "a'b".to_owned(),
                },
            },
            // false
            LexerTest {
                expected_result: false,
                value: "a",
                expected_value: Token::Empty,
            },
            LexerTest {
                expected_result: false,
                value: "",
                expected_value: Token::Empty,
            },
            LexerTest {
                expected_result: false,
                value: "'",
                expected_value: Token::Empty,
            },
            LexerTest {
                expected_result: false,
                value: " 'bpp'",
                expected_value: Token::Empty,
            },
        ];

        run_lexer_tests(Lexer::lex_string, string_tests, "lex_string");
    }

    #[test]
    fn test_token_lex_symbol() {
        let symbol_tests = vec![
            // true
            LexerTest {
                expected_result: true,
                value: "= ",
                expected_value: Token::Equal,
            },
            LexerTest {
                expected_result: true,
                value: "||",
                expected_value: Token::Concat,
            },
            LexerTest {
                expected_result: true,
                value: ",",
                expected_value: Token::Comma,
            },
            // false
            LexerTest {
                expected_result: false,
                value: "a",
                expected_value: Token::Empty,
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
                expected_value: Token::IdentifierValue {
                    value: "a".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "abc",
                expected_value: Token::IdentifierValue {
                    value: "abc".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "abc ",
                expected_value: Token::IdentifierValue {
                    value: "abc".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "abc ",
                expected_value: Token::IdentifierValue {
                    value: "abc".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "a9$",
                expected_value: Token::IdentifierValue {
                    value: "a9$".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "userName",
                expected_value: Token::IdentifierValue {
                    value: "username".to_owned(),
                },
            },
            LexerTest {
                expected_result: true,
                value: "\"userName\"",
                expected_value: Token::IdentifierValue {
                    value: "userName".to_owned(),
                },
            },
            // false
            LexerTest {
                expected_result: false,
                value: "\"",
                expected_value: Token::Empty,
            },
            LexerTest {
                expected_result: false,
                value: "_sddfdff",
                expected_value: Token::Empty,
            },
            LexerTest {
                expected_result: false,
                value: "9dfdfd",
                expected_value: Token::Empty,
            },
            LexerTest {
                expected_result: false,
                value: " abc",
                expected_value: Token::Empty,
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
                expected_value: Token::Select,
            },
            LexerTest {
                expected_result: true,
                value: "from",
                expected_value: Token::From,
            },
            LexerTest {
                expected_result: true,
                value: "as",
                expected_value: Token::As,
            },
            LexerTest {
                expected_result: true,
                value: "SELECT",
                expected_value: Token::Select,
            },
            LexerTest {
                expected_result: true,
                value: "into",
                expected_value: Token::Into,
            },
            // false
            LexerTest {
                expected_result: false,
                value: " from",
                expected_value: Token::Empty,
            },
            LexerTest {
                expected_result: false,
                value: "fdfd",
                expected_value: Token::Empty,
            },
        ];

        run_lexer_tests(Lexer::lex_keyword, keyword_tests, "lex_keyword");
    }

    struct LexTest {
        valid: bool,
        input: &'static str,
        tokens: Vec<TokenContainer>,
    }

    #[test]
    fn test_lex() {
        let lex_tests = vec![
            LexTest {
                valid: true,
                input: "select a",
                tokens: vec![
                    TokenContainer {
                        loc: TokenLocation { col: 0, line: 0 },
                        token: Token::Select,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 7, line: 0 },
                        token: Token::IdentifierValue {
                            value: "a".to_owned(),
                        },
                    },
                ],
            },
            LexTest {
                valid: true,
                input: "select true",
                tokens: vec![
                    TokenContainer {
                        loc: TokenLocation { col: 0, line: 0 },
                        token: Token::Select,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 7, line: 0 },
                        token: Token::BoolValue { value: true },
                    },
                ],
            },
            LexTest {
                valid: true,
                input: "select 1",
                tokens: vec![
                    TokenContainer {
                        loc: TokenLocation { col: 0, line: 0 },
                        token: Token::Select,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 7, line: 0 },
                        token: Token::NumericValue {
                            value: "1".to_owned(),
                        },
                    },
                ],
            },
            LexTest {
                valid: true,
                input: "select 'foo' || 'bar';",
                tokens: vec![
                    TokenContainer {
                        loc: TokenLocation { col: 0, line: 0 },
                        token: Token::Select,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 7, line: 0 },
                        token: Token::StringValue {
                            value: "foo".to_owned(),
                        },
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 13, line: 0 },
                        token: Token::Concat,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 16, line: 0 },
                        token: Token::StringValue {
                            value: "bar".to_owned(),
                        },
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 21, line: 0 },
                        token: Token::Semicolon,
                    },
                ],
            },
            LexTest {
                valid: true,
                input: "CREATE TABLE u (id INT, name TEXT)",
                tokens: vec![
                    TokenContainer {
                        loc: TokenLocation { col: 0, line: 0 },
                        token: Token::Create,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 7, line: 0 },
                        token: Token::Table,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 13, line: 0 },
                        token: Token::IdentifierValue {
                            value: "u".to_owned(),
                        },
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 15, line: 0 },
                        token: Token::LeftParenthesis,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 16, line: 0 },
                        token: Token::IdentifierValue {
                            value: "id".to_owned(),
                        },
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 19, line: 0 },
                        token: Token::Int,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 22, line: 0 },
                        token: Token::Comma,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 24, line: 0 },
                        token: Token::IdentifierValue {
                            value: "name".to_owned(),
                        },
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 29, line: 0 },
                        token: Token::Text,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 33, line: 0 },
                        token: Token::RightParenthesis,
                    },
                ],
            },
            LexTest {
                valid: true,
                input: "insert into users values (545, 232)",
                tokens: vec![
                    TokenContainer {
                        loc: TokenLocation { col: 0, line: 0 },
                        token: Token::Insert,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 7, line: 0 },
                        token: Token::Into,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 12, line: 0 },
                        token: Token::IdentifierValue {
                            value: "users".to_owned(),
                        },
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 18, line: 0 },
                        token: Token::Values,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 25, line: 0 },
                        token: Token::LeftParenthesis,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 26, line: 0 },
                        token: Token::NumericValue {
                            value: "545".to_owned(),
                        },
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 30, line: 0 },
                        token: Token::Comma,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 32, line: 0 },
                        token: Token::NumericValue {
                            value: "232".to_owned(),
                        },
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 36, line: 0 },
                        token: Token::RightParenthesis,
                    },
                ],
            },
            LexTest {
                valid: true,
                input: "SELECT id FROM users;",
                tokens: vec![
                    TokenContainer {
                        loc: TokenLocation { col: 0, line: 0 },
                        token: Token::Select,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 7, line: 0 },
                        token: Token::IdentifierValue {
                            value: "id".to_owned(),
                        },
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 10, line: 0 },
                        token: Token::From,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 15, line: 0 },
                        token: Token::IdentifierValue {
                            value: "users".to_owned(),
                        },
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 20, line: 0 },
                        token: Token::Semicolon,
                    },
                ],
            },
            LexTest {
                valid: true,
                input: "SELECT id, name FROM users;",
                tokens: vec![
                    TokenContainer {
                        loc: TokenLocation { col: 0, line: 0 },
                        token: Token::Select,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 7, line: 0 },
                        token: Token::IdentifierValue {
                            value: "id".to_owned(),
                        },
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 9, line: 0 },
                        token: Token::Comma,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 11, line: 0 },
                        token: Token::IdentifierValue {
                            value: "name".to_owned(),
                        },
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 16, line: 0 },
                        token: Token::From,
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 21, line: 0 },
                        token: Token::IdentifierValue {
                            value: "users".to_owned(),
                        },
                    },
                    TokenContainer {
                        loc: TokenLocation { col: 26, line: 0 },
                        token: Token::Semicolon,
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

                            if test_token.token != expected_token.token {
                                found_faults = true;
                                err_msg.push_str(
                                &format!(
                                    "For Input `{}` the token at position {} was expected to have a kind of `{:?}`\
, but one with kind `{:?}` was received\n\n",
                                    test.input,
                                    i,
                                    expected_token.token,
                                    test_token.token
                                )
                                .to_owned(),
                            );
                            }

                            if test_token.token != expected_token.token {
                                found_faults = true;
                                err_msg.push_str(
                                &format!(
                                    "For Input `{}` the token at position {} was expected to have a value of `{:?}`\
, but one with value `{:?}` was received\n\n",
                                    test.input,
                                    i,
                                    expected_token.token,
                                    test_token.token
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
