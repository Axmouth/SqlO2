#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Location {
    pub line: u32,
    pub col: u32,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Token {
    pub value: String,
    pub kind: TokenKind,
    pub loc: Location,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Cursor {
    pub pointer: u32,
    pub loc: Location,
}

pub type Keyword = &'static str;
pub type Symbol = &'static str;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum TokenKind {
    KeywordKind,
    SymbolKind,
    IdentifierKind,
    StringKind,
    NumericKind,
    BoolKind,
    None,
}

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

pub const SEMICOLON_SYMBOL: Symbol = ";";
pub const ASTERISK_SYMBOL: Symbol = "*";
pub const COMMA_SYMBOL: Symbol = ",";
pub const LEFT_PARENTHESIS_SYMBOL: Symbol = "(";
pub const RIGHT_PARENTHESIS_SYMBOL: Symbol = ")";
pub const CONCAT_SYMBOL: Symbol = "||";
pub const EQUALS_SYMBOL: Symbol = "=";

pub type Lexer = fn(&str, Cursor) -> Option<(Token, Cursor)>;

pub fn lex(source: &str) -> Result<Vec<Token>, String> {
    let mut tokens = vec![];
    let mut cur: Cursor = Cursor {
        pointer: 0,
        loc: Location { line: 0, col: 0 },
    };

    'lex: while cur.pointer < source.len() as u32 {
        let lexers: &[Lexer] = &[
            lex_keyword,
            lex_symbol,
            lex_string,
            lex_numeric,
            lex_identifier,
        ];

        for lexer in lexers {
            let token_result = lexer(source, cur.clone());
            if token_result.is_some() {
                let (token, new_cursor) = token_result.unwrap();
                cur = new_cursor.clone();
                if token.kind != TokenKind::None {
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

        let error = format!(
            "Unable to lex token {}, at {}:{}",
            hint, cur.loc.line, cur.loc.col
        );
        return Err(error);
    }

    Ok(tokens)
}

pub fn lex_numeric(source: &str, ic: Cursor) -> Option<(Token, Cursor)> {
    let mut cur = ic.clone();

    let mut period_found = false;
    let mut exp_marker_found = false;

    while cur.pointer < source.len() as u32 {
        let c = get_chat_at(source, cur.pointer as usize);
        cur.loc.col += 1;

        let is_digit = c >= '0' && c <= '9';
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

        if is_exp_marker {
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

            let c_next = get_chat_at(source, (cur.pointer + 1) as usize);
            if c_next == '-' || c_next == '+' {
                cur.pointer += 1;
                cur.loc.col += 1;
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

pub fn lex_character_delimited(
    source: &str,
    ic: Cursor,
    delimiter: char,
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

    while cur.pointer < source.len() as u32 {
        let c = get_chat_at(source, cur.pointer as usize);

        if c == delimiter {
            // SQL escapes are via double characters, not backslash.
            if cur.pointer + 1 >= source.len() as u32
                || get_chat_at(source, (cur.pointer + 1) as usize) != delimiter
            {
                cur.pointer += 1;
                cur.loc.col += 1;
                return Some((
                    Token {
                        value,
                        loc: ic.loc,
                        kind: TokenKind::StringKind,
                    },
                    cur.clone(),
                ));
            } else if get_chat_at(source, (cur.pointer + 1) as usize) == delimiter {
                cur.pointer += 1;
                cur.loc.col += 1;
            } else {
                value.push(delimiter);
                cur.pointer += 1;
                cur.loc.col += 1;
            }
        }

        value.push(c);
        cur.loc.col += 1;
        cur.pointer += 1;
    }

    None
}

pub fn lex_string(source: &str, ic: Cursor) -> Option<(Token, Cursor)> {
    return lex_character_delimited(source, ic, '\'');
}

// longestMatch iterates through a source string starting at the given
// cursor to find the longest matching substring among the provided
// options
pub fn longest_match(source: &str, ic: Cursor, options: &Vec<String>) -> String {
    let mut value: String = "".to_owned();
    let mut skip_list: Vec<u32> = vec![];
    let mut text_match: String = "".to_owned();

    let mut cur = ic.clone();

    while cur.pointer < source.len() as u32 {
        value.push(get_chat_at(
            source.to_lowercase().as_str(),
            cur.pointer as usize,
        ));
        cur.pointer += 1;

        'match_loop: for i in 0..options.len() {
            let option = options[i].clone();

            for skip in skip_list.iter() {
                if i as u32 == *skip {
                    continue 'match_loop;
                }
            }

            // Deal with cases like INT vs INTO
            if option == value.to_owned() {
                skip_list.push(i as u32);

                if option.len() > text_match.len() {
                    text_match = option;
                }

                continue;
            }

            let shares_prefix = value == option[..(cur.pointer - ic.pointer) as usize];
            let too_long = value.len() > option.len();
            if too_long || !shares_prefix {
                skip_list.push(i as u32);
            }
        }

        if skip_list.len() == options.len() {
            break;
        }
    }
    text_match
}

pub fn lex_symbol(source: &str, ic: Cursor) -> Option<(Token, Cursor)> {
    let c = get_chat_at(source, ic.pointer as usize);
    let mut cur = ic.clone();

    // Will get overwritten later if not an ignored syntax
    cur.pointer += 1;
    cur.loc.col += 1;

    match c {
        '\n' => {
            cur.loc.line += 1;
            cur.loc.col = 0;
        }
        '\t' => {}
        ' ' => {
            return Some((
                Token {
                    kind: TokenKind::None,
                    loc: Location { line: 0, col: 0 },
                    value: "".to_owned(),
                },
                cur,
            ));
        }
        _ => {}
    }

    let symbols = &vec![
        COMMA_SYMBOL.to_owned(),
        LEFT_PARENTHESIS_SYMBOL.to_owned(),
        RIGHT_PARENTHESIS_SYMBOL.to_owned(),
        SEMICOLON_SYMBOL.to_owned(),
        ASTERISK_SYMBOL.to_owned(),
        CONCAT_SYMBOL.to_owned(),
        EQUALS_SYMBOL.to_owned(),
    ];
    // Use `ic`, not `cur`
    let string_match = longest_match(source, ic.clone(), symbols);
    // Unknown character
    if string_match == "" {
        return None;
    }
    cur.pointer = ic.pointer + string_match.len() as u32;
    cur.loc.col = ic.loc.col + string_match.len() as u32;

    Some((
        Token {
            value: string_match,
            loc: ic.loc,
            kind: TokenKind::SymbolKind,
        },
        cur,
    ))
}

pub fn lex_keyword(source: &str, ic: Cursor) -> Option<(Token, Cursor)> {
    let mut cur = ic.clone();
    let keywords = &vec![
        SELECT_KEYWORD.to_owned(),
        INSERT_KEYWORD.to_owned(),
        VALUES_KEYWORD.to_owned(),
        TABLE_KEYWORD.to_owned(),
        CREATE_KEYWORD.to_owned(),
        WHERE_KEYWORD.to_owned(),
        FROM_KEYWORD.to_owned(),
        INTO_KEYWORD.to_owned(),
        TEXT_KEYWORD.to_owned(),
        INT_KEYWORD.to_owned(),
        AS_KEYWORD.to_owned(),
    ];

    let keyword_match = longest_match(source, ic.clone(), keywords);
    if keyword_match == "" {
        return None;
    }
    cur.pointer = ic.pointer + keyword_match.len() as u32;
    cur.loc.col = ic.loc.col + keyword_match.len() as u32;

    Some((
        Token {
            value: keyword_match,
            loc: ic.loc,
            kind: TokenKind::KeywordKind,
        },
        cur,
    ))
}

pub fn lex_identifier(source: &str, ic: Cursor) -> Option<(Token, Cursor)> {
    // Handle separately if is a double-quoted identifier
    let token_result = lex_character_delimited(source, ic.clone(), '"');
    if token_result.is_some() {
        return token_result;
    }

    let mut cur = ic.clone();
    let mut c = get_chat_at(source, ic.pointer as usize);

    // Other characters count too, but ignoring non-ascii for now
    let is_alphabetical = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
    if !is_alphabetical {
        return None;
    }

    cur.pointer += 1;
    cur.loc.col += 1;

    let mut value: String = format!("{}", c);

    while cur.pointer < source.len() as u32 {
        c = get_chat_at(source, cur.pointer as usize);

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
            value: value.to_lowercase(),
            loc: ic.loc,
            kind: TokenKind::IdentifierKind,
        },
        cur,
    ))
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
