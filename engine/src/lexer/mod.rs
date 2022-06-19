use std::{borrow::Cow, iter::FromIterator};
use test_util::TestSubjectExt;

// Location of the token in source code
#[derive(Clone, Copy, Eq, PartialEq, Debug, Ord, PartialOrd, Default)]
pub struct TokenLocation {
    pub line: usize,
    pub col: usize,
}

impl TokenLocation {
    pub fn new() -> Self {
        TokenLocation { col: 0, line: 0 }
    }
    pub fn new_with_col_and_line(col: usize, line: usize) -> Self {
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
pub struct TokenContainer<'a> {
    pub token: Token<'a>,
    pub loc: TokenLocation,
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct Cursor {
    pub pointer: usize,
    pub loc: TokenLocation,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Token<'a> {
    // Keywords
    As,
    From,
    Into,
    Values,
    Insert,
    Select,
    Create,
    Where,
    Table,
    Drop,
    And,
    Or,
    Not,
    True,
    Join,
    False,
    Inner,
    Left,
    Right,
    Is,
    SmallInt,
    Int,
    BigInt,
    Real,
    Double,
    Precision,
    DoublePrecision,
    Varchar,
    Char,
    Text,
    Bool,
    Unique,
    Index,
    On,
    Primary,
    Key,
    Null,
    Alter,
    Delete,
    Update,
    Constraint,
    Foreign,
    Distinct,
    Order,
    By,
    OrderBy,
    Asc,
    Desc,
    Offset,
    Limit,
    Outer,
    Full,
    Like,

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
    Slash,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Modulo,
    Exponentiation,
    SquareRoot,
    CubeRoot,
    Factorial,
    FactorialPrefix,
    AbsoluteValue,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    BitwiseShiftLeft,
    BitwiseShiftRight,
    TypeCast,
    Dot,

    // Values
    IdentifierValue { value: Cow<'a, str> },
    StringValue { value: Cow<'a, str> },
    NumericValue { value: Cow<'a, str> },
    BoolValue { value: bool },

    // Default
    Empty,

    // Comment
    Comment,
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::And => write!(f, "{AND_KEYWORD}"),
            Token::As => write!(f, "{AS_KEYWORD}"),
            Token::Asterisk => write!(f, "{ASTERISK_SYMBOL}"),
            Token::Bool => write!(f, "{BOOL_KEYWORD}"),
            Token::BoolValue { value } => {
                if *value {
                    write!(f, "{TRUE_KEYWORD}")
                } else {
                    write!(f, "{FALSE_KEYWORD}")
                }
            }
            Token::Comma => write!(f, "{COMMA_SYMBOL}"),
            Token::Concat => write!(f, "{CONCAT_SYMBOL}"),
            Token::Create => write!(f, "{CREATE_KEYWORD}"),
            Token::Drop => write!(f, "{DROP_KEYWORD}"),
            Token::Empty => write!(f, ""),
            Token::Equal => write!(f, "{EQUAL_SYMBOL}"),
            Token::False => write!(f, "{FALSE_KEYWORD}"),
            Token::From => write!(f, "{FROM_KEYWORD}"),
            Token::GreaterThan => write!(f, "{GREATER_THAN_SYMBOL}"),
            Token::GreaterThanOrEqual => write!(f, "{GREATER_THAN_OR_EQUAL_SYMBOL}"),
            Token::IdentifierValue { value } => write!(f, "\"{value}\""),
            Token::Index => write!(f, "{INDEX_KEYWORD}"),
            Token::Insert => write!(f, "{INSERT_KEYWORD}"),
            Token::Int => write!(f, "{INT_KEYWORD}"),
            Token::Into => write!(f, "{INTO_KEYWORD}"),
            Token::Key => write!(f, "{KEY_KEYWORD}"),
            Token::LeftParenthesis => write!(f, "{LEFT_PARENTHESIS_SYMBOL}"),
            Token::LessThan => write!(f, "{LESS_THAN_SYMBOL}"),
            Token::LessThanOrEqual => write!(f, "{LESS_THAN_OR_EQUAL_SYMBOL}"),
            Token::Minus => write!(f, "{MINUS_SYMBOL}"),
            Token::NotEqual => write!(f, "{NOT_EQUAL_SYMBOL}"),
            Token::Null => write!(f, "{NULL_KEYWORD}"),
            Token::NumericValue { value } => write!(f, "{value}"),
            Token::On => write!(f, "{ON_KEYWORD}"),
            Token::Or => write!(f, "{OR_KEYWORD}"),
            Token::Not => write!(f, "{NOT_KEYWORD}"),
            Token::Plus => write!(f, "{PLUS_SYMBOL}"),
            Token::Slash => write!(f, "{SLASH_SYMBOL}"),
            Token::Modulo => write!(f, "{MODULO_SYMBOL}"),
            Token::Exponentiation => write!(f, "{EXPONENTIATION_SYMBOL}"),
            Token::SquareRoot => write!(f, "{SQUARE_ROOT_SYMBOL}"),
            Token::CubeRoot => write!(f, "{CUBE_ROOT_SYMBOL}"),
            Token::Factorial => write!(f, "{FACTORIAL_SYMBOL}"),
            Token::FactorialPrefix => write!(f, "{FACTORIAL_PREFIX_SYMBOL}"),
            Token::AbsoluteValue => write!(f, "{ABS_SYMBOL}"),
            Token::BitwiseAnd => write!(f, "{BITWISE_AND_SYMBOL}"),
            Token::BitwiseOr => write!(f, "{BITWISE_OR_SYMBOL}"),
            Token::BitwiseXor => write!(f, "{BITWISE_XOR_SYMBOL}"),
            Token::BitwiseNot => write!(f, "{BITWISE_NOT_SYMBOL}"),
            Token::BitwiseShiftLeft => write!(f, "{BITWISE_SHIFT_LEFT_SYMBOL}"),
            Token::BitwiseShiftRight => write!(f, "{BITWISE_SHIFT_RIGHT_SYMBOL}"),
            Token::Primary => write!(f, "{PRIMARY_KEYWORD}"),
            Token::RightParenthesis => write!(f, "{RIGHT_PARENTHESIS_SYMBOL}"),
            Token::Select => write!(f, "{SELECT_KEYWORD}"),
            Token::Semicolon => write!(f, "{SEMICOLON_SYMBOL}"),
            Token::StringValue { value } => write!(f, "'{value}'"),
            Token::Table => write!(f, "{TABLE_KEYWORD}"),
            Token::Text => write!(f, "{TEXT_KEYWORD}"),
            Token::True => write!(f, "{TRUE_KEYWORD}"),
            Token::Unique => write!(f, "{UNIQUE_KEYWORD}"),
            Token::Values => write!(f, "{VALUES_KEYWORD}"),
            Token::Where => write!(f, "{WHERE_KEYWORD}"),
            Token::Alter => write!(f, "{ALTER_KEYWORD}"),
            Token::Delete => write!(f, "{DELETE_KEYWORD}"),
            Token::Update => write!(f, "{UPDATE_KEYWORD}"),
            Token::Join => write!(f, "{JOIN_KEYWORD}"),
            Token::Inner => write!(f, "{INNER_KEYWORD}"),
            Token::Right => write!(f, "{RIGHT_KEYWORD}"),
            Token::Left => write!(f, "{LEFT_KEYWORD}"),
            Token::Constraint => write!(f, "{CONSTRAINT_KEYWORD}"),
            Token::Foreign => write!(f, "{FOREIGN_KEYWORD}"),
            Token::Double => write!(f, "{DOUBLE_KEYWORD}"),
            Token::DoublePrecision => write!(f, "DOUBLE PRECISION"),
            Token::Precision => write!(f, "{PRECISION_KEYWORD}"),
            Token::Real => write!(f, "{REAL_KEYWORD}"),
            Token::SmallInt => write!(f, "{SMALLINT_KEYWORD}"),
            Token::BigInt => write!(f, "{BIGINT_KEYWORD}"),
            Token::Varchar => write!(f, "{VARCHAR_KEYWORD}"),
            Token::Char => write!(f, "{CHAR_KEYWORD}"),
            Token::Is => write!(f, "{IS_KEYWORD}"),
            Token::TypeCast => write!(f, "{TYPE_CAST_SYMBOL}"),
            Token::Distinct => write!(f, "{DISTINCT_KEYWORD}"),
            Token::Order => write!(f, "{ORDER_KEYWORD}"),
            Token::By => write!(f, "{BY_KEYWORD}"),
            Token::OrderBy => write!(f, "ORDER BY"),
            Token::Asc => write!(f, "{ASC_KEYWORD}"),
            Token::Desc => write!(f, "{DESC_KEYWORD}"),
            Token::Limit => write!(f, "{LIMIT_KEYWORD}"),
            Token::Offset => write!(f, "{OFFSET_KEYWORD}"),
            Token::Dot => write!(f, "{DOT_SYMBOL}"),
            Token::Outer => write!(f, "{OUTER_KEYWORD}"),
            Token::Full => write!(f, "{FULL_KEYWORD}"),
            Token::Like => write!(f, "{LIKE_KEYWORD}"),
            Token::Comment => write!(f, ""),
        }
    }
}

impl<'a> Token<'_> {
    pub fn binding_power(&self) -> u32 {
        match self {
            Token::And => 1,
            Token::Or => 1,

            Token::Equal => 2,
            Token::NotEqual => 2,

            Token::LessThan => 3,
            Token::GreaterThan => 3,

            // For some reason these are grouped separately
            Token::LessThanOrEqual => 4,
            Token::GreaterThanOrEqual => 4,

            Token::Plus => 5,
            Token::Minus => 5,

            Token::Concat => 6,
            Token::Asterisk => 6,
            Token::Slash => 6,
            Token::Modulo => 6,
            Token::Exponentiation => 6,

            // Prefix Unary ops
            Token::SquareRoot => 7,
            Token::CubeRoot => 7,
            Token::FactorialPrefix => 7,

            // Postfix Unary ops
            Token::Factorial => 8,

            // Cast
            Token::TypeCast => 9,

            _ => 0,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
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
            | Token::Slash
            | Token::LessThan
            | Token::LessThanOrEqual
            | Token::GreaterThan
            | Token::GreaterThanOrEqual
            | Token::BitwiseAnd
            | Token::BitwiseNot
            | Token::BitwiseOr
            | Token::BitwiseXor
            | Token::AbsoluteValue
            | Token::BitwiseShiftLeft
            | Token::BitwiseShiftRight
            | Token::SquareRoot
            | Token::CubeRoot
            | Token::Factorial
            | Token::FactorialPrefix
            | Token::Exponentiation
            | Token::Modulo
            | Token::TypeCast
            | Token::Dot => {
                return true;
            }
            _ => {}
        }

        false
    }

    pub fn is_keyword(&self) -> bool {
        match self {
            Token::As
            | Token::From
            | Token::Into
            | Token::Values
            | Token::Insert
            | Token::Select
            | Token::Create
            | Token::Where
            | Token::Table
            | Token::Drop
            | Token::And
            | Token::Or
            | Token::Not
            | Token::True
            | Token::False
            | Token::Join
            | Token::Left
            | Token::Right
            | Token::Inner
            | Token::Is
            | Token::Limit
            | Token::Offset
            | Token::Int
            | Token::BigInt
            | Token::SmallInt
            | Token::Real
            | Token::DoublePrecision
            | Token::Double
            | Token::Precision
            | Token::Varchar
            | Token::Text
            | Token::Char
            | Token::Bool
            | Token::Unique
            | Token::Index
            | Token::On
            | Token::Primary
            | Token::Key
            | Token::Null
            | Token::Alter
            | Token::Delete
            | Token::Update
            | Token::Constraint
            | Token::Foreign
            | Token::Distinct
            | Token::Order
            | Token::By
            | Token::OrderBy
            | Token::Outer
            | Token::Full
            | Token::Like => {
                return true;
            }
            _ => {}
        }

        false
    }

    pub fn is_datatype(&self) -> bool {
        matches!(
            self,
            Token::Int
                | Token::BigInt
                | Token::SmallInt
                | Token::Text
                | Token::Varchar
                | Token::Real
                | Token::DoublePrecision
                | Token::Char
                | Token::Bool
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum LexingError {
    General { msg: String, loc: TokenLocation },
}

macro_rules! lex_error {
    ($msg:expr) => {
        LexingError::General {
            msg: $msg.to_string(),
            loc: TokenLocation::new(0, 0),
        }
    };
    ($msg:expr, $loc:expr) => {
        LexingError::General {
            msg: $msg.to_string(),
            loc: $loc,
        }
    };
}

impl std::fmt::Display for LexingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LexingError::General { msg, loc: _ } => msg.clone(),
            }
        )
    }
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
pub const NOT_KEYWORD: Keyword = "not";
pub const TRUE_KEYWORD: Keyword = "true";
pub const FALSE_KEYWORD: Keyword = "false";
pub const UNIQUE_KEYWORD: Keyword = "unique";
pub const INDEX_KEYWORD: Keyword = "index";
pub const ON_KEYWORD: Keyword = "on";
pub const PRIMARY_KEYWORD: Keyword = "primary";
pub const KEY_KEYWORD: Keyword = "key";
pub const NULL_KEYWORD: Keyword = "null";
pub const ALTER_KEYWORD: Keyword = "alter";
pub const DELETE_KEYWORD: Keyword = "delete";
pub const UPDATE_KEYWORD: Keyword = "update";
pub const JOIN_KEYWORD: Keyword = "join";
pub const INNER_KEYWORD: Keyword = "inner";
pub const RIGHT_KEYWORD: Keyword = "right";
pub const LEFT_KEYWORD: Keyword = "left";
pub const CONSTRAINT_KEYWORD: Keyword = "constraint";
pub const FOREIGN_KEYWORD: Keyword = "foreign";
pub const IS_KEYWORD: Keyword = "is";
pub const BIGINT_KEYWORD: Keyword = "bigint";
pub const SMALLINT_KEYWORD: Keyword = "smallint";
pub const REAL_KEYWORD: Keyword = "real";
pub const DOUBLE_KEYWORD: Keyword = "double";
pub const PRECISION_KEYWORD: Keyword = "precision";
pub const VARCHAR_KEYWORD: Keyword = "varchar";
pub const CHAR_KEYWORD: Keyword = "char";
pub const DISTINCT_KEYWORD: Keyword = "distinct";
pub const ORDER_KEYWORD: Keyword = "order";
pub const BY_KEYWORD: Keyword = "by";
pub const ASC_KEYWORD: Keyword = "asc";
pub const DESC_KEYWORD: Keyword = "desc";
pub const OFFSET_KEYWORD: Keyword = "offset";
pub const LIMIT_KEYWORD: Keyword = "limit";
pub const OUTER_KEYWORD: Keyword = "outer";
pub const FULL_KEYWORD: Keyword = "full";
pub const LIKE_KEYWORD: Keyword = "like";
// new
pub const DECIMAL_KEYWORD: Keyword = "decimal";
pub const NUMERIC_KEYWORD: Keyword = "numeric";

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
pub const SLASH_SYMBOL: Symbol = "/";
pub const LESS_THAN_SYMBOL: Symbol = "<";
pub const LESS_THAN_OR_EQUAL_SYMBOL: Symbol = "<=";
pub const GREATER_THAN_SYMBOL: Symbol = ">";
pub const GREATER_THAN_OR_EQUAL_SYMBOL: Symbol = ">=";
pub const MODULO_SYMBOL: Symbol = "%";
pub const EXPONENTIATION_SYMBOL: Symbol = "^";
pub const SQUARE_ROOT_SYMBOL: Symbol = "|/";
pub const CUBE_ROOT_SYMBOL: Symbol = "||/";
pub const FACTORIAL_SYMBOL: Symbol = "!";
pub const FACTORIAL_PREFIX_SYMBOL: Symbol = "!!";
pub const ABS_SYMBOL: Symbol = "@";
pub const BITWISE_AND_SYMBOL: Symbol = "&";
pub const BITWISE_OR_SYMBOL: Symbol = "|";
pub const BITWISE_XOR_SYMBOL: Symbol = "#";
pub const BITWISE_NOT_SYMBOL: Symbol = "~";
pub const BITWISE_SHIFT_LEFT_SYMBOL: Symbol = "<<";
pub const BITWISE_SHIFT_RIGHT_SYMBOL: Symbol = ">>";
pub const TYPE_CAST_SYMBOL: Symbol = "::";
pub const DOT_SYMBOL: Symbol = ".";

// Syntax that should be kept
static SYMBOLS: &[&str] = &[
    COMMA_SYMBOL,
    NOT_EQUAL_SYMBOL,
    NOT_EQUAL_SYMBOL_2,
    LESS_THAN_OR_EQUAL_SYMBOL,
    GREATER_THAN_OR_EQUAL_SYMBOL,
    EQUAL_SYMBOL,
    LEFT_PARENTHESIS_SYMBOL,
    RIGHT_PARENTHESIS_SYMBOL,
    SEMICOLON_SYMBOL,
    PLUS_SYMBOL,
    MINUS_SYMBOL,
    ASTERISK_SYMBOL,
    SLASH_SYMBOL,
    BITWISE_SHIFT_LEFT_SYMBOL,
    BITWISE_SHIFT_RIGHT_SYMBOL,
    LESS_THAN_SYMBOL,
    GREATER_THAN_SYMBOL,
    CONCAT_SYMBOL,
    MODULO_SYMBOL,
    EXPONENTIATION_SYMBOL,
    CUBE_ROOT_SYMBOL,
    SQUARE_ROOT_SYMBOL,
    FACTORIAL_SYMBOL,
    FACTORIAL_PREFIX_SYMBOL,
    ABS_SYMBOL,
    BITWISE_AND_SYMBOL,
    BITWISE_OR_SYMBOL,
    BITWISE_XOR_SYMBOL,
    BITWISE_NOT_SYMBOL,
    TYPE_CAST_SYMBOL,
    DOT_SYMBOL,
];

static KEYWORDS: &[&str] = &[
    SELECT_KEYWORD,
    INSERT_KEYWORD,
    VALUES_KEYWORD,
    TABLE_KEYWORD,
    CREATE_KEYWORD,
    DROP_KEYWORD,
    WHERE_KEYWORD,
    FROM_KEYWORD,
    TEXT_KEYWORD,
    BOOL_KEYWORD,
    AND_KEYWORD,
    ORDER_KEYWORD,
    OR_KEYWORD,
    DESC_KEYWORD,
    ASC_KEYWORD,
    AS_KEYWORD,
    TRUE_KEYWORD,
    FALSE_KEYWORD,
    JOIN_KEYWORD,
    INNER_KEYWORD,
    LEFT_KEYWORD,
    RIGHT_KEYWORD,
    OUTER_KEYWORD,
    FULL_KEYWORD,
    IS_KEYWORD,
    LIMIT_KEYWORD,
    OFFSET_KEYWORD,
    BY_KEYWORD,
    DISTINCT_KEYWORD,
    INTO_KEYWORD,
    INT_KEYWORD,
    BIGINT_KEYWORD,
    SMALLINT_KEYWORD,
    REAL_KEYWORD,
    DOUBLE_KEYWORD,
    PRECISION_KEYWORD,
    VARCHAR_KEYWORD,
    CHAR_KEYWORD,
    UNIQUE_KEYWORD,
    INDEX_KEYWORD,
    ON_KEYWORD,
    PRIMARY_KEYWORD,
    KEY_KEYWORD,
    NULL_KEYWORD,
    ALTER_KEYWORD,
    DELETE_KEYWORD,
    UPDATE_KEYWORD,
    CONSTRAINT_KEYWORD,
    FOREIGN_KEYWORD,
    LIKE_KEYWORD,
];

impl TokenContainer<'_> {
    pub fn equals(&self, other: &Self) -> bool {
        self.token == other.token
    }

    pub fn binding_power(&self) -> u32 {
        self.token.binding_power()
    }
}

pub type LexerFn<'a> = fn(&'a Lexer, &'a str, Cursor) -> Option<(TokenContainer<'a>, Cursor)>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Lexer {
    max_keyword_length: usize,
    max_symbol_length: usize,
}

impl TestSubjectExt for Lexer {
    fn init() -> Self {
        Self::new()
    }
}

impl Lexer {
    pub fn new() -> Self {
        let max_symbol_length =
            SYMBOLS.iter().fold(
                0,
                |acc, item| {
                    if item.len() > acc {
                        item.len()
                    } else {
                        acc
                    }
                },
            );
        let max_keyword_length =
            KEYWORDS.iter().fold(
                0,
                |acc, item| {
                    if item.len() > acc {
                        item.len()
                    } else {
                        acc
                    }
                },
            );
        Lexer {
            max_symbol_length,
            max_keyword_length,
        }
    }

    // lex splits an input string into a list of tokens. This process
    // can be divided into following tasks:
    //
    // 1. Instantiating a cursor with pointing to the start of the string
    //
    // 2. Go through all the lexers in series.
    //
    // 3. If any of the lexer generate a token then add the token to the
    // token list, update the cursor and restart the process from the new
    pub fn lex<'a>(&'a self, source: &'a str) -> Result<Vec<TokenContainer<'a>>, LexingError> {
        let mut tokens = Vec::with_capacity(30);
        let mut cur: Cursor = Cursor {
            pointer: 0,
            loc: TokenLocation { line: 0, col: 0 },
        };

        'lex: while cur.pointer < source.len() {
            if let Some((token, new_cursor)) = self.lex_keyword(source, cur) {
                cur = new_cursor;

                // Omit empty tokens for valid, but empty syntax like newlines
                if token.token != Token::Empty {
                    if token.token == Token::By {
                        if let Some(TokenContainer {
                            token: last_token,
                            loc: _,
                        }) = tokens.last_mut()
                        {
                            if last_token == &Token::Order {
                                *last_token = Token::OrderBy;
                                continue 'lex;
                            }
                        }
                    }
                    if token.token == Token::Precision {
                        if let Some(TokenContainer {
                            token: last_token,
                            loc: _,
                        }) = tokens.last_mut()
                        {
                            if last_token == &Token::Double {
                                *last_token = Token::DoublePrecision;
                                continue 'lex;
                            }
                        }
                    }

                    tokens.push(token);
                }
                continue 'lex;
            } else if let Some((_, new_cursor)) = self.lex_comment(source, cur) {
                cur = new_cursor;
                continue 'lex;
            } else if let Some((token, new_cursor)) = self.lex_symbol(source, cur) {
                cur = new_cursor;

                // Omit empty tokens for valid, but empty syntax like newlines
                if token.token != Token::Empty {
                    tokens.push(token);
                }
                continue 'lex;
            } else if let Some((token, new_cursor)) = self.lex_numeric(source, cur) {
                cur = new_cursor;

                // Omit empty tokens for valid, but empty syntax like newlines
                if token.token != Token::Empty {
                    tokens.push(token);
                }
                continue 'lex;
            } else if let Some((token, new_cursor)) = self.lex_identifier(source, cur) {
                cur = new_cursor;

                // Omit empty tokens for valid, but empty syntax like newlines
                if token.token != Token::Empty {
                    tokens.push(token);
                }
                continue 'lex;
            } else if let Some((token, new_cursor)) = self.lex_string(source, cur) {
                cur = new_cursor;

                // Omit empty tokens for valid, but empty syntax like newlines
                if token.token != Token::Empty {
                    tokens.push(token);
                }
                continue 'lex;
            }
            let mut hint = "".to_string();

            if let Some(TokenContainer { loc: _, token }) = tokens.last() {
                hint = "after ".to_string();
                hint.push_str(format!("{:?}", token).as_str());
            }
            let loc = get_location_from_cursor(source, cur.pointer);

            lex_error!(
                format!("Unable to lex token {}, at {}:{}", hint, loc.line, loc.col),
                loc
            );
        }
        Ok(tokens)
    }

    pub fn lex_comment(&self, source: &str, ic: Cursor) -> Option<(TokenContainer, Cursor)> {
        let mut cur = ic;
        if source[cur.pointer..].starts_with("/*") {
            cur.pointer += 2;
            let mut char_iter = source[cur.pointer..].chars().peekable();
            while let Some(c) = char_iter.next() {
                cur.pointer += 1;
                if c == '\n' {
                    cur.loc.col = 0;
                    cur.loc.line += 1;
                } else if c == '*' && char_iter.peek() == Some(&'/') {
                    cur.pointer += 2;
                    break;
                }
            }
            return Some((
                TokenContainer {
                    token: Token::Comment,
                    loc: ic.loc,
                },
                cur,
            ));
        }
        if source[cur.pointer..].starts_with("--") {
            cur.pointer += 2;
            let char_iter = source[cur.pointer..].chars().peekable();
            for c in char_iter {
                cur.pointer += 1;
                if c == '\n' {
                    cur.loc.col = 0;
                    cur.loc.line += 1;
                    break;
                }
            }
            return Some((
                TokenContainer {
                    token: Token::Comment,
                    loc: ic.loc,
                },
                cur,
            ));
        }
        None
    }

    pub fn lex_numeric<'a>(
        &self,
        source: &'a str,
        ic: Cursor,
    ) -> Option<(TokenContainer<'a>, Cursor)> {
        let mut cur = ic;

        let mut period_found = false;
        let mut exp_marker_found = false;

        let mut char_iter = source[cur.pointer..].chars().peekable();

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
                if cur.pointer == (source.len() - 1) {
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
                    value: Cow::Borrowed(&source[ic.pointer..cur.pointer]),
                },
            },
            cur,
        ))
    }

    // lex_character_delimited looks through a source string starting at the
    // given cursor to find a start- and end- delimiter. The delimiter can
    // be escaped be preceeding the delimiter with itself.
    pub fn lex_character_delimited<'a>(
        &self,
        source: &'a str,
        ic: Cursor,
        delimiter: char,
        kind: TokenKind,
    ) -> Option<(TokenContainer<'a>, Cursor)> {
        let mut cur = ic;

        if source[cur.pointer..].is_empty() {
            return None;
        }

        let first_char = match get_chat_at(source, cur.pointer) {
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

        let mut escaped = false;

        let mut char_iter = source[cur.pointer..].chars().peekable();

        while let Some(c) = char_iter.next() {
            if c == delimiter {
                cur.pointer += 1;
                cur.loc.col += 1;
                // SQL escapes are via double characters, not backslash.
                match char_iter.peek() {
                    None => {
                        if let Some(src) = source.get((ic.pointer + 1)..(cur.pointer - 1)) {
                            let value = if escaped {
                                Cow::Owned(src.replace(
                                    &String::from_iter([delimiter, delimiter]),
                                    &String::from_iter([delimiter]),
                                ))
                            } else {
                                Cow::Borrowed(src)
                            };
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
                    }
                    Some(char) => {
                        if *char != delimiter {
                            if let Some(src) = source.get((ic.pointer + 1)..(cur.pointer - 1)) {
                                let value = if escaped {
                                    Cow::Owned(src.replace(
                                        &String::from_iter([delimiter, delimiter]),
                                        &String::from_iter([delimiter]),
                                    ))
                                } else {
                                    Cow::Borrowed(src)
                                };
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
                        } else if *char == delimiter {
                            escaped = true;
                            char_iter.next();
                        }
                    }
                }
            }

            cur.loc.col += 1;
            cur.pointer += 1;
        }

        None
    }

    pub fn lex_string<'a>(
        &self,
        source: &'a str,
        ic: Cursor,
    ) -> Option<(TokenContainer<'a>, Cursor)> {
        self.lex_character_delimited(source, ic, '\'', TokenKind::String)
    }

    // longestMatch iterates through a source string starting at the given
    // cursor to find the longest matching substring among the provided
    // options
    pub fn longest_match<'a>(&self, source: &'a str, ic: Cursor, options: &[&str]) -> &'a str {
        let mut text_match_len = 0;
        let cur = ic;

        let rest_of_text_len = source.len() - cur.pointer;

        for option in options {
            if option.len() > text_match_len
                && rest_of_text_len >= option.len()
                && source[cur.pointer..(cur.pointer + option.len())].eq_ignore_ascii_case(option)
            {
                text_match_len = option.len();
            }
        }

        &source[cur.pointer..(cur.pointer + text_match_len)]
    }

    pub fn lex_symbol(&self, source: &str, ic: Cursor) -> Option<(TokenContainer, Cursor)> {
        let c = match get_chat_at(source, ic.pointer) {
            None => {
                return None;
            }
            Some(value) => value,
        };
        let mut cur = ic;

        // Will get overwritten later if not an ignored syntax
        cur.pointer += 1;
        cur.loc.col += 1;

        // Syntax that should be thrown away
        if c == '\n' {
            cur.loc.line += 1;
            cur.loc.col = 0;
        }

        match c {
            ' ' | '\n' | '\r' | '\t' => {
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
        let symbol_match = self.longest_match(source, ic, SYMBOLS);
        // Unknown character
        if symbol_match.is_empty() {
            return None;
        }
        // != is rewritten as <>: https://www.postgresql.org/docs/9.5/functions-comparison.html
        let kind = match symbol_match {
            COMMA_SYMBOL => Token::Comma,
            EQUAL_SYMBOL => Token::Equal,
            NOT_EQUAL_SYMBOL | NOT_EQUAL_SYMBOL_2 => Token::NotEqual,
            ASTERISK_SYMBOL => Token::Asterisk,
            LEFT_PARENTHESIS_SYMBOL => Token::LeftParenthesis,
            RIGHT_PARENTHESIS_SYMBOL => Token::RightParenthesis,
            LESS_THAN_SYMBOL => Token::LessThan,
            LESS_THAN_OR_EQUAL_SYMBOL => Token::LessThanOrEqual,
            GREATER_THAN_SYMBOL => Token::GreaterThan,
            GREATER_THAN_OR_EQUAL_SYMBOL => Token::GreaterThanOrEqual,
            PLUS_SYMBOL => Token::Plus,
            MINUS_SYMBOL => Token::Minus,
            SLASH_SYMBOL => Token::Slash,
            MODULO_SYMBOL => Token::Modulo,
            EXPONENTIATION_SYMBOL => Token::Exponentiation,
            TYPE_CAST_SYMBOL => Token::TypeCast,
            SQUARE_ROOT_SYMBOL => Token::SquareRoot,
            CUBE_ROOT_SYMBOL => Token::CubeRoot,
            FACTORIAL_SYMBOL => Token::Factorial,
            FACTORIAL_PREFIX_SYMBOL => Token::FactorialPrefix,
            ABS_SYMBOL => Token::AbsoluteValue,
            BITWISE_AND_SYMBOL => Token::BitwiseAnd,
            BITWISE_OR_SYMBOL => Token::BitwiseOr,
            BITWISE_XOR_SYMBOL => Token::BitwiseXor,
            BITWISE_NOT_SYMBOL => Token::BitwiseNot,
            BITWISE_SHIFT_LEFT_SYMBOL => Token::BitwiseShiftLeft,
            BITWISE_SHIFT_RIGHT_SYMBOL => Token::BitwiseShiftRight,
            SEMICOLON_SYMBOL => Token::Semicolon,
            DOT_SYMBOL => Token::Dot,
            CONCAT_SYMBOL => Token::Concat,
            _ => {
                return None;
            }
        };

        cur.pointer = ic.pointer + symbol_match.len();
        cur.loc.col = ic.loc.col + symbol_match.len();

        Some((
            TokenContainer {
                loc: ic.loc,
                token: kind,
            },
            cur,
        ))
    }

    pub fn lex_keyword(&self, source: &str, ic: Cursor) -> Option<(TokenContainer, Cursor)> {
        let mut cur = ic;

        let keyword_match = self.longest_match(source, ic, KEYWORDS);
        if keyword_match.is_empty() {
            return None;
        }
        cur.pointer = ic.pointer + keyword_match.len();
        cur.loc.col = ic.loc.col + keyword_match.len();
        // Check if the word continues, thus being an identifier
        if let Some(next_char) = source.chars().nth(cur.pointer) {
            if is_char_valid_for_identifier(next_char) {
                return None;
            }
        }

        let mut kind = match keyword_match.to_ascii_lowercase().as_str() {
            SELECT_KEYWORD => Token::Select,
            FROM_KEYWORD => Token::From,
            WHERE_KEYWORD => Token::Where,
            AND_KEYWORD => Token::And,
            ORDER_KEYWORD => Token::Order,
            OR_KEYWORD => Token::Or,
            NOT_KEYWORD => Token::Not,
            DESC_KEYWORD => Token::Desc,
            ASC_KEYWORD => Token::Asc,
            AS_KEYWORD => Token::As,
            TRUE_KEYWORD => Token::True,
            FALSE_KEYWORD => Token::False,
            NULL_KEYWORD => Token::Null,
            JOIN_KEYWORD => Token::Join,
            INNER_KEYWORD => Token::Inner,
            LEFT_KEYWORD => Token::Left,
            RIGHT_KEYWORD => Token::Right,
            OUTER_KEYWORD => Token::Outer,
            FULL_KEYWORD => Token::Full,
            IS_KEYWORD => Token::Is,
            LIMIT_KEYWORD => Token::Limit,
            OFFSET_KEYWORD => Token::Offset,
            BY_KEYWORD => Token::By,
            DISTINCT_KEYWORD => Token::Distinct,
            CONSTRAINT_KEYWORD => Token::Constraint,
            ON_KEYWORD => Token::On,
            INT_KEYWORD => Token::Int,
            TEXT_KEYWORD => Token::Text,
            BIGINT_KEYWORD => Token::BigInt,
            SMALLINT_KEYWORD => Token::SmallInt,
            REAL_KEYWORD => Token::Real,
            DOUBLE_KEYWORD => Token::Double,
            PRECISION_KEYWORD => Token::Precision,
            BOOL_KEYWORD => Token::Bool,
            INSERT_KEYWORD => Token::Insert,
            VALUES_KEYWORD => Token::Values,
            INTO_KEYWORD => Token::Into,
            CREATE_KEYWORD => Token::Create,
            TABLE_KEYWORD => Token::Table,
            DROP_KEYWORD => Token::Drop,
            ALTER_KEYWORD => Token::Alter,
            DELETE_KEYWORD => Token::Delete,
            UPDATE_KEYWORD => Token::Update,
            PRIMARY_KEYWORD => Token::Primary,
            KEY_KEYWORD => Token::Key,
            UNIQUE_KEYWORD => Token::Unique,
            INDEX_KEYWORD => Token::Index,
            FOREIGN_KEYWORD => Token::Foreign,
            LIKE_KEYWORD => Token::Like,
            _ => {
                return None;
            }
        };

        if keyword_match == TRUE_KEYWORD || keyword_match == FALSE_KEYWORD {
            kind = Token::BoolValue {
                value: keyword_match == TRUE_KEYWORD,
            };
        }

        if keyword_match == NULL_KEYWORD {
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

    pub fn lex_identifier<'a>(
        &'a self,
        source: &'a str,
        ic: Cursor,
    ) -> Option<(TokenContainer<'a>, Cursor)> {
        // Handle separately if is a double-quoted identifier
        let token_result = self.lex_character_delimited(source, ic, '"', TokenKind::Identifier);
        if token_result.is_some() {
            return token_result;
        }
        if let Some(res) = self.lex_character_delimited(source, ic, '"', TokenKind::Identifier) {
            return Some(res);
        }

        let mut cur = ic;
        let c = match get_chat_at(source, ic.pointer) {
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

        for c in source[cur.pointer..].chars() {
            // Other characters count too, big ignoring non-ascii for now
            if is_char_valid_for_identifier(c) {
                cur.pointer += 1;
                cur.loc.col += 1;
                continue;
            }

            break;
        }

        if ic.pointer > cur.pointer {
            return None;
        }

        Some((
            TokenContainer {
                // Unquoted identifiers are case-insensitive
                loc: ic.loc,
                token: Token::IdentifierValue {
                    value: Cow::Borrowed(&source[ic.pointer..cur.pointer]),
                },
            },
            cur,
        ))
    }
}

pub fn get_location_from_cursor(source: &str, cursor: usize) -> TokenLocation {
    let rev_pos = source[..(cursor + 1)]
        .chars()
        .rev()
        .collect::<String>()
        .find('\n');
    let mut col = cursor;
    if let Some(rev_pos) = rev_pos {
        col = source[..(cursor + 1)].len() - rev_pos;
    }
    TokenLocation {
        col,
        line: source[..(cursor + 1)].matches('\n').count(),
    }
}

fn get_chat_at(source: &str, position: usize) -> Option<char> {
    source[position..(position + 1)].chars().next()
}

fn is_char_alphabetical(c: char) -> bool {
    ('A'..='Z').contains(&c) || ('a'..='z').contains(&c)
}

fn is_char_digit(c: char) -> bool {
    ('0'..='9').contains(&c)
}

fn is_char_valid_for_identifier(c: char) -> bool {
    is_char_alphabetical(c) || is_char_digit(c) || c == '$' || c == '_'
}

#[cfg(test)]
mod tests {
    use super::super::lexer::*;
    use test_macros::test_case;

    struct LexerTest<'a> {
        expected_result: bool,
        expected_value: Token<'a>,
        value: &'static str,
    }

    mod lex_numeric_tests {
        use super::test_case;
        use super::*;
        #[test_case(LexerTest {
        expected_result: true,
        value: "105",
        expected_value: Token::NumericValue {
            value: Cow::Borrowed("105"),
        },
          })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "105 ",
        expected_value: Token::NumericValue {
            value: Cow::Borrowed("105"),
        },
          })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "123.",
        expected_value: Token::NumericValue {
            value: Cow::Borrowed("123."),
        },
          } )]
        #[test_case(LexerTest {
        expected_result: true,
        value: "123.145",
        expected_value: Token::NumericValue {
            value: Cow::Borrowed("123.145"),
        },
           } )]
        #[test_case(LexerTest {
        expected_result: true,
        value: "1e5",
        expected_value: Token::NumericValue {
            value: Cow::Borrowed("1e5"),
        },
          } )]
        #[test_case(LexerTest {
        expected_result: true,
        value: "1.e21",
        expected_value: Token::NumericValue {
            value: Cow::Borrowed("1.e21"),
        },
           } )]
        #[test_case(LexerTest {
        expected_result: true,
        value: "1.1e2",
        expected_value: Token::NumericValue {
            value: Cow::Borrowed("1.1e2"),
        },
          } )]
        #[test_case(LexerTest {
        expected_result: true,
        value: "1.1e-2",
        expected_value: Token::NumericValue {
            value: Cow::Borrowed("1.1e-2"),
        },
         } )]
        #[test_case(LexerTest {
        expected_result: true,
        value: "1.1e+2",
        expected_value: Token::NumericValue {
            value: Cow::Borrowed("1.1e+2"),
        },
        } )]
        #[test_case(LexerTest {
        expected_result: true,
        value: "1e-1",
        expected_value: Token::NumericValue {
            value: Cow::Borrowed("1e-1"),
        },
        } )]
        #[test_case(LexerTest {
        expected_result: true,
        value: ".1",
        expected_value: Token::NumericValue {
            value: Cow::Borrowed(".1"),
        },
        } )]
        #[test_case(LexerTest {
        expected_result: true,
        value: "4.",
        expected_value: Token::NumericValue {
            value: Cow::Borrowed("4."),
        },
        } )]
        #[test_case(LexerTest {
        expected_result: false,
        value: "e4",
        expected_value: Token::Empty,
        } )]
        #[test_case(LexerTest {
        expected_result: false,
        value: "1..",
        expected_value: Token::Empty,
        } )]
        #[test_case(LexerTest {
        expected_result: false,
        value: "1ee4",
        expected_value: Token::Empty,
        } )]
        #[test_case(LexerTest {
        expected_result: false,
        value: " 1",
        expected_value: Token::Empty,
        })]
        fn lexer_num_test(test: LexerTest) {
            let lexer = Lexer::new();
            let result = lexer.lex_numeric(
                test.value,
                Cursor {
                    pointer: 0,
                    loc: TokenLocation { col: 0, line: 0 },
                },
            );

            if test.expected_result && result.is_some() {
                let (token_c, _) = result.expect("Expected a token");
                assert_eq!(test.expected_value, token_c.token);
            } else if !test.expected_result && result.is_some() {
                panic!("Expected no token, got {:?}", result);
            } else if test.expected_result && result.is_none() {
                panic!("Expected a token from {}", test.value);
            }
        }
    }

    mod lex_symbol_string {
        use super::test_case;
        use super::*;
        #[test_case(LexerTest {
        expected_result: true,
        value: "'abc'",
        expected_value: Token::StringValue {
            value: Cow::Borrowed("abc"),
        },
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "'a'",
        expected_value: Token::StringValue {
            value: Cow::Borrowed("a"),
        },
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "'a b'",
        expected_value: Token::StringValue {
            value: Cow::Borrowed("a b"),
        },
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "'a b  c '",
        expected_value: Token::StringValue {
            value: Cow::Borrowed("a b  c "),
        },
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "'a b '''' c'",
        expected_value: Token::StringValue {
            value: Cow::Borrowed("a b '' c"),
        },
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "'a''b'",
        expected_value: Token::StringValue {
            value: Cow::Borrowed("a'b"),
        },
    })]
        #[test_case(LexerTest {
        expected_result: false,
        value: "a",
        expected_value: Token::Empty,
    })]
        #[test_case(LexerTest {
        expected_result: false,
        value: "",
        expected_value: Token::Empty,
    })]
        #[test_case(LexerTest {
        expected_result: false,
        value: "'",
        expected_value: Token::Empty,
    })]
        #[test_case(LexerTest {
        expected_result: false,
        value: " 'bpp'",
        expected_value: Token::Empty,
    })]
        fn lexer_string_test(test: LexerTest) {
            let lexer = Lexer::new();
            let result = lexer.lex_string(
                test.value,
                Cursor {
                    pointer: 0,
                    loc: TokenLocation { col: 0, line: 0 },
                },
            );

            if test.expected_result && result.is_some() {
                let (token_c, _) = result.expect("Expected a token");
                assert_eq!(test.expected_value, token_c.token);
            } else if !test.expected_result && result.is_some() {
                panic!("Expected no token, got {:?}", result);
            } else if test.expected_result && result.is_none() {
                panic!("Expected a token from {}", test.value);
            }
        }
    }

    mod lex_symbol_tests {
        use super::test_case;
        use super::*;

        #[test_case(LexerTest {
        expected_result: true,
        value: "= ",
        expected_value: Token::Equal,
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "||",
        expected_value: Token::Concat,
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: ",",
        expected_value: Token::Comma,
    })]
        #[test_case(LexerTest {
        expected_result: false,
        value: "a",
        expected_value: Token::Empty,
    })]
        fn lexer_symbol_test(test: LexerTest) {
            let lexer = Lexer::new();
            let result = lexer.lex_symbol(
                test.value,
                Cursor {
                    pointer: 0,
                    loc: TokenLocation { col: 0, line: 0 },
                },
            );

            if test.expected_result && result.is_some() {
                let (token_c, _) = result.expect("Expected a token");
                assert_eq!(test.expected_value, token_c.token);
            } else if !test.expected_result && result.is_some() {
                panic!("Expected no token, got {:?}", result);
            } else if test.expected_result && result.is_none() {
                panic!("Expected a token from {}", test.value);
            }
        }

        #[test_case(LexerTest {
        expected_result: true,
        value: "a",
        expected_value: Token::IdentifierValue {
            value: Cow::Borrowed("a"),
        },
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "abc",
        expected_value: Token::IdentifierValue {
            value: Cow::Borrowed("abc"),
        },
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "abc ",
        expected_value: Token::IdentifierValue {
            value: Cow::Borrowed("abc"),
        },
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "abc ",
        expected_value: Token::IdentifierValue {
            value: Cow::Borrowed("abc"),
        },
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "a9$",
        expected_value: Token::IdentifierValue {
            value: Cow::Borrowed("a9$"),
        },
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "userName",
        expected_value: Token::IdentifierValue {
            value: Cow::Borrowed("userName"),
        },
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "\"userName\"",
        expected_value: Token::IdentifierValue {
            value: Cow::Borrowed("userName"),
        },
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "indexed_value",
        expected_value: Token::IdentifierValue {
            value: Cow::Borrowed("indexed_value"),
        },
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "unique_values",
        expected_value: Token::IdentifierValue {
            value: Cow::Borrowed("unique_values"),
        },
    })]
        #[test_case(LexerTest {
        expected_result: false,
        value: "\"",
        expected_value: Token::Empty,
    })]
        #[test_case(LexerTest {
        expected_result: false,
        value: "_sddfdff",
        expected_value: Token::Empty,
    })]
        #[test_case(LexerTest {
        expected_result: false,
        value: "9dfdfd",
        expected_value: Token::Empty,
    })]
        #[test_case(LexerTest {
        expected_result: false,
        value: " abc",
        expected_value: Token::Empty,
    })]
        fn lexer_identifier_test(test: LexerTest) {
            let lexer = Lexer::new();
            let result = lexer.lex_identifier(
                test.value,
                Cursor {
                    pointer: 0,
                    loc: TokenLocation { col: 0, line: 0 },
                },
            );

            if test.expected_result && result.is_some() {
                let (token_c, _) = result.expect("Expected a token");
                assert_eq!(test.expected_value, token_c.token);
            } else if !test.expected_result && result.is_some() {
                panic!("Expected no token, got {:?}", result);
            } else if test.expected_result && result.is_none() {
                panic!("Expected a token from {}", test.value);
            }
        }
    }

    mod lex_keyword_tests {
        use super::test_case;
        use super::*;

        #[test_case(LexerTest {
        expected_result: true,
        value: "select ",
        expected_value: Token::Select,
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "from",
        expected_value: Token::From,
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "as",
        expected_value: Token::As,
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "SELECT",
        expected_value: Token::Select,
    })]
        #[test_case(LexerTest {
        expected_result: true,
        value: "into",
        expected_value: Token::Into,
    })]
        #[test_case(LexerTest {
        expected_result: false,
        value: " from",
        expected_value: Token::Empty,
    })]
        #[test_case(LexerTest {
        expected_result: false,
        value: "fdfd",
        expected_value: Token::Empty,
    })]
        fn lexer_keyword_test(test: LexerTest) {
            let lexer = Lexer::new();
            let result = lexer.lex_keyword(
                test.value,
                Cursor {
                    pointer: 0,
                    loc: TokenLocation { col: 0, line: 0 },
                },
            );

            if test.expected_result && result.is_some() {
                let (token_c, _) = result.expect("Expected a token");
                assert_eq!(test.expected_value, token_c.token);
            } else if !test.expected_result && result.is_some() {
                panic!("Expected no token, got {:?}", result);
            } else if test.expected_result && result.is_none() {
                panic!("Expected a token from {}", test.value);
            }
        }
    }

    mod lexer_tests {
        use super::test_case;
        use super::*;

        struct LexTest<'a> {
            valid: bool,
            input: &'static str,
            tokens: Vec<TokenContainer<'a>>,
        }

        #[test_case(LexTest {
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
                    value: Cow::Borrowed("a"),
                },
            },
        ],
        })]
        #[test_case(LexTest {
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
        })]
        #[test_case(LexTest {
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
                    value: Cow::Borrowed("1"),
                },
            },
        ],
        })]
        #[test_case(LexTest {
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
                    value: Cow::Borrowed("foo"),
                },
            },
            TokenContainer {
                loc: TokenLocation { col: 13, line: 0 },
                token: Token::Concat,
            },
            TokenContainer {
                loc: TokenLocation { col: 16, line: 0 },
                token: Token::StringValue {
                    value: Cow::Borrowed("bar"),
                },
            },
            TokenContainer {
                loc: TokenLocation { col: 21, line: 0 },
                token: Token::Semicolon,
            },
        ],
        })]
        #[test_case(LexTest {
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
                    value: Cow::Borrowed("u"),
                },
            },
            TokenContainer {
                loc: TokenLocation { col: 15, line: 0 },
                token: Token::LeftParenthesis,
            },
            TokenContainer {
                loc: TokenLocation { col: 16, line: 0 },
                token: Token::IdentifierValue {
                    value: Cow::Borrowed("id"),
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
                    value: Cow::Borrowed("name"),
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
        })]
        #[test_case(LexTest {
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
                    value: Cow::Borrowed("users"),
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
                    value: Cow::Borrowed("545"),
                },
            },
            TokenContainer {
                loc: TokenLocation { col: 30, line: 0 },
                token: Token::Comma,
            },
            TokenContainer {
                loc: TokenLocation { col: 32, line: 0 },
                token: Token::NumericValue {
                    value: Cow::Borrowed("232"),
                },
            },
            TokenContainer {
                loc: TokenLocation { col: 36, line: 0 },
                token: Token::RightParenthesis,
            },
        ],
        })]
        #[test_case(LexTest {
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
                    value: Cow::Borrowed("id"),
                },
            },
            TokenContainer {
                loc: TokenLocation { col: 10, line: 0 },
                token: Token::From,
            },
            TokenContainer {
                loc: TokenLocation { col: 15, line: 0 },
                token: Token::IdentifierValue {
                    value: Cow::Borrowed("users"),
                },
            },
            TokenContainer {
                loc: TokenLocation { col: 20, line: 0 },
                token: Token::Semicolon,
            },
        ],
        })]
        #[test_case(LexTest {
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
                    value: Cow::Borrowed("id"),
                },
            },
            TokenContainer {
                loc: TokenLocation { col: 9, line: 0 },
                token: Token::Comma,
            },
            TokenContainer {
                loc: TokenLocation { col: 11, line: 0 },
                token: Token::IdentifierValue {
                    value: Cow::Borrowed("name"),
                },
            },
            TokenContainer {
                loc: TokenLocation { col: 16, line: 0 },
                token: Token::From,
            },
            TokenContainer {
                loc: TokenLocation { col: 21, line: 0 },
                token: Token::IdentifierValue {
                    value: Cow::Borrowed("users"),
                },
            },
            TokenContainer {
                loc: TokenLocation { col: 26, line: 0 },
                token: Token::Semicolon,
            },
        ],
        })]
        fn lexer_whole_test(test: LexTest) {
            let lexer = Lexer::new();
            let result = lexer.lex(test.input);

            if test.valid && result.is_ok() {
                let tokens = result.expect("Expected a valid result");
                assert_eq!(test.tokens, tokens);
            } else if !test.valid && result.is_ok() {
                panic!("Expected no valid result, got {:?}", result);
            } else if test.valid && result.is_err() {
                panic!("Expected a valid result from {}", test.input);
            }
        }
    }

    #[cfg(feature = "alloc_counter")]
    mod alloc_tests {
        use super::test_case;
        use super::*;

        use alloc_counter::{count_alloc, AllocCounterSystem};

        #[global_allocator]
        static A: AllocCounterSystem = AllocCounterSystem;

        struct LexAllocTest<'a> {
            valid: bool,
            input: &'static str,
            allocations: usize,
            tokens: Vec<TokenContainer<'a>>,
        }

        #[test_case(LexAllocTest {
            valid: true,
            input: "SELECT id, name FROM users;",
            allocations: 3,
            tokens: vec![
                TokenContainer {
                    loc: TokenLocation { col: 0, line: 0 },
                    token: Token::Select,
                },
                TokenContainer {
                    loc: TokenLocation { col: 7, line: 0 },
                    token: Token::IdentifierValue {
                        value: Cow::Borrowed("id"),
                    },
                },
                TokenContainer {
                    loc: TokenLocation { col: 9, line: 0 },
                    token: Token::Comma,
                },
                TokenContainer {
                    loc: TokenLocation { col: 11, line: 0 },
                    token: Token::IdentifierValue {
                        value: Cow::Borrowed("name"),
                    },
                },
                TokenContainer {
                    loc: TokenLocation { col: 16, line: 0 },
                    token: Token::From,
                },
                TokenContainer {
                    loc: TokenLocation { col: 21, line: 0 },
                    token: Token::IdentifierValue {
                        value: Cow::Borrowed("users"),
                    },
                },
                TokenContainer {
                    loc: TokenLocation { col: 26, line: 0 },
                    token: Token::Semicolon,
                },
            ],
        })]
        fn lexer_whole_alloc_test(test: LexAllocTest) {
            let lexer = Lexer::new();
            let ((allocations, reallocations, deallocations), result) =
                count_alloc(|| lexer.lex(test.input));

            if test.valid && result.is_ok() {
                let tokens = result.expect("Expected a valid result");
                assert_eq!(test.tokens, tokens);
            } else if !test.valid && result.is_ok() {
                panic!("Expected no valid result, got {result:?}");
            } else if test.valid && result.is_err() {
                panic!("Expected a valid result from {}", test.input);
            }

            if allocations > test.allocations {
                panic!(
                    "Expected maximum {} allocations, got {allocations}",
                    test.allocations
                );
            }
        }
    }
}
