use strum::EnumIter;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'s> {
    pub offset: usize,
    pub kind: TokenKind<'s>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind<'s> {
    Ident(&'s str),
    Int(&'s str),
    Symbol(Symbol),
    Keyword(Keyword),
}

#[derive(Debug, Clone, Copy, EnumIter, PartialEq)]
pub enum Symbol {
    Assign,
    Semicolon,
    OpenBrace,
    CloseBrace,
    Plus,
    Minus,
    Colon,
    OpenCurlyBrace,
    CloseCurlyBrace,
    Asterisk,
    Ampersand,
}

#[derive(Debug, Clone, Copy, EnumIter, PartialEq)]
pub enum Keyword {
    Func,
    Var,
    Return,
    While,
    If,
    Else,
    True,
    False,

    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,

    Bool,
}

impl Symbol {
    pub fn str(&self) -> &'static str {
        match self {
            Symbol::Assign => "=",
            Symbol::Semicolon => ";",
            Symbol::OpenBrace => "(",
            Symbol::CloseBrace => ")",
            Symbol::Plus => "+",
            Symbol::Minus => "-",
            Symbol::Colon => ":",
            Symbol::OpenCurlyBrace => "{",
            Symbol::CloseCurlyBrace => "}",
            Symbol::Asterisk => "*",
            Symbol::Ampersand => "&",
        }
    }
}

impl Keyword {
    pub fn str(&self) -> &'static str {
        match self {
            Keyword::Func => "func",
            Keyword::Var => "var",
            Keyword::Return => "return",
            Keyword::While => "while",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::I8 => "i8",
            Keyword::I16 => "i16",
            Keyword::I32 => "i32",
            Keyword::I64 => "i64",
            Keyword::U8 => "u8",
            Keyword::U16 => "u16",
            Keyword::U32 => "u32",
            Keyword::U64 => "u64",
            Keyword::Bool => "bool",
            Keyword::True => "true",
            Keyword::False => "false",
        }
    }
}
