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
}

#[derive(Debug, Clone, Copy, EnumIter, PartialEq)]
pub enum Keyword {
    Func,
    Var,
    Return,
}

impl<'s> TokenKind<'s> {
    pub fn is_symbol(&self, symbol: Symbol) -> bool {
        match self {
            TokenKind::Symbol(token) => *token == symbol,
            _ => false,
        }
    }
    fn is_keyword(&self, keyword: Keyword) -> bool {
        match self {
            TokenKind::Keyword(token) => *token == keyword,
            _ => false,
        }
    }
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
        }
    }
}

impl Keyword {
    pub fn str(&self) -> &'static str {
        match self {
            Keyword::Func => "func",
            Keyword::Var => "var",
            Keyword::Return => "return",
        }
    }
}
