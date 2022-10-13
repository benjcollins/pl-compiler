use strum::IntoEnumIterator;

use crate::token::{Keyword, Symbol, Token, TokenKind};

pub struct Lexer<'s> {
    offset: usize,
    source: &'s str,
}

impl<'s> Lexer<'s> {
    pub fn new(source: &'s str) -> Lexer<'s> {
        Lexer { offset: 0, source }
    }
    fn peek_char(&self) -> Option<char> {
        self.source[self.offset..].chars().next()
    }
    fn next(&mut self) {
        match self.peek_char() {
            Some(ch) => self.offset += ch.len_utf8(),
            None => panic!(),
        }
    }
    fn next_while(&mut self, cond: impl Fn(char) -> bool + Copy) {
        while self.peek_char().map_or(false, cond) {
            self.next();
        }
    }
    fn eat_str(&mut self, s: &str) -> bool {
        if self.source[self.offset..].starts_with(s) {
            self.offset += s.len();
            true
        } else {
            false
        }
    }
    pub fn next_token(&mut self) -> Option<Token<'s>> {
        let mut offset;
        let kind = 'outer: loop {
            offset = self.offset;
            let ch = match self.peek_char() {
                Some(ch) => ch,
                None => return None,
            };
            if ch.is_whitespace() {
                self.next();
                continue;
            }
            if self.eat_str("//") {
                self.next_while(|ch| ch != '\n')
            }
            if ch.is_numeric() {
                self.next_while(|ch| ch.is_numeric());
                break TokenKind::Int(&self.source[offset..self.offset]);
            }
            if ch.is_alphabetic() {
                self.next_while(|ch| ch.is_alphanumeric());
                let ident = &self.source[offset..self.offset];
                for keyword in Keyword::iter() {
                    if keyword.str() == ident {
                        break 'outer TokenKind::Keyword(keyword);
                    }
                }
                break TokenKind::Ident(ident);
            }
            for symbol in Symbol::iter() {
                if self.eat_str(symbol.str()) {
                    break 'outer TokenKind::Symbol(symbol);
                }
            }
            panic!()
        };
        Some(Token { kind, offset })
    }
}

#[test]
fn test_lex_ident() {
    let mut lexer = Lexer::new("hello");
    let token = lexer.next_token().unwrap();
    assert_eq!(token.kind, TokenKind::Ident("hello"))
}

#[test]
fn test_lex_int() {
    let mut lexer = Lexer::new("543");
    let token = lexer.next_token().unwrap();
    assert_eq!(token.kind, TokenKind::Int("543"))
}

#[test]
fn test_lex_symbol_plus() {
    let mut lexer = Lexer::new("+");
    let token = lexer.next_token().unwrap();
    assert_eq!(token.kind, TokenKind::Symbol(Symbol::Plus))
}

#[test]
fn test_lex_keyword_var() {
    let mut lexer = Lexer::new("var");
    let token = lexer.next_token().unwrap();
    assert_eq!(token.kind, TokenKind::Keyword(Keyword::Var));
}