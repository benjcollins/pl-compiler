#[cfg(test)]
use crate::{lexer::Lexer, token::{TokenKind, Symbol, Keyword}};

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

#[test]
fn test_lex_comment() {
    let mut lexer = Lexer::new("// hello\n");
    assert!(lexer.next_token().is_none());
}