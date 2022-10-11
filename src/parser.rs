use strum::IntoEnumIterator;

use crate::{
    ast::{Expr, InfixOp},
    idents::Idents,
    lexer::Lexer,
    token::{Symbol, Token, TokenKind},
};

struct Parser<'s> {
    lexer: Lexer<'s>,
    token: Option<Token<'s>>,
    idents: Idents<'s>,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum Prec {
    Product,
    Sum,
    Bracket,
}

fn op_symbol(op: InfixOp) -> Symbol {
    match op {
        InfixOp::Add => Symbol::Plus,
        InfixOp::Subtract => Symbol::Minus,
    }
}

fn op_prec(op: InfixOp) -> Prec {
    match op {
        InfixOp::Add => Prec::Sum,
        InfixOp::Subtract => Prec::Sum,
    }
}

impl<'s> Parser<'s> {
    fn new(source: &'s str) -> Parser<'s> {
        let mut lexer = Lexer::new(source);
        let token = lexer.next_token();
        Parser {
            lexer,
            token,
            idents: Idents::new(),
        }
    }
    fn peek(&self) -> Option<TokenKind<'s>> {
        self.token.map(|token| token.kind)
    }
    fn next(&mut self) {
        match self.token {
            None => panic!(),
            _ => self.token = self.lexer.next_token(),
        }
    }
    fn eat_symbol(&mut self, s0: Symbol) -> bool {
        match self.peek() {
            Some(TokenKind::Symbol(s1)) if s0 == s1 => {
                self.next();
                true
            }
            _ => false,
        }
    }
    fn expect_symbol(&mut self, symbol: Symbol) -> Result<(), ()> {
        if self.eat_symbol(symbol) {
            Ok(())
        } else {
            Err(())
        }
    }
    fn parse_expr(&mut self, prec: Prec) -> Result<Expr<'s>, ()> {
        let mut left = match self.peek() {
            Some(TokenKind::Int(value)) => {
                self.next();
                Expr::Int(value.parse().unwrap())
            }
            Some(TokenKind::Ident(ident)) => {
                self.next();
                Expr::Ident(self.idents.intern(ident))
            }
            Some(TokenKind::Symbol(Symbol::OpenBrace)) => {
                self.next();
                let expr = self.parse_expr(Prec::Bracket)?;
                self.expect_symbol(Symbol::CloseBrace)?;
                expr
            }
            _ => panic!(),
        };
        'outer: loop {
            for op in InfixOp::iter() {
                if self.eat_symbol(op_symbol(op)) && prec > op_prec(op) {
                    let right = self.parse_expr(op_prec(op))?;
                    left = Expr::Infix {
                        left: Box::new(left),
                        right: Box::new(right),
                        op,
                    };
                    continue 'outer;
                }
            }
            break;
        }
        Ok(left)
    }
}

#[test]
fn test_parse_expr() {
    let mut parser = Parser::new("5 + (2 - 9)");
    let expr = parser.parse_expr(Prec::Bracket);
    assert_eq!(
        expr,
        Ok(Expr::Infix {
            left: Box::new(Expr::Int(5)),
            op: InfixOp::Add,
            right: Box::new(Expr::Infix {
                left: Box::new(Expr::Int(2)),
                op: InfixOp::Subtract,
                right: Box::new(Expr::Int(9)),
            })
        })
    )
}
