use strum::IntoEnumIterator;

use crate::{
    ast::{Expr, InfixOp, RefExpr, Stmt, Type},
    idents::Idents,
    lexer::Lexer,
    token::{Keyword, Symbol, Token, TokenKind},
};

pub struct Parser<'s> {
    lexer: Lexer<'s>,
    token: Option<Token<'s>>,
    idents: Idents<'s>,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Prec {
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
    pub fn new(source: &'s str) -> Parser<'s> {
        let mut lexer = Lexer::new(source);
        let token = lexer.next_token();
        Parser {
            lexer,
            token,
            idents: Idents::new(),
        }
    }
    pub fn peek(&self) -> Option<TokenKind<'s>> {
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
    pub fn parse_expr(&mut self, prec: Prec) -> Result<Expr<'s>, ()> {
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
            _ => return Err(()),
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
    pub fn parse_type(&mut self) -> Result<Type, ()> {
        self.next();
        Ok(Type::I32)
    }
    pub fn parse_stmt(&mut self) -> Result<Stmt<'s>, ()> {
        match self.peek() {
            Some(TokenKind::Keyword(Keyword::Var)) => {
                self.next();
                let name = match self.peek() {
                    Some(TokenKind::Ident(name)) => self.idents.intern(name),
                    _ => return Err(()),
                };
                self.next();
                let ty = if self.eat_symbol(Symbol::Colon) {
                    Some(self.parse_type()?)
                } else {
                    None
                };
                let expr = if self.eat_symbol(Symbol::Assign) {
                    Some(self.parse_expr(Prec::Bracket)?)
                } else {
                    None
                };
                self.expect_symbol(Symbol::Semicolon)?;
                Ok(Stmt::Decl { name, ty, expr })
            }
            Some(TokenKind::Ident(name)) => {
                self.next();
                self.expect_symbol(Symbol::Assign)?;
                let expr = self.parse_expr(Prec::Bracket)?;
                self.expect_symbol(Symbol::Semicolon)?;
                Ok(Stmt::Assign {
                    ref_expr: RefExpr::Ident(self.idents.intern(name)),
                    expr,
                })
            }
            Some(TokenKind::Keyword(Keyword::Return)) => {
                self.next();
                let expr = if self.eat_symbol(Symbol::Semicolon) {
                    None
                } else {
                    let expr = self.parse_expr(Prec::Bracket)?;
                    self.expect_symbol(Symbol::Semicolon)?;
                    Some(expr)
                };
                Ok(Stmt::Return(expr))
            }
            _ => Err(()),
        }
    }
}
