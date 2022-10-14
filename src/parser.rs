use strum::IntoEnumIterator;

use crate::{
    ast::{Block, Else, Expr, If, InfixOp, RefExpr, Stmt, Type, Func},
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

#[derive(Debug, Clone)]
pub enum Expected {
    Symbol(Symbol),
    Ident,
    Stmt,
    Expr,
    Func,
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
    fn eat(&mut self, f: impl Fn(&TokenKind) -> bool) -> bool {
        match self.peek() {
            Some(token) if f(&token) => {
                self.next();
                true
            }
            _ => false,
        }
    }
    fn eat_symbol(&mut self, symbol: Symbol) -> bool {
        self.eat(|token| *token == TokenKind::Symbol(symbol))
    }
    fn eat_keyword(&mut self, keyword: Keyword) -> bool {
        self.eat(|token| *token == TokenKind::Keyword(keyword))
    }
    fn expect_symbol(&mut self, symbol: Symbol) -> Result<(), Expected> {
        if self.eat_symbol(symbol) {
            Ok(())
        } else {
            Err(Expected::Symbol(symbol))
        }
    }
    pub fn parse_expr(&mut self, prec: Prec) -> Result<Expr<'s>, Expected> {
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
            _ => return Err(Expected::Expr),
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
    pub fn parse_type(&mut self) -> Result<Type, Expected> {
        self.next();
        Ok(Type::I32)
    }
    fn parse_if(&mut self) -> Result<If<'s>, Expected> {
        let cond = self.parse_expr(Prec::Bracket)?;
        let if_block = self.parse_block()?;
        let else_block = if self.eat_keyword(Keyword::Else) {
            if self.eat_keyword(Keyword::If) {
                Else::If(Box::new(self.parse_if()?))
            } else {
                Else::Block(self.parse_block()?)
            }
        } else {
            Else::None
        };
        Ok(If {
            cond,
            if_block,
            else_block,
        })
    }
    pub fn parse_stmt(&mut self) -> Result<Stmt<'s>, Expected> {
        match self.peek() {
            Some(TokenKind::Keyword(Keyword::Var)) => {
                self.next();
                let name = match self.peek() {
                    Some(TokenKind::Ident(name)) => self.idents.intern(name),
                    _ => return Err(Expected::Ident),
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
            Some(TokenKind::Symbol(Symbol::Asterisk)) => {
                self.next();
                let ptr = self.parse_expr(Prec::Bracket)?;
                self.expect_symbol(Symbol::Assign)?;
                let expr = self.parse_expr(Prec::Bracket)?;
                self.expect_symbol(Symbol::Semicolon)?;
                Ok(Stmt::DerefAssign { ptr, expr })
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
            Some(TokenKind::Keyword(Keyword::While)) => {
                self.next();
                let cond = self.parse_expr(Prec::Bracket)?;
                let block = self.parse_block()?;
                Ok(Stmt::While { cond, block })
            }
            Some(TokenKind::Keyword(Keyword::If)) => {
                self.next();
                Ok(Stmt::If(self.parse_if()?))
            }
            _ => Err(Expected::Stmt),
        }
    }
    fn parse_block(&mut self) -> Result<Block<'s>, Expected> {
        let mut stmts = vec![];
        self.expect_symbol(Symbol::OpenCurlyBrace)?;
        while self.peek() != Some(TokenKind::Symbol(Symbol::CloseCurlyBrace)) {
            stmts.push(self.parse_stmt()?);
        }
        self.next();
        Ok(Block(stmts))
    }

    pub fn parse_func(&mut self) -> Result<Func<'s>, Expected> {
        match self.peek() {
            Some(TokenKind::Keyword(Keyword::Func)) => {
                self.next();
                let name = match self.peek() {
                    Some(TokenKind::Ident(name)) => name,
                    _ => return Err(Expected::Ident),
                };
                self.next();
                self.expect_symbol(Symbol::OpenBrace)?;
                self.expect_symbol(Symbol::CloseBrace)?;
                let block = if self.eat_symbol(Symbol::Semicolon) {
                    None
                } else {
                    Some(self.parse_block()?)
                };
                Ok(Func { name: self.idents.intern(name), params: vec![], returns: None, block })
            }
            _ => Err(Expected::Func),
        }
    }
}
