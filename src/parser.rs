use strum::IntoEnumIterator;

use crate::{
    ast::{
        Block, Else, Expr, Func, FuncCall, If, InfixOp, IntSize, IntType, Param, RefExpr, Stmt,
        Type,
    },
    lexer::Lexer,
    token::{Keyword, Symbol, Token, TokenKind},
};

pub struct Parser<'s> {
    lexer: Lexer<'s>,
    token: Option<Token<'s>>,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Prec {
    Deref,
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
    RefExpr,
    Type,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s str) -> Parser<'s> {
        let mut lexer = Lexer::new(source);
        let token = lexer.next_token();
        Parser { lexer, token }
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
    pub fn parse_ref_expr(&mut self) -> Result<RefExpr, Expected> {
        match self.peek() {
            Some(TokenKind::Ident(ident)) => {
                self.next();
                Ok(RefExpr::Ident(ident.to_string()))
            }
            _ => Err(Expected::RefExpr),
        }
    }
    pub fn parse_expr(&mut self, prec: Prec) -> Result<Expr, Expected> {
        let mut left = match self.peek() {
            Some(TokenKind::Keyword(Keyword::True)) => {
                self.next();
                Expr::Bool(true)
            }
            Some(TokenKind::Keyword(Keyword::False)) => {
                self.next();
                Expr::Bool(false)
            }
            Some(TokenKind::Int(value)) => {
                self.next();
                Expr::Int(value.parse().unwrap())
            }
            Some(TokenKind::Ident(ident)) => {
                self.next();
                if self.eat_symbol(Symbol::OpenBrace) {
                    let args = self.parse_list(Symbol::Comma, Symbol::CloseBrace, |parser| {
                        parser.parse_expr(Prec::Bracket)
                    })?;
                    Expr::Call(FuncCall {
                        name: ident.to_string(),
                        args,
                    })
                } else {
                    Expr::Ident(ident.to_string())
                }
            }
            Some(TokenKind::Symbol(Symbol::Ampersand)) => {
                self.next();
                Expr::Ref(self.parse_ref_expr()?)
            }
            Some(TokenKind::Symbol(Symbol::Asterisk)) => {
                self.next();
                Expr::Deref(Box::new(self.parse_expr(Prec::Deref)?))
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
                if prec > op_prec(op) && self.eat_symbol(op_symbol(op)) {
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
    pub fn parse_int_ty(&mut self, size: IntSize, signed: bool) -> Result<Type, Expected> {
        self.next();
        Ok(Type::Int(IntType { size, signed }))
    }
    pub fn parse_type(&mut self) -> Result<Type, Expected> {
        match self.peek() {
            Some(TokenKind::Symbol(Symbol::Asterisk)) => {
                self.next();
                Ok(Type::Ptr(Box::new(self.parse_type()?)))
            }
            Some(TokenKind::Keyword(Keyword::I8)) => self.parse_int_ty(IntSize::B8, true),
            Some(TokenKind::Keyword(Keyword::I16)) => self.parse_int_ty(IntSize::B16, true),
            Some(TokenKind::Keyword(Keyword::I32)) => self.parse_int_ty(IntSize::B32, true),
            Some(TokenKind::Keyword(Keyword::I64)) => self.parse_int_ty(IntSize::B64, true),

            Some(TokenKind::Keyword(Keyword::U8)) => self.parse_int_ty(IntSize::B8, false),
            Some(TokenKind::Keyword(Keyword::U16)) => self.parse_int_ty(IntSize::B16, false),
            Some(TokenKind::Keyword(Keyword::U32)) => self.parse_int_ty(IntSize::B32, false),
            Some(TokenKind::Keyword(Keyword::U64)) => self.parse_int_ty(IntSize::B64, false),

            Some(TokenKind::Keyword(Keyword::Bool)) => {
                self.next();
                Ok(Type::Bool)
            }

            _ => return Err(Expected::Type),
        }
    }
    fn parse_if(&mut self) -> Result<If, Expected> {
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
    pub fn parse_stmt(&mut self) -> Result<Stmt, Expected> {
        match self.peek() {
            Some(TokenKind::Keyword(Keyword::Var)) => {
                self.next();
                let name = match self.peek() {
                    Some(TokenKind::Ident(name)) => name.to_string(),
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
                    ref_expr: RefExpr::Ident(name.to_string()),
                    expr,
                })
            }
            Some(TokenKind::Symbol(Symbol::Asterisk)) => {
                self.next();
                let ptr = self.parse_expr(Prec::Bracket)?;
                self.expect_symbol(Symbol::Assign)?;
                let expr = self.parse_expr(Prec::Bracket)?;
                self.expect_symbol(Symbol::Semicolon)?;
                Ok(Stmt::DerefAssign {
                    ref_expr: ptr,
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
    fn parse_block(&mut self) -> Result<Block, Expected> {
        let mut stmts = vec![];
        self.expect_symbol(Symbol::OpenCurlyBrace)?;
        while self.peek() != Some(TokenKind::Symbol(Symbol::CloseCurlyBrace)) {
            stmts.push(self.parse_stmt()?);
        }
        self.next();
        Ok(Block(stmts))
    }
    fn parse_list<T>(
        &mut self,
        sep: Symbol,
        term: Symbol,
        item: impl Fn(&mut Parser) -> Result<T, Expected>,
    ) -> Result<Vec<T>, Expected> {
        let mut items = vec![];
        if self.eat_symbol(term) {
            return Ok(items);
        }
        loop {
            items.push(item(self)?);
            if !self.eat_symbol(sep) {
                break;
            }
        }
        self.expect_symbol(term)?;
        Ok(items)
    }
    fn parse_param(&mut self) -> Result<Param, Expected> {
        let name = match self.peek() {
            Some(TokenKind::Ident(name)) => name.to_string(),
            _ => return Err(Expected::Ident),
        };
        self.next();
        self.expect_symbol(Symbol::Colon)?;
        let ty = self.parse_type()?;
        Ok(Param { name, ty })
    }
    pub fn parse_func(&mut self) -> Result<Func, Expected> {
        match self.peek() {
            Some(TokenKind::Keyword(Keyword::Func)) => {
                self.next();
                let name = match self.peek() {
                    Some(TokenKind::Ident(name)) => name,
                    _ => return Err(Expected::Ident),
                };
                self.next();
                self.expect_symbol(Symbol::OpenBrace)?;
                let params = self.parse_list(Symbol::Comma, Symbol::CloseBrace, |parser| {
                    parser.parse_param()
                })?;
                let block = if self.eat_symbol(Symbol::Semicolon) {
                    None
                } else {
                    Some(self.parse_block()?)
                };
                Ok(Func {
                    name: name.to_string(),
                    params,
                    returns: None,
                    block,
                })
            }
            _ => Err(Expected::Func),
        }
    }
}
