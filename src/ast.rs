use std::fmt;

use strum::EnumIter;

use crate::{idents::Ident, token::Symbol};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'s> {
    Int(u32),
    Infix {
        left: Box<Expr<'s>>,
        right: Box<Expr<'s>>,
        op: InfixOp,
    },
    Ref(RefExpr<'s>),
    Ident(Ident<'s>),
}

#[derive(Debug, Clone, Copy, PartialEq, EnumIter)]
pub enum InfixOp {
    Add,
    Subtract,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RefExpr<'s> {
    Ident(Ident<'s>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'s> {
    Decl {
        name: Ident<'s>,
        ty: Option<Type>,
        expr: Option<Expr<'s>>,
    },
    Assign {
        ref_expr: RefExpr<'s>,
        expr: Option<Expr<'s>>,
    },
    If(If<'s>),
    While {
        cond: Expr<'s>,
        block: Block<'s>,
    },
    Return(Option<Expr<'s>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct If<'s> {
    pub cond: Expr<'s>,
    pub if_block: Block<'s>,
    pub else_block: Else<'s>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Else<'s> {
    Block(Block<'s>),
    If(Box<If<'s>>),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block<'s>(pub Vec<Stmt<'s>>);

#[derive(Debug, Clone, PartialEq)]
pub struct Param<'s> {
    name: Ident<'s>,
    ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func<'s> {
    params: Vec<Param<'s>>,
    returns: Option<Type>,
    block: Option<Block<'s>>,
}

impl InfixOp {
    pub fn symbol(&self) -> Symbol {
        match self {
            InfixOp::Add => Symbol::Plus,
            InfixOp::Subtract => Symbol::Minus,
        }
    }
}

impl<'s> fmt::Display for Expr<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}
