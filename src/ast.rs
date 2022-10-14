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

#[derive(Debug, Clone, Copy, EnumIter, PartialEq)]
pub enum InfixOp {
    Add,
    Subtract,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RefExpr<'s> {
    Ident(Ident<'s>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'s> {
    Decl {
        name: Ident<'s>,
        ty: Option<Type>,
        expr: Option<Expr<'s>>,
    },
    Assign {
        ref_expr: RefExpr<'s>,
        expr: Expr<'s>,
    },
    DerefAssign {
        ptr: Expr<'s>,
        expr: Expr<'s>,
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
    pub name: Ident<'s>,
    pub params: Vec<Param<'s>>,
    pub returns: Option<Type>,
    pub block: Option<Block<'s>>,
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
        match self {
            Expr::Int(val) => write!(f, "{}", val),
            Expr::Infix { left, right, op } => write!(f, "({} {} {})", left, op, right),
            Expr::Ref(expr) => write!(f, "&{}", expr),
            Expr::Ident(ident) => write!(f, "{}", ident.as_str()),
        }
    }
}

impl<'s> fmt::Display for RefExpr<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RefExpr::Ident(ident) => write!(f, "{}", ident.as_str()),
        }
    }
}

impl fmt::Display for InfixOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InfixOp::Add => write!(f, "+"),
            InfixOp::Subtract => write!(f, "-"),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
        }
    }
}

impl<'s> Expr<'s> {
    pub fn infix(left: Expr<'s>, op: InfixOp, right: Expr<'s>) -> Expr<'s> {
        Expr::Infix {
            left: Box::new(left),
            right: Box::new(right),
            op,
        }
    }
    pub fn ident(s: &'s str) -> Expr<'s> {
        Expr::Ident(Ident::new(s))
    }
}

impl<'s> RefExpr<'s> {
    pub fn ident(s: &'s str) -> RefExpr<'s> {
        RefExpr::Ident(Ident::new(s))
    }
}

impl<'s> Block<'s> {
    pub fn empty() -> Block<'s> {
        Block(vec![])
    }
}
