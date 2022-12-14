use std::fmt;

use strum::EnumIter;

#[derive(Debug, Clone)]
pub struct Span<T> {
    pub ty: T,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Call(FuncCall),
    Int(u32),
    Bool(bool),
    Infix {
        left: Box<Span<Expr>>,
        right: Box<Span<Expr>>,
        op: InfixOp,
    },
    Deref(Box<Span<Expr>>),
    Ref(Span<RefExpr>),
    Ident(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    pub name: String,
    pub args: Vec<Span<Expr>>,
}

#[derive(Debug, Clone, Copy, EnumIter, PartialEq)]
pub enum InfixOp {
    Add,
    Subtract,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RefExpr {
    Ident(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Ptr(Box<Span<Type>>),
    Int(IntType),
    Bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IntType {
    pub size: IntSize,
    pub signed: bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IntSize {
    B64,
    B32,
    B16,
    B8,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Decl {
        name: String,
        ty: Option<Span<Type>>,
        expr: Option<Span<Expr>>,
    },
    Assign {
        ref_expr: Span<RefExpr>,
        expr: Span<Expr>,
    },
    DerefAssign {
        ref_expr: Span<Expr>,
        expr: Span<Expr>,
    },
    If(If),
    While {
        cond: Span<Expr>,
        block: Block,
    },
    Return(Option<Span<Expr>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub cond: Span<Expr>,
    pub if_block: Block,
    pub else_block: Else,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Else {
    Block(Block),
    If(Box<If>),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Span<Stmt>>);

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Span<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub name: String,
    pub params: Vec<Param>,
    pub returns: Option<Span<Type>>,
    pub block: Block,
}

pub struct Program {
    pub funcs: Vec<Func>,
}

impl<T: PartialEq> PartialEq for Span<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl<T: fmt::Display> fmt::Display for Span<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ty)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Bool(true) => write!(f, "true"),
            Expr::Bool(false) => write!(f, "false"),
            Expr::Int(val) => write!(f, "{}", val),
            Expr::Infix { left, right, op } => write!(f, "({} {} {})", left, op, right),
            Expr::Ref(expr) => write!(f, "&{}", expr),
            Expr::Ident(ident) => write!(f, "{}", ident.as_str()),
            Expr::Deref(expr) => write!(f, "*{}", expr),
            Expr::Call(func_call) => write!(f, "{}", func_call),
        }
    }
}

impl fmt::Display for FuncCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.name)?;
        let mut arg_iter = self.args.iter();
        if let Some(arg) = arg_iter.next() {
            write!(f, "{}", arg)?;
            for arg in arg_iter {
                write!(f, ", {}", arg)?;
            }
        }

        write!(f, ")")
    }
}

impl fmt::Display for RefExpr {
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
            Type::Ptr(ty) => write!(f, "*{}", ty),
            Type::Int(int) => write!(f, "{}", int),
            Type::Bool => write!(f, "bool"),
        }
    }
}

impl fmt::Display for IntType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let size = match self.size {
            IntSize::B64 => "64",
            IntSize::B32 => "32",
            IntSize::B16 => "16",
            IntSize::B8 => "8",
        };
        if self.signed {
            write!(f, "i{}", size)
        } else {
            write!(f, "u{}", size)
        }
    }
}
