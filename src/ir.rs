use std::{fmt, hash, rc::Rc};

use crate::{ast::InfixOp, ty::TypeVarRef};

pub struct Program {
    pub funcs: Vec<Func>,
}

pub struct Block {
    pub stmts: Vec<Stmt>,
    pub branch: Branch,
}

pub struct Func {
    pub name: String,
    pub params: Vec<VarRef>,
    pub blocks: Vec<Block>,
    pub entry: BlockRef,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Decl(VarRef),
    Drop(VarRef),
    Assign { ref_expr: Expr, expr: Expr },
}

#[derive(Clone)]
pub enum Branch {
    Static(BlockRef),
    Cond {
        cond: Expr,
        if_true: BlockRef,
        if_false: BlockRef,
    },
    Return(Option<Expr>),
    Call {
        name: String,
        args: Vec<Expr>,
        return_to: BlockRef,
    },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Bool(bool),
    Int(u32),
    Infix {
        left: Box<Expr>,
        right: Box<Expr>,
        op: InfixOp,
    },
    Ref(RefExpr),
    Deref(Box<Expr>),
    Var(VarRef),
    Returned,
}

#[derive(Debug, Clone)]
pub enum RefExpr {
    Var(VarRef),
}

#[derive(Debug)]
pub struct Var {
    pub name: String,
    pub ty: TypeVarRef,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct BlockRef(usize);

#[derive(Debug, Clone)]
pub struct VarRef(Rc<Var>);

impl PartialEq for VarRef {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl hash::Hash for VarRef {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0.as_ref() as *const _ as usize)
    }
}

impl Eq for VarRef {}

impl VarRef {
    pub fn new(name: impl Into<String>, ty: TypeVarRef) -> VarRef {
        VarRef(Rc::new(Var {
            name: name.into(),
            ty,
        }))
    }
    pub fn name(&self) -> &str {
        &self.0.name
    }
    pub fn ty(&self) -> &TypeVarRef {
        &self.0.ty
    }
}

impl Block {
    pub fn new() -> Block {
        Block {
            stmts: vec![],
            branch: Branch::Return(None),
        }
    }
}

impl Func {
    pub fn new(name: impl Into<String>) -> Func {
        Func {
            name: name.into(),
            blocks: vec![Block::new()],
            entry: BlockRef(0),
            params: vec![],
        }
    }
    pub fn alloc_block(&mut self) -> BlockRef {
        self.blocks.push(Block::new());
        BlockRef(self.blocks.len() - 1)
    }
    pub fn get_block(&self, block: BlockRef) -> &Block {
        &self.blocks[block.0]
    }
    pub fn get_block_mut(&mut self, block: BlockRef) -> &mut Block {
        &mut self.blocks[block.0]
    }
    pub fn iter_blocks(&self) -> BlockRefIter {
        BlockRefIter {
            index: 0,
            end: self.blocks.len(),
        }
    }
}

pub struct BlockRefIter {
    index: usize,
    end: usize,
}

impl Iterator for BlockRefIter {
    type Item = BlockRef;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.end {
            self.index += 1;
            Some(BlockRef(self.index - 1))
        } else {
            None
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Decl(var) => write!(f, "var {}: {}", var.0.name, var.0.ty),
            Stmt::Drop(var) => write!(f, "drop {}", var.0.name),
            Stmt::Assign {
                ref_expr: Expr::Ref(ref_expr),
                expr,
            } => write!(f, "{} = {}", ref_expr, expr),
            Stmt::Assign { ref_expr, expr } => write!(f, "*{} = {}", ref_expr, expr),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Int(value) => write!(f, "{}", value),
            Expr::Infix { left, right, op } => write!(f, "({} {} {})", left, op, right),
            Expr::Ref(expr) => write!(f, "&{}", expr),
            Expr::Var(var) => write!(f, "{}", var.0.name),
            Expr::Deref(expr) => write!(f, "*{}", expr),
            Expr::Bool(true) => write!(f, "true"),
            Expr::Bool(false) => write!(f, "false"),
            Expr::Returned => write!(f, "returned"),
        }
    }
}

impl fmt::Display for RefExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RefExpr::Var(var) => write!(f, "{}", var.0.name),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for func in &self.funcs {
            writeln!(f, "{}", func)?;
        }
        Ok(())
    }
}

impl fmt::Display for BlockRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "b{}", self.0)
    }
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "func {}(", self.name)?;
        let mut param_iter = self.params.iter();
        if let Some(param) = param_iter.next() {
            write!(f, "{}: {}", param.0.name, param.0.ty)?;
            for param in param_iter {
                write!(f, ", {}: {}", param.0.name, param.0.ty)?;
            }
        }
        writeln!(f, ")")?;
        for block in self.iter_blocks() {
            writeln!(f, "{}:", block)?;
            for stmt in &self.get_block(block).stmts {
                writeln!(f, "  {}", stmt)?;
            }
            match &self.get_block(block).branch {
                Branch::Static(block) => {
                    writeln!(f, "  goto {}", block)
                }
                Branch::Cond {
                    if_true,
                    if_false,
                    cond,
                } => {
                    writeln!(f, "  if {} goto {} else goto {}", cond, if_true, if_false)
                }
                Branch::Return(Some(expr)) => writeln!(f, "  return {}", expr),
                Branch::Return(None) => writeln!(f, "  return"),
                Branch::Call {
                    name,
                    args,
                    return_to,
                } => {
                    write!(f, "  call {}(", name)?;
                    let mut args_iter = args.iter();
                    if let Some(arg) = args_iter.next() {
                        write!(f, "{}", arg)?;
                        for arg in args_iter {
                            write!(f, ", {}", arg)?;
                        }
                    }
                    writeln!(f, ")")?;
                    writeln!(f, "  goto {}", return_to)
                }
            }?;
        }
        Ok(())
    }
}
