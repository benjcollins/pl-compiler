use std::{
    cell::{Ref, RefCell, RefMut},
    fmt, hash,
    rc::{Rc, Weak},
};

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
    pub params: Vec<Rc<Var>>,
    pub blocks: Vec<StrongBlockRef>,
    pub entry: WeakBlockRef,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Decl(Rc<Var>),
    Drop(Rc<Var>),
    Assign { ref_expr: Expr, expr: Expr },
}

#[derive(Clone)]
pub enum Branch {
    Static(WeakBlockRef),
    Cond {
        cond: Expr,
        if_true: WeakBlockRef,
        if_false: WeakBlockRef,
    },
    Return(Option<Expr>),
    Call {
        name: String,
        args: Vec<Expr>,
        return_to: WeakBlockRef,
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
    Var(Rc<Var>),
    Returned,
}

#[derive(Debug, Clone)]
pub enum RefExpr {
    Var(Rc<Var>),
}

#[derive(Debug)]
pub struct Var {
    pub name: String,
    pub ty: TypeVarRef,
}

#[derive(Clone)]
pub struct WeakBlockRef(Weak<RefCell<Block>>);

#[derive(Clone)]
pub struct StrongBlockRef(Rc<RefCell<Block>>);

impl PartialEq for StrongBlockRef {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl PartialEq for WeakBlockRef {
    fn eq(&self, other: &Self) -> bool {
        Weak::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for StrongBlockRef {}
impl Eq for WeakBlockRef {}

impl hash::Hash for WeakBlockRef {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0.as_ptr() as usize)
    }
}

impl hash::Hash for StrongBlockRef {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0.as_ptr() as usize)
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
    pub fn new(name: String) -> Func {
        let mut blocks = vec![];
        let block = StrongBlockRef(Rc::new(RefCell::new(Block::new())));
        blocks.push(block.clone());
        Func {
            name,
            blocks,
            entry: block.downgrade(),
            params: vec![],
        }
    }
    pub fn new_block(&mut self) -> WeakBlockRef {
        let block = StrongBlockRef(Rc::new(RefCell::new(Block::new())));
        self.blocks.push(block.clone());
        block.downgrade()
    }
    fn get_block_num(&self, block: &WeakBlockRef) -> usize {
        self.blocks
            .iter()
            .position(|b| &b.downgrade() == block)
            .unwrap()
    }
}

impl StrongBlockRef {
    pub fn downgrade(&self) -> WeakBlockRef {
        WeakBlockRef(Rc::downgrade(&self.0))
    }
    pub fn get(&self) -> Ref<Block> {
        self.0.borrow()
    }
    pub fn get_mut(&self) -> RefMut<Block> {
        self.0.borrow_mut()
    }
}

impl WeakBlockRef {
    pub fn upgrade(&self) -> StrongBlockRef {
        StrongBlockRef(self.0.upgrade().unwrap())
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Decl(var) => write!(f, "var {}: {}", var.name, var.ty),
            Stmt::Drop(var) => write!(f, "drop {}", var.name),
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
            Expr::Var(var) => write!(f, "{}", var.name),
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
            RefExpr::Var(var) => write!(f, "{}", var.name),
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

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "func {}(", self.name)?;
        let mut param_iter = self.params.iter();
        if let Some(param) = param_iter.next() {
            write!(f, "{}: {}", param.name, param.ty)?;
            for param in param_iter.next() {
                write!(f, ", {}: {}", param.name, param.ty)?;
            }
        }
        writeln!(f, ")")?;
        for block in &self.blocks {
            writeln!(f, "b{}:", self.get_block_num(&block.downgrade()))?;
            for stmt in &block.get().stmts {
                writeln!(f, "  {}", stmt)?;
            }
            match &block.get().branch {
                Branch::Static(block) => {
                    writeln!(f, "  goto b{}", self.get_block_num(block))
                }
                Branch::Cond {
                    if_true,
                    if_false,
                    cond,
                } => {
                    let if_true_num = self.get_block_num(&if_true);
                    let if_false_num = self.get_block_num(&if_false);
                    writeln!(
                        f,
                        "  if {} goto b{} else goto b{}",
                        cond, if_true_num, if_false_num
                    )
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
                    writeln!(f, "  goto b{}", self.get_block_num(&return_to))
                }
            }?;
        }
        Ok(())
    }
}
