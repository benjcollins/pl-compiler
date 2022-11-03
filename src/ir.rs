use std::{
    cell::{Ref, RefCell, RefMut},
    collections::{HashMap, VecDeque},
    fmt, hash,
    rc::{Rc, Weak},
};

use crate::{ast::InfixOp, ty::TypeVarRef};

pub struct Block {
    pub stmts: Vec<Stmt>,
    pub branch: Branch,
}

pub struct Func {
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
}

#[derive(Clone)]
pub enum IterBranchTargets {
    Cond(WeakBlockRef, WeakBlockRef),
    Static(WeakBlockRef),
    None,
}

impl<'f> Iterator for IterBranchTargets {
    type Item = WeakBlockRef;

    fn next(&mut self) -> Option<Self::Item> {
        let (next, target) = match self {
            IterBranchTargets::Cond(a, b) => {
                (IterBranchTargets::Static(a.clone()), Some(b.clone()))
            }
            IterBranchTargets::Static(a) => (IterBranchTargets::None, Some(a.clone())),
            IterBranchTargets::None => (IterBranchTargets::None, None),
        };
        *self = next;
        target
    }
}

impl Branch {
    pub fn iter_branch_targets(&self) -> IterBranchTargets {
        match self {
            Branch::Static(a) => IterBranchTargets::Static(a.clone()),
            Branch::Cond {
                if_true, if_false, ..
            } => IterBranchTargets::Cond(if_true.clone(), if_false.clone()),
            Branch::Return(_) => IterBranchTargets::None,
        }
    }
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
    pub fn new() -> Func {
        let mut blocks = vec![];
        let block = StrongBlockRef(Rc::new(RefCell::new(Block::new())));
        blocks.push(block.clone());
        Func {
            blocks,
            entry: block.downgrade(),
        }
    }
    pub fn new_block(&mut self) -> WeakBlockRef {
        let block = StrongBlockRef(Rc::new(RefCell::new(Block::new())));
        self.blocks.push(block.clone());
        block.downgrade()
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
            Stmt::Decl(var) => write!(f, "decl {} : {}", var.name, var.ty),
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

struct FuncFmt {
    numbering: HashMap<WeakBlockRef, u32>,
    queue: VecDeque<WeakBlockRef>,
    current_number: u32,
}

impl FuncFmt {
    fn fmt_block(&mut self, f: &mut fmt::Formatter<'_>, block: WeakBlockRef) -> fmt::Result {
        writeln!(f, "b{}:", self.numbering.get(&block).unwrap())?;
        for stmt in &block.upgrade().get().stmts {
            writeln!(f, "  {}", stmt)?;
        }
        match &block.upgrade().get().branch {
            Branch::Static(block) => {
                let num = self.get_block_num(block.clone());
                writeln!(f, "  goto b{};", num)
            }
            Branch::Cond {
                if_true,
                if_false,
                cond,
            } => {
                let if_true_num = self.get_block_num(if_true.clone());
                let if_false_num = self.get_block_num(if_false.clone());
                writeln!(
                    f,
                    "  if {} goto b{} else goto b{}",
                    cond, if_true_num, if_false_num
                )
            }
            Branch::Return(Some(expr)) => writeln!(f, "  return {}", expr),
            Branch::Return(None) => writeln!(f, "  return"),
        }
    }
    fn get_block_num(&mut self, block: WeakBlockRef) -> u32 {
        let number = self.numbering.entry(block.clone()).or_insert_with(|| {
            self.current_number += 1;
            self.queue.push_front(block);
            self.current_number - 1
        });
        *number
    }
}

impl<'f> fmt::Display for WeakBlockRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fmt = FuncFmt {
            current_number: 1,
            numbering: HashMap::new(),
            queue: VecDeque::new(),
        };
        fmt.get_block_num(self.clone());
        while let Some(block) = fmt.queue.pop_back() {
            fmt.fmt_block(f, block)?;
        }
        Ok(())
    }
}
