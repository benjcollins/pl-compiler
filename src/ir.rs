use std::{
    cell::{Ref, RefCell, RefMut},
    collections::{HashMap, VecDeque},
    fmt, hash, ptr,
};

use typed_arena::Arena;

use crate::{
    ast::InfixOp,
    ty::{TypeVarRef, TypeVars},
    unify::UnifyVars,
};

pub struct Block<'c> {
    pub stmts: Vec<Stmt<'c>>,
    pub branch: Branch<'c>,
}

pub struct Func<'c> {
    pub blocks: Arena<RefCell<Block<'c>>>,
    pub vars: Arena<Variable<'c>>,
    pub types: TypeVars<'c>,
}

#[derive(Debug, Clone)]
pub enum Stmt<'f> {
    Decl(&'f Variable<'f>),
    Drop(&'f Variable<'f>),
    Assign { ref_expr: Expr<'f>, expr: Expr<'f> },
}

#[derive(Clone)]
pub enum Branch<'f> {
    Static(BlockRef<'f>),
    Cond {
        cond: Expr<'f>,
        if_true: BlockRef<'f>,
        if_false: BlockRef<'f>,
    },
    Return(Option<Expr<'f>>),
}

// pub enum IterBranchTargets<'f> {
//     None,
//     One
// }

// impl<'f> Iterator for IterBranchTargets<'f> {
//     type Item = BlockRef<'f>;

//     fn next(&mut self) -> Option<Self::Item> {
//         let (branch, ret) = match &self.0 {
//             Branch::Static(target) => (Branch::Return(None), Some(*target)),
//             Branch::Cond {
//                 if_true, if_false, ..
//             } => (Branch::Static(*if_false), Some(*if_true)),
//             Branch::Return(_) => (Branch::Return(None), None),
//         };
//         self.0 = branch;
//         ret
//     }
// }

// impl<'f> Branch<'f> {
//     pub fn iter_branch_targets(&self) -> IterBranchTargets<'f> {
//         IterBranchTargets(self.clone())
//     }
// }

#[derive(Debug, Clone)]
pub enum Expr<'f> {
    Bool(bool),
    Int(u32),
    Infix {
        left: Box<Expr<'f>>,
        right: Box<Expr<'f>>,
        op: InfixOp,
    },
    Ref(RefExpr<'f>),
    Deref(Box<Expr<'f>>),
    Var(&'f Variable<'f>),
}

#[derive(Debug, Clone)]
pub enum RefExpr<'f> {
    Var(&'f Variable<'f>),
}

#[derive(Debug)]
pub struct Variable<'f> {
    pub name: String,
    pub ty: TypeVarRef<'f>,
}

#[derive(Clone)]
pub struct BlockRef<'f>(&'f RefCell<Block<'f>>);

impl<'f> PartialEq for BlockRef<'f> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl<'f> Eq for BlockRef<'f> {}
impl<'f> Copy for BlockRef<'f> {}

impl<'f> hash::Hash for BlockRef<'f> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0 as *const _ as usize)
    }
}

impl<'f> Block<'f> {
    pub fn new() -> Block<'f> {
        Block {
            stmts: vec![],
            branch: Branch::Return(None),
        }
    }
}

impl<'f> Func<'f> {
    pub fn new() -> Func<'f> {
        Func {
            blocks: Arena::new(),
            vars: Arena::new(),
            types: UnifyVars::new(),
        }
    }
    pub fn alloc_block(&'f self) -> BlockRef<'f> {
        BlockRef(self.blocks.alloc(RefCell::new(Block::new())))
    }
    // pub fn alloc_var(&'f self, var: Variable<'f>) -> &'f Variable<'f> {
    //     self.vars.alloc(var)
    // }
}

impl<'f> BlockRef<'f> {
    pub fn get(&self) -> Ref<Block<'f>> {
        self.0.borrow()
    }
    pub fn get_mut(&self) -> RefMut<Block<'f>> {
        self.0.borrow_mut()
    }
}

impl fmt::Display for Stmt<'_> {
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

impl fmt::Display for Expr<'_> {
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

impl fmt::Display for RefExpr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RefExpr::Var(var) => write!(f, "{}", var.name),
        }
    }
}

struct FuncFmt<'f> {
    numbering: HashMap<BlockRef<'f>, u32>,
    queue: VecDeque<BlockRef<'f>>,
    current_number: u32,
}

impl<'f> FuncFmt<'f> {
    fn fmt_block(&mut self, f: &mut fmt::Formatter<'_>, block: BlockRef<'f>) -> fmt::Result {
        writeln!(f, "b{}:", self.numbering.get(&block).unwrap())?;
        for stmt in &block.get().stmts {
            writeln!(f, "  {}", stmt)?;
        }
        match &block.get().branch {
            Branch::Static(block) => {
                let num = self.get_block_num(*block);
                writeln!(f, "  goto b{};", num)
            }
            Branch::Cond {
                if_true,
                if_false,
                cond,
            } => {
                let if_true_num = self.get_block_num(*if_true);
                let if_false_num = self.get_block_num(*if_false);
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
    fn get_block_num(&mut self, block: BlockRef<'f>) -> u32 {
        let number = self.numbering.entry(block).or_insert_with(|| {
            self.current_number += 1;
            self.queue.push_front(block);
            self.current_number - 1
        });
        *number
    }
}

impl<'f> fmt::Display for BlockRef<'f> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fmt = FuncFmt {
            current_number: 1,
            numbering: HashMap::new(),
            queue: VecDeque::new(),
        };
        fmt.get_block_num(*self);
        while let Some(block) = fmt.queue.pop_back() {
            fmt.fmt_block(f, block)?;
        }
        Ok(())
    }
}
