use std::{
    cell::{Ref, RefCell},
    collections::{HashMap, VecDeque},
    fmt, hash, ptr,
};

use typed_arena::Arena;

use crate::{ast, idents::Ident};

#[derive(Debug, Clone)]
pub struct Func<'b, 's> {
    pub entry: BlockRef<'b, 's>,
}

#[derive(Debug, Clone)]
pub struct Block<'b, 's> {
    stmts: Vec<Stmt<'s>>,
    branch: Branch<'b, 's>,
}

#[derive(Debug, Clone, Copy)]
pub struct BlockRef<'b, 's>(&'b RefCell<Block<'b, 's>>);

#[derive(Debug, Clone)]
pub enum Stmt<'s> {
    Decl {
        name: Ident<'s>,
        ty: Option<ast::Type>,
        expr: Option<ast::Expr<'s>>,
    },
    Assign {
        ref_expr: ast::RefExpr<'s>,
        expr: ast::Expr<'s>,
    },
    DerefAssign {
        ptr: ast::Expr<'s>,
        expr: ast::Expr<'s>,
    },
    Drop(Ident<'s>),
}

#[derive(Debug, Clone)]
pub enum Branch<'b, 's> {
    Static(BlockRef<'b, 's>),
    Cond {
        cond: ast::Expr<'s>,
        if_true: BlockRef<'b, 's>,
        if_false: BlockRef<'b, 's>,
    },
    Return(Option<ast::Expr<'s>>),
}

struct State<'b, 's> {
    current_block: BlockRef<'b, 's>,
    arena: &'b Arena<RefCell<Block<'b, 's>>>,
}

impl<'b, 's> PartialEq for BlockRef<'b, 's> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl<'b, 's> Eq for BlockRef<'b, 's> {}

impl<'b, 's> hash::Hash for BlockRef<'b, 's> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0 as *const _ as usize)
    }
}

impl<'b, 's> BlockRef<'b, 's> {
    pub fn new(arena: &'b Arena<RefCell<Block<'b, 's>>>) -> BlockRef<'b, 's> {
        BlockRef(arena.alloc(RefCell::new(Block {
            stmts: vec![],
            branch: Branch::Return(None),
        })))
    }
    pub fn append_stmt(&self, stmt: Stmt<'s>) {
        self.0.borrow_mut().stmts.push(stmt)
    }
    pub fn set_branch(&self, branch: Branch<'b, 's>) {
        self.0.borrow_mut().branch = branch;
    }
    pub fn get_block(&self) -> Ref<'_, Block<'b, 's>> {
        self.0.borrow()
    }
}

impl<'b, 's> State<'b, 's> {
    fn compile_if(&mut self, if_stmt: ast::If<'s>) {
        let if_block = BlockRef::new(self.arena);
        let else_block = BlockRef::new(self.arena);
        let exit_block = BlockRef::new(self.arena);
        self.current_block.0.borrow_mut().branch = Branch::Cond {
            cond: if_stmt.cond,
            if_true: if_block,
            if_false: else_block,
        };

        self.current_block = if_block;
        self.compile_block(if_stmt.if_block);
        self.current_block.set_branch(Branch::Static(exit_block));

        self.current_block = else_block;
        match if_stmt.else_block {
            ast::Else::Block(block) => self.compile_block(block),
            ast::Else::If(if_stmt) => self.compile_if(*if_stmt),
            ast::Else::None => (),
        }
        self.current_block.set_branch(Branch::Static(exit_block));

        self.current_block = exit_block;
    }
    fn compile_block(&mut self, block: ast::Block<'s>) {
        let mut decls = vec![];

        for stmt in block.0 {
            match stmt {
                ast::Stmt::If(if_stmt) => self.compile_if(if_stmt),
                ast::Stmt::While { cond, block } => {
                    let cond_block = BlockRef::new(self.arena);
                    let loop_block = BlockRef::new(self.arena);
                    let exit_block = BlockRef::new(self.arena);

                    self.current_block.set_branch(Branch::Static(cond_block));

                    cond_block.set_branch(Branch::Cond {
                        cond,
                        if_true: loop_block,
                        if_false: exit_block,
                    });

                    self.current_block = loop_block;
                    self.compile_block(block);
                    self.current_block.set_branch(Branch::Static(cond_block));

                    self.current_block = exit_block;
                }
                ast::Stmt::Return(expr) => {
                    self.current_block.set_branch(Branch::Return(expr));
                }

                ast::Stmt::Decl { name, ty, expr } => {
                    self.current_block
                        .append_stmt(Stmt::Decl { name, ty, expr });
                    decls.push(name);
                }
                ast::Stmt::Assign { ref_expr, expr } => self
                    .current_block
                    .append_stmt(Stmt::Assign { ref_expr, expr }),

                ast::Stmt::DerefAssign { ptr, expr } => self
                    .current_block
                    .append_stmt(Stmt::DerefAssign { ptr, expr }),
            }
        }
        for name in decls {
            self.current_block.append_stmt(Stmt::Drop(name))
        }
    }
}

pub fn create_cfg<'b, 's>(
    func: ast::Func<'s>,
    arena: &'b Arena<RefCell<Block<'b, 's>>>,
) -> Option<Func<'b, 's>> {
    let entry = BlockRef::new(arena);
    let mut state = State {
        current_block: entry,
        arena,
    };
    state.compile_block(func.block?);
    Some(Func { entry })
}

struct FuncFormatter<'b, 's, 'f, 'f1> {
    numbering: HashMap<BlockRef<'b, 's>, u32>,
    queue: VecDeque<BlockRef<'b, 's>>,
    current_number: u32,
    f: &'f mut fmt::Formatter<'f1>,
}

impl<'b, 's, 'f, 'f1> FuncFormatter<'b, 's, 'f, 'f1> {
    fn format_block(&mut self, block: BlockRef<'b, 's>) -> fmt::Result {
        writeln!(self.f, "b{}:", self.numbering.get(&block).unwrap())?;
        for stmt in &block.get_block().stmts {
            writeln!(self.f, "  {}", stmt)?;
        }
        match &block.get_block().branch {
            Branch::Static(block) => {
                let num = self.get_block_num(*block);
                writeln!(self.f, "  goto b{};", num)
            }
            Branch::Cond {
                if_true,
                if_false,
                cond,
            } => {
                let if_true_num = self.get_block_num(*if_true);
                let if_false_num = self.get_block_num(*if_false);
                writeln!(
                    self.f,
                    "  if {} goto b{} else goto b{};",
                    cond, if_true_num, if_false_num
                )
            }
            Branch::Return(Some(expr)) => writeln!(self.f, "  return {};", expr),
            Branch::Return(None) => writeln!(self.f, "  return;"),
        }
    }
    fn get_block_num(&mut self, block: BlockRef<'b, 's>) -> u32 {
        let number = self.numbering.entry(block).or_insert_with(|| {
            self.current_number += 1;
            self.queue.push_front(block);
            self.current_number - 1
        });
        *number
    }
}

impl<'b, 's> fmt::Display for Func<'b, 's> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ff = FuncFormatter {
            current_number: 1,
            f,
            numbering: HashMap::new(),
            queue: VecDeque::new(),
        };
        ff.get_block_num(self.entry);
        while let Some(block) = ff.queue.pop_back() {
            ff.format_block(block)?;
        }
        Ok(())
    }
}

impl<'s> fmt::Display for Stmt<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Decl { name, ty, expr } => {
                write!(f, "var {}", name.as_str())?;
                if let Some(ty) = ty {
                    write!(f, ": {}", ty)?;
                }
                if let Some(expr) = expr {
                    write!(f, " = {}", expr)?;
                }
                write!(f, ";")
            }
            Stmt::Assign { ref_expr, expr } => {
                write!(f, "{} = {};", ref_expr, expr)
            }
            Stmt::DerefAssign { ptr, expr } => {
                write!(f, "{} = {};", ptr, expr)
            }
            Stmt::Drop(ident) => {
                write!(f, "drop {};", ident.as_str())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use typed_arena::Arena;

    use crate::{ast::{Func, Block, Stmt, If, Expr, Else}, idents::Ident};

    use super::{create_cfg, Branch};

    #[test]
    fn test_if_cfg() {
        let func = Func {
            name: Ident::new("f"),
            params: vec![],
            returns: None,
            block: Some(Block(vec![
                Stmt::If(If {
                    cond: Expr::ident("x"),
                    if_block: Block::empty(),
                    else_block: Else::None,
                })
            ])),
        };
        let arena = Arena::new();
        let cfg_func = create_cfg(func, &arena).unwrap();
        let entry = cfg_func.entry.get_block();
        match entry.branch {
            Branch::Cond { if_true, if_false, .. } => {
                let if_true_target = match if_true.get_block().branch {
                    Branch::Static(target) => target,
                    _ => panic!(),
                };
                let if_false_target = match if_false.get_block().branch {
                    Branch::Static(target) => target,
                    _ => panic!(),
                };
                assert_eq!(if_true_target, if_false_target);
            }
            _ => panic!(),
        }

    }
}
