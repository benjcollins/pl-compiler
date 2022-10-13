use core::fmt;
use std::{cell::RefCell, collections::HashMap};

use by_address::ByAddress;
use typed_arena::Arena;

use crate::{ast, idents::Ident};

pub struct Func<'b, 's> {
    entry: &'b RefCell<Block<'b, 's>>,
}

pub struct Block<'b, 's> {
    stmts: Vec<Stmt<'s>>,
    branch: Branch<'b, 's>,
}

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

pub enum Branch<'b, 's> {
    Static(&'b RefCell<Block<'b, 's>>),
    Cond {
        cond: ast::Expr<'s>,
        if_true: &'b RefCell<Block<'b, 's>>,
        if_false: &'b RefCell<Block<'b, 's>>,
    },
    Return(Option<ast::Expr<'s>>),
}

struct State<'b, 's> {
    current_block: &'b RefCell<Block<'b, 's>>,
    arena: &'b Arena<RefCell<Block<'b, 's>>>,
}

impl<'b, 's> State<'b, 's> {
    fn new_block(&self) -> &'b RefCell<Block<'b, 's>> {
        self.arena.alloc(RefCell::new(Block {
            stmts: vec![],
            branch: Branch::Return(None),
        }))
    }
    fn compile_if(&mut self, if_stmt: ast::If<'s>) {
        let if_block = self.new_block();
        let else_block = self.new_block();
        let exit_block = self.new_block();
        self.current_block.borrow_mut().branch = Branch::Cond {
            cond: if_stmt.cond,
            if_true: if_block,
            if_false: else_block,
        };

        self.current_block = if_block;
        self.compile_block(if_stmt.if_block);
        self.current_block.borrow_mut().branch = Branch::Static(exit_block);

        self.current_block = else_block;
        match if_stmt.else_block {
            ast::Else::Block(block) => self.compile_block(block),
            ast::Else::If(if_stmt) => self.compile_if(*if_stmt),
            ast::Else::None => (),
        }
        self.current_block.borrow_mut().branch = Branch::Static(exit_block);

        self.current_block = exit_block;
    }
    fn compile_block(&mut self, block: ast::Block<'s>) {
        let mut decls = vec![];

        for stmt in block.0 {
            match stmt {
                ast::Stmt::If(if_stmt) => self.compile_if(if_stmt),
                ast::Stmt::While { cond, block } => {
                    let cond_block = self.new_block();
                    let loop_block = self.new_block();
                    let exit_block = self.new_block();

                    self.current_block.borrow_mut().branch = Branch::Static(cond_block);

                    cond_block.borrow_mut().branch = Branch::Cond {
                        cond,
                        if_true: loop_block,
                        if_false: exit_block,
                    };

                    self.current_block = loop_block;
                    self.compile_block(block);
                    loop_block.borrow_mut().branch = Branch::Static(cond_block);

                    self.current_block = exit_block;
                }
                ast::Stmt::Return(expr) => {
                    self.current_block.borrow_mut().branch = Branch::Return(expr);
                }

                ast::Stmt::Decl { name, ty, expr } => {
                    self.current_block
                        .borrow_mut()
                        .stmts
                        .push(Stmt::Decl { name, ty, expr });
                    decls.push(name);
                }
                ast::Stmt::Assign { ref_expr, expr } => self
                    .current_block
                    .borrow_mut()
                    .stmts
                    .push(Stmt::Assign { ref_expr, expr }),

                ast::Stmt::DerefAssign { ptr, expr } => self
                    .current_block
                    .borrow_mut()
                    .stmts
                    .push(Stmt::DerefAssign { ptr, expr }),
            }
        }
        for name in decls {
            self.current_block.borrow_mut().stmts.push(Stmt::Drop(name))
        }
    }
}

struct FuncFormatter<'b, 's, 'f, 'f1> {
    numbering: HashMap<ByAddress<&'b RefCell<Block<'b, 's>>>, u32>,
    queue: Vec<&'b RefCell<Block<'b, 's>>>,
    current_number: u32,
    f: &'f mut fmt::Formatter<'f1>,
}

impl<'b, 's, 'f, 'f1> FuncFormatter<'b, 's, 'f, 'f1> {
    fn format_block(&mut self, block: &'b RefCell<Block<'b, 's>>) -> fmt::Result {
        println!("block{}:", self.numbering.get(&ByAddress(block)).unwrap());
        for stmt in &block.borrow().stmts {
            writeln!(self.f, "  {}", stmt)?;
        }
        match &block.borrow().branch {
            Branch::Static(block) => {
                let num = self.get_block_num(block);
                writeln!(self.f, "goto b{}", num)
            }
            Branch::Cond {
                if_true,
                if_false,
                cond,
            } => {
                let if_true_num = self.get_block_num(if_true);
                let if_false_num = self.get_block_num(if_false);
                writeln!(
                    self.f,
                    "if {} goto b{} else goto b{}",
                    cond, if_true_num, if_false_num
                )
            }
            Branch::Return(Some(expr)) => writeln!(self.f, "return {}", expr),
            Branch::Return(None) => writeln!(self.f, "return"),
        }
    }
    fn get_block_num(&mut self, block: &'b RefCell<Block<'b, 's>>) -> u32 {
        let number = self.numbering.entry(ByAddress(block)).or_insert_with(|| {
            self.current_number += 1;
            self.queue.push(block);
            self.current_number - 1
        });
        *number
    }
}

impl<'b, 's> fmt::Display for Func<'b, 's> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut numbering = HashMap::new();
        numbering.insert(ByAddress(self.entry), 0);
        let mut ff = FuncFormatter {
            current_number: 0,
            f,
            numbering: HashMap::new(),
            queue: vec![self.entry],
        };
        while let Some(block) = ff.queue.pop() {
            ff.format_block(block)?;
        }
        Ok(())
    }
}

impl<'s> fmt::Display for Stmt<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}
