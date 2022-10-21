use std::{
    cell::{Ref, RefCell},
    collections::{HashMap, VecDeque},
    fmt, hash, ptr,
};

use typed_arena::Arena;

pub struct Block<'b, S, E> {
    pub stmts: Vec<S>,
    pub branch: Branch<'b, S, E>,
}

pub enum Branch<'b, S, E> {
    Static(BlockRef<'b, S, E>),
    Cond {
        cond: E,
        if_true: BlockRef<'b, S, E>,
        if_false: BlockRef<'b, S, E>,
    },
    Return(Option<E>),
}

#[derive(Clone)]
pub struct BlockRef<'b, S, E>(&'b RefCell<Block<'b, S, E>>);

impl<'b, S: Clone, E: Clone> Copy for BlockRef<'b, S, E> {}

impl<'b, S, E> PartialEq for BlockRef<'b, S, E> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl<'b, S, E> Eq for BlockRef<'b, S, E> {}

impl<'b, S, E> hash::Hash for BlockRef<'b, S, E> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0 as *const _ as usize)
    }
}

impl<'b, S, E> BlockRef<'b, S, E> {
    pub fn new(arena: &'b Arena<RefCell<Block<'b, S, E>>>) -> BlockRef<'b, S, E> {
        BlockRef(arena.alloc(RefCell::new(Block {
            stmts: vec![],
            branch: Branch::Return(None),
        })))
    }
    pub fn append_stmt(&self, stmt: S) {
        self.0.borrow_mut().stmts.push(stmt)
    }
    pub fn set_branch(&self, branch: Branch<'b, S, E>) {
        self.0.borrow_mut().branch = branch;
    }
    pub fn get_block(&self) -> Ref<'_, Block<'b, S, E>> {
        self.0.borrow()
    }
}

struct FuncFormatter<'b, 'f, 'f1, S, E> {
    numbering: HashMap<BlockRef<'b, S, E>, u32>,
    queue: VecDeque<BlockRef<'b, S, E>>,
    current_number: u32,
    f: &'f mut fmt::Formatter<'f1>,
}

impl<'b, 'f, 'f1, S: fmt::Display + Clone, E: fmt::Display + Clone>
    FuncFormatter<'b, 'f, 'f1, S, E>
{
    fn format_block(&mut self, block: BlockRef<'b, S, E>) -> fmt::Result {
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
    fn get_block_num(&mut self, block: BlockRef<'b, S, E>) -> u32 {
        let number = self.numbering.entry(block).or_insert_with(|| {
            self.current_number += 1;
            self.queue.push_front(block);
            self.current_number - 1
        });
        *number
    }
}

impl<'b, S: fmt::Display + Clone, E: fmt::Display + Clone> fmt::Display for BlockRef<'b, S, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ff = FuncFormatter {
            current_number: 1,
            f,
            numbering: HashMap::new(),
            queue: VecDeque::new(),
        };
        ff.get_block_num(*self);
        while let Some(block) = ff.queue.pop_back() {
            ff.format_block(block)?;
        }
        Ok(())
    }
}
