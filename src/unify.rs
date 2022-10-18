use std::cell::RefCell;

use typed_arena::Arena;

#[derive(Debug, Clone, Copy)]
pub enum UnifyVar<'v, T> {
    Equal(UnifyVarRef<'v, T>),
    Is(T),
}

#[derive(Debug)]
pub struct UnifyVarRef<'v, T>(&'v RefCell<UnifyVar<'v, T>>);

impl<'v, T> Clone for UnifyVarRef<'v, T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<'v, T> Copy for UnifyVarRef<'v, T> {}

pub trait Unify {
    fn unify(&self, other: &Self) -> Self;
}

impl<'v, T: Unify + Clone> UnifyVarRef<'v, T> {
    pub fn new(
        var: UnifyVar<'v, T>,
        arena: &'v Arena<RefCell<UnifyVar<'v, T>>>,
    ) -> UnifyVarRef<'v, T> {
        UnifyVarRef(arena.alloc(RefCell::new(var)))
    }
    pub fn is(ty: T, arena: &'v Arena<RefCell<UnifyVar<'v, T>>>) -> UnifyVarRef<'v, T> {
        UnifyVarRef::new(UnifyVar::Is(ty), arena)
    }
    pub fn equal(
        var: UnifyVarRef<'v, T>,
        arena: &'v Arena<RefCell<UnifyVar<'v, T>>>,
    ) -> UnifyVarRef<'v, T> {
        UnifyVarRef::new(UnifyVar::Equal(var), arena)
    }
    pub fn map(&self, f: impl Fn(&T) -> UnifyVarRef<'v, T>) -> UnifyVarRef<'v, T> {
        let unify_var_ref = match &*self.0.borrow() {
            UnifyVar::Equal(var) => var.map(f),
            UnifyVar::Is(ty) => f(ty),
        };
        *self.0.borrow_mut() = UnifyVar::Equal(unify_var_ref);
        unify_var_ref
    }
    pub fn unify(
        &self,
        other: UnifyVarRef<'v, T>,
        arena: &'v Arena<RefCell<UnifyVar<'v, T>>>,
    ) -> UnifyVarRef<'v, T> {
        self.map(|a| other.map(|b| UnifyVarRef::is(a.unify(b), arena)))
    }
    pub fn peek<U>(&self, f: impl Fn(&T) -> U) -> U {
        match &*self.0.borrow() {
            UnifyVar::Equal(var) => var.peek(f),
            UnifyVar::Is(ty) => f(ty),
        }
    }
    pub fn get_ty(&self) -> T {
        self.peek(|a| a.clone())
    }
}
