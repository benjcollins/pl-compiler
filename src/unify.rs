use std::{
    cell::RefCell,
    fmt::{self, Display},
    ptr,
};

use typed_arena::Arena;

#[derive(Debug, Clone, PartialEq)]
pub enum UnifyVar<'v, T> {
    Equal(UnifyVarRef<'v, T>),
    Is(T),
}

#[derive(Debug, PartialEq)]
pub struct UnifyVarRef<'v, T>(&'v RefCell<UnifyVar<'v, T>>);

pub struct UnifyVars<'v, T> {
    arena: Arena<RefCell<UnifyVar<'v, T>>>,
}

impl<'v, T: Unify<'v> + Clone + fmt::Debug> UnifyVars<'v, T> {
    pub fn new() -> UnifyVars<'v, T> {
        UnifyVars {
            arena: Arena::new(),
        }
    }
    pub fn alloc_type_var(&'v self, ty: T) -> UnifyVarRef<'v, T> {
        UnifyVarRef(self.arena.alloc(RefCell::new(UnifyVar::Is(ty))))
    }
    pub fn unify(&'v self, a: UnifyVarRef<'v, T>, b: UnifyVarRef<'v, T>) -> UnifyVarRef<'v, T> {
        if a.peek(|a| b.peek(|b| ptr::eq(a, b))) {
            return a;
        }
        a.map(|a| b.map(|b| self.alloc_type_var(a.unify(b, self).unwrap())))
    }
}

impl<'v, T> Clone for UnifyVarRef<'v, T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<'v, T> Copy for UnifyVarRef<'v, T> {}

pub trait Unify<'v>
where
    Self: Sized,
{
    fn unify(&self, other: &Self, types: &'v UnifyVars<Self>) -> Result<Self, (Self, Self)>;
}

impl<'v, T: Unify<'v> + Clone> UnifyVarRef<'v, T> {
    pub fn map(&self, f: impl Fn(&T) -> UnifyVarRef<'v, T>) -> UnifyVarRef<'v, T> {
        let unify_var_ref = match &*self.0.borrow() {
            UnifyVar::Equal(var) => var.map(f),
            UnifyVar::Is(ty) => f(ty),
        };
        *self.0.borrow_mut() = UnifyVar::Equal(unify_var_ref);
        unify_var_ref
    }
    pub fn peek<U>(&self, mut f: impl FnMut(&T) -> U) -> U {
        match &*self.0.borrow() {
            UnifyVar::Equal(var) => var.peek(f),
            UnifyVar::Is(ty) => f(ty),
        }
    }
    pub fn get_ty(&self) -> T {
        self.peek(|a| a.clone())
    }
}

impl<'v, T: Unify<'v> + Display + Clone> fmt::Display for UnifyVarRef<'v, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.peek(|ty| write!(f, "{}", ty))
    }
}
