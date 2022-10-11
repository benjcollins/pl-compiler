use std::cell::RefCell;

pub enum Type {
    Int { size: IntSize, sign: Sign },
    UntypedInt,
    Bool,
    Ref(Box<Type>),
    Any,
}

pub enum IntSize {
    B64,
    B32,
    B16,
    B8,
}

pub enum Sign {
    Signed,
    Unsigned,
}

pub enum UnifyVar<'v, T> {
    Equal(&'v RefCell<UnifyVar<'v, T>>),
    Is(T),
}

impl<'v, T> UnifyVar<'v, T> {
    pub fn unify(&self, other: &mut UnifyVar<'v, T>) {}
}
