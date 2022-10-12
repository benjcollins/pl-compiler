use std::{collections::HashSet, ptr};

pub struct Idents<'s> {
    set: HashSet<&'s str>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Ident<'s>(&'s str);

impl<'s> Idents<'s> {
    pub fn new() -> Idents<'s> {
        Idents {
            set: HashSet::new(),
        }
    }
    pub fn intern(&mut self, s: &'s str) -> Ident<'s> {
        Ident(self.set.get_or_insert(s))
    }
}

impl<'s> Ident<'s> {
    pub fn new(s: &'s str) -> Ident<'s> {
        Ident(s)
    }
    pub fn as_str(&self) -> &'s str {
        self.0
    }
    pub fn fast_eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}
