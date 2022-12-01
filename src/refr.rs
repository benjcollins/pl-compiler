use std::{
    cell::{Ref, RefCell, RefMut},
    hash,
    rc::Rc,
};

#[derive(Debug, Clone)]
pub struct Refr<T>(Rc<RefCell<T>>);

impl<T> hash::Hash for Refr<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0.as_ptr() as usize)
    }
}

impl<T> PartialEq for Refr<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<T> Eq for Refr<T> {}

impl<T> Refr<T> {
    pub fn new(t: T) -> Refr<T> {
        Refr(Rc::new(RefCell::new(t)))
    }
    pub fn borrow(&self) -> Ref<T> {
        self.0.borrow()
    }
    pub fn borrow_mut(&self) -> RefMut<T> {
        self.0.borrow_mut()
    }
}
