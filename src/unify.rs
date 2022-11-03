use std::{cell::RefCell, fmt, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub enum UnifyVar<T> {
    Equal(UnifyVarRef<T>),
    Is(T),
}

#[derive(Debug, PartialEq)]
pub struct UnifyVarRef<T>(Rc<RefCell<UnifyVar<T>>>);

impl<T> Clone for UnifyVarRef<T> {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

pub trait Unify
where
    Self: Sized,
{
    fn unify(&self, other: &Self) -> Result<Self, (Self, Self)>;
}

impl<T: Unify + Clone + fmt::Debug> UnifyVarRef<T> {
    pub fn apply<U>(&self, mut f: impl FnMut(&T) -> U) -> U {
        match &*self.0.borrow() {
            UnifyVar::Equal(var) => var.apply(f),
            UnifyVar::Is(ty) => f(ty),
        }
    }
    pub fn get_ty(&self) -> T {
        self.apply(|a| a.clone())
    }
    pub fn new(ty: T) -> UnifyVarRef<T> {
        UnifyVarRef(Rc::new(RefCell::new(UnifyVar::Is(ty))))
    }
    pub fn unify_var(&self, other: &UnifyVarRef<T>) {
        let ty = match (&*self.0.borrow(), &*other.0.borrow()) {
            (UnifyVar::Equal(ty_var), _) => {
                ty_var.unify_var(other);
                return;
            }
            (_, UnifyVar::Equal(ty_var)) => {
                ty_var.unify_var(self);
                return;
            }
            (UnifyVar::Is(a_ty), UnifyVar::Is(b_ty)) => {
                if Rc::ptr_eq(&self.0, &other.0) {
                    return;
                }
                a_ty.unify(b_ty).unwrap()
            }
        };
        let var = UnifyVarRef::new(ty);
        *self.0.borrow_mut() = UnifyVar::Equal(var.clone());
        *other.0.borrow_mut() = UnifyVar::Equal(var);
    }
    pub fn unify_ty(&self, new_ty: T) {
        match &mut *self.0.borrow_mut() {
            UnifyVar::Equal(ty_var) => ty_var.unify_ty(new_ty),
            UnifyVar::Is(ty) => *ty = new_ty,
        }
    }
}

impl<T: Unify + fmt::Display + Clone + fmt::Debug> fmt::Display for UnifyVarRef<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.apply(|ty| write!(f, "{}", ty))
    }
}
