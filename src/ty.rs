use std::fmt;

use crate::{
    ast::IntType,
    unify::{Unify, UnifyVarRef, UnifyVars},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'v> {
    Int(IntType),
    Bool,
    Ptr(TypeVarRef<'v>),
    AnyInt,
    Any,
}

impl<'v> Unify<'v> for Type<'v> {
    fn unify(
        &self,
        other: &Self,
        types: &'v UnifyVars<Type<'v>>,
    ) -> Result<Type<'v>, (Type<'v>, Type<'v>)> {
        match (self, other) {
            (Type::Any, a) | (a, Type::Any) => Ok(a.clone()),
            (Type::Bool, Type::Bool) => Ok(Type::Bool),
            (Type::Ptr(a), Type::Ptr(b)) => Ok(Type::Ptr(types.unify(*a, *b))),
            (Type::Int(a), Type::Int(b)) if a == b => Ok(Type::Int(*a)),
            (Type::AnyInt, Type::Int(a)) | (Type::Int(a), Type::AnyInt) => Ok(Type::Int(*a)),
            (Type::AnyInt, Type::AnyInt) => Ok(Type::AnyInt),
            (a, b) => Err((a.clone(), b.clone())),
        }
    }
}

impl fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int(ty) => write!(f, "{}", ty),
            Type::Bool => write!(f, "bool"),
            Type::Ptr(ty) => write!(f, "*{}", ty),
            Type::AnyInt => write!(f, "any-int"),
            Type::Any => write!(f, "any"),
        }
    }
}

pub type TypeVarRef<'v> = UnifyVarRef<'v, Type<'v>>;
pub type TypeVars<'v> = UnifyVars<'v, Type<'v>>;

#[cfg(test)]
mod tests {
    use crate::{
        ast::{IntSize, IntType},
        unify::UnifyVars,
    };

    use super::Type;

    const I32: IntType = IntType {
        size: IntSize::B32,
        signed: true,
    };

    #[test]
    fn test_unify_int() {
        let unify_vars = UnifyVars::new();
        let a = unify_vars.alloc_type_var(Type::Any);
        let b = unify_vars.alloc_type_var(Type::Int(I32));
        unify_vars.unify(a, b);
        assert_eq!(a.get_ty(), Type::Int(I32));
        assert_eq!(a.get_ty(), Type::Int(I32));
    }
    #[test]
    fn test_unify_ref_bool() {
        let unify_vars = UnifyVars::new();
        let a = unify_vars.alloc_type_var(Type::Ptr(unify_vars.alloc_type_var(Type::Bool)));
        let b = unify_vars.alloc_type_var(Type::Ptr(unify_vars.alloc_type_var(Type::Any)));
        unify_vars.unify(a, b);
        assert_eq!(a.get_ty(), b.get_ty());
    }
}
