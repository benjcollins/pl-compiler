use std::fmt;

use crate::{
    ast::IntType,
    unify::{Unify, UnifyVarRef},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int(IntType),
    Bool,
    Ptr(TypeVarRef),
    AnyInt,
    Any,
}

impl Unify for Type {
    fn unify(&self, other: &Self) -> Option<Type> {
        match (self, other) {
            (Type::Any, a) | (a, Type::Any) => Some(a.clone()),
            (Type::Bool, Type::Bool) => Some(Type::Bool),
            (Type::Ptr(a), Type::Ptr(b)) => {
                a.unify_var(b).map_or(None, |ty| Some(ty))?;
                Some(Type::Ptr(a.clone()))
            }
            (Type::Int(a), Type::Int(b)) if a == b => Some(Type::Int(*a)),
            (Type::AnyInt, Type::Int(a)) | (Type::Int(a), Type::AnyInt) => Some(Type::Int(*a)),
            (Type::AnyInt, Type::AnyInt) => Some(Type::AnyInt),
            _ => None,
        }
    }
}

impl fmt::Display for Type {
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

pub type TypeVarRef = UnifyVarRef<Type>;

#[cfg(test)]
mod tests {
    use crate::{
        ast::{IntSize, IntType},
        ty::TypeVarRef,
    };

    use super::Type;

    const I32: IntType = IntType {
        size: IntSize::B32,
        signed: true,
    };

    #[test]
    fn test_unify_int() {
        let a = TypeVarRef::new(Type::Any);
        let b = TypeVarRef::new(Type::Int(I32));
        a.unify_var(&b).unwrap();
        assert_eq!(a, b);
    }
    #[test]
    fn test_unify_ref_bool() {
        let a = TypeVarRef::new(Type::Ptr(TypeVarRef::new(Type::Bool)));
        let b = TypeVarRef::new(Type::Ptr(TypeVarRef::new(Type::Any)));
        a.unify_var(&b).unwrap();
        assert_eq!(a, b);
    }
}
