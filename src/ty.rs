use crate::unify::Unify;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int(IntType),
    UntypedInt,
    Bool,
    Ref(Box<Type>),
    Any,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IntType {
    size: IntSize,
    sign: Sign,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IntSize {
    B64,
    B32,
    B16,
    B8,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Sign {
    Signed,
    Unsigned,
}

impl Unify for Type {
    fn unify(&self, other: &Self) -> Self {
        match (self, other) {
            (Type::Any, a) | (a, Type::Any) => a.clone(),
            (Type::Bool, Type::Bool) => Type::Bool,
            (Type::Ref(a), Type::Ref(b)) => Type::Ref(Box::new(a.unify(b))),
            (Type::Int(a), Type::Int(b)) if a == b => Type::Int(*a),
            (Type::UntypedInt, Type::Int(a)) | (Type::Int(a), Type::UntypedInt) => Type::Int(*a),
            _ => panic!(),
        }
    }
}

impl Type {
    pub fn as_ref(self) -> Type {
        Type::Ref(Box::new(self))
    }
}

#[cfg(test)]
mod tests {
    use typed_arena::Arena;

    use crate::unify::UnifyVarRef;

    use super::{IntSize, IntType, Sign, Type};

    const I32: IntType = IntType {
        size: IntSize::B32,
        sign: Sign::Signed,
    };

    #[test]
    fn test_unify_int() {
        let arena = Arena::new();
        let a = UnifyVarRef::is(Type::Any, &arena);
        let b = UnifyVarRef::is(Type::Int(I32), &arena);
        a.unify(b, &arena);
        assert_eq!(a.get_ty(), Type::Int(I32));
        assert_eq!(a.get_ty(), Type::Int(I32));
    }
    #[test]
    fn test_unify_ref_bool() {
        let arena = Arena::new();
        let a = UnifyVarRef::is(Type::Bool.as_ref(), &arena);
        let b = UnifyVarRef::is(Type::Any.as_ref(), &arena);
        a.unify(b, &arena);
        assert_eq!(Type::Bool.as_ref(), a.get_ty());
        assert_eq!(Type::Bool.as_ref(), b.get_ty());
    }
}
