use std::collections::{HashMap, HashSet};

use crate::{ir, refr::Refr, ty::Type};

#[derive(Debug, Clone)]
pub enum MemType {
    Local(bool),
    Ptr(HashSet<Refr<MemType>>),
}

#[derive(Debug, Clone)]
pub struct Scope {
    map: HashMap<ir::VarRef, Refr<MemType>>,
}

impl MemType {
    fn ptr(ty: Refr<MemType>) -> MemType {
        let mut set = HashSet::new();
        set.insert(ty);
        MemType::Ptr(set)
    }
    fn from_type(ty: &Type) -> MemType {
        match ty {
            Type::Int(_) | Type::Bool => MemType::Local(false),
            Type::Ptr(_) => MemType::Ptr(HashSet::new()),
            Type::AnyInt | Type::Any => panic!("type could not be infered"),
        }
    }
    fn update(&mut self, other: &MemType) -> bool {
        match (self, other) {
            (MemType::Ptr(a), MemType::Ptr(b)) => {
                let mut updated = false;
                for r in b.iter() {
                    updated = updated || a.insert(r.clone());
                }
                updated
            }
            (MemType::Local(a), MemType::Local(b)) => {
                let res = *a && !*b;
                *a = *a && *b;
                res
            }
            _ => panic!("incorrectly typed program"),
        }
    }
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            map: HashMap::new(),
        }
    }
    pub fn update(&self, other: &Scope) -> bool {
        let mut updated = false;
        for (v, t) in other.map.iter() {
            let ty = self
                .map
                .get(&v)
                .expect("scopes don't match for merged blocks");
            if t != ty {
                updated = updated || t.borrow_mut().update(&ty.borrow());
            }
        }
        updated
    }
}

pub fn check_program(program: &ir::Program) {
    for func in &program.funcs {
        check_func(func);
    }
}

struct State {
    queue: Vec<ir::BlockRef>,
    map: HashMap<ir::BlockRef, Scope>,
}

pub fn check_func(func: &ir::Func) {
    let mut state = State {
        map: HashMap::new(),
        queue: vec![],
    };

    let mut initial_scope = Scope::new();

    for param in &func.params {
        let ty = param.ty().apply(MemType::from_type);
        initial_scope.map.insert(param.clone(), Refr::new(ty));
    }

    state.queue.push(func.entry);
    state.map.insert(func.entry, initial_scope);

    loop {
        let block = match state.queue.pop() {
            Some(block) => block,
            None => break,
        };
        let new_scope = eval_block(block, state.map.get(&block).unwrap(), func);
        for (var, ty) in &new_scope.map {
            println!("{}: {:?}", var.name(), ty.borrow());
        }

        match &func.get_block(block).branch {
            ir::Branch::Static(target) => {
                queue_if_changed(*target, new_scope, &mut state);
            }
            ir::Branch::Cond {
                if_true, if_false, ..
            } => {
                queue_if_changed(*if_true, new_scope.clone(), &mut state);
                queue_if_changed(*if_false, new_scope, &mut state);
            }
            ir::Branch::Return(expr) => {
                if let Some(expr) = expr {
                    eval_expr(expr, &new_scope);
                }
            }
            ir::Branch::Call { .. } => todo!(),
        }
    }
}

fn queue_if_changed(block: ir::BlockRef, new_scope: Scope, state: &mut State) {
    state
        .map
        .entry(block)
        .and_modify(|old_scope| {
            if old_scope.update(&new_scope) {
                state.queue.push(block);
            }
        })
        .or_insert_with(|| {
            state.queue.push(block);
            new_scope
        });
}

pub fn eval_block(block: ir::BlockRef, initial_scope: &Scope, func: &ir::Func) -> Scope {
    let mut scope = initial_scope.clone();
    for stmt in &func.get_block(block).stmts {
        match stmt {
            ir::Stmt::Decl(var) => {
                let ty = var.ty().apply(MemType::from_type);
                scope.map.insert(var.clone(), Refr::new(ty));
            }
            ir::Stmt::Drop(_) => {
                // scope.map.remove(var);
            }
            ir::Stmt::Assign { ref_expr, expr } => {
                let src = eval_expr(expr, &scope);
                let MemType::Ptr(tys) = eval_expr(ref_expr, &scope) else {
                    panic!("badly typed program!");
                };
                for ty in tys {
                    *ty.borrow_mut() = src.clone();
                }
            }
        }
    }
    scope
}

fn eval_expr<'f>(expr: &ir::Expr, scope: &Scope) -> MemType {
    match expr {
        ir::Expr::Bool(_) | ir::Expr::Int(_) => MemType::Local(true),
        ir::Expr::Infix { left, right, .. } => {
            let left = eval_expr(left, scope);
            let right = eval_expr(right, scope);
            match (left, right) {
                (MemType::Local(true), MemType::Local(true)) => MemType::Local(true),
                (MemType::Local(_), MemType::Local(_)) => {
                    println!("Invalid operation!");
                    MemType::Local(false)
                }
                _ => panic!("incorrect types!"),
            }
        }
        ir::Expr::Ref(ref_expr) => match ref_expr {
            ir::RefExpr::Var(var) => {
                let ty = scope.map.get(&var).unwrap();
                MemType::ptr(ty.clone())
            }
        },
        ir::Expr::Deref(expr) => {
            let MemType::Ptr(tys) = eval_expr(expr, scope) else {
                panic!("badly typed program!");
            };
            let mut iter = tys.iter();
            let mut ty = iter.next().unwrap().borrow().clone();
            for t in iter {
                ty.update(&t.borrow());
            }
            ty
        }
        ir::Expr::Var(var) => scope
            .map
            .get(&var)
            .expect("undefined variable")
            .borrow()
            .clone(),
        ir::Expr::Returned => todo!(),
    }
}

// #[cfg(test)]
// mod tests {
//     use crate::{
//         ast::{IntSize, IntType},
//         ir::{Expr, Func, RefExpr, Stmt, VarRef},
//         ty::{Type, TypeVarRef},
//     };

//     use super::{eval_block, RegionData};

//     const I32: IntType = IntType {
//         size: IntSize::B32,
//         signed: true,
//     };

//     #[test]
//     fn test_region() {
//         let mut func = Func::new("test");
//         let block = func.alloc_block();

//         let i32 = TypeVarRef::new(Type::Int(I32));
//         let ptr_i32 = TypeVarRef::new(Type::Ptr(i32.clone()));

//         let x = VarRef::new("x", i32);
//         let y = VarRef::new("y", ptr_i32);

//         func.get_block_mut(block).stmts.extend(
//             [
//                 Stmt::Decl(x.clone()),
//                 Stmt::Decl(y),
//                 Stmt::Assign {
//                     ref_expr: Expr::Ref(RefExpr::Var(x)),
//                     expr: Expr::Int(5),
//                 },
//             ]
//             .iter()
//             .cloned(),
//         );
//         let regions = RegionData::new();
//         eval_block(block, &regions, &func);
//     }
// }
