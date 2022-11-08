use std::collections::{HashMap, HashSet};

use crate::{ir, ty::Type};

#[derive(Debug, Clone, PartialEq)]
pub enum Region {
    Local(bool),
    Ptr(HashSet<RegionId>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RegionId(usize);

fn create_region(ty: &Type) -> Region {
    match ty {
        Type::Int(_) | Type::Bool => Region::Local(false),
        Type::Ptr(_) => Region::Ptr(HashSet::new()),
        Type::AnyInt | Type::Any => panic!(),
    }
}

fn create_region_param(ty: &Type, regions: &mut RegionData) -> RegionId {
    let region = match ty {
        Type::Int(_) | Type::Bool => Region::Local(true),
        Type::Ptr(r) => {
            let t = r.apply(|r| create_region_param(r, regions));
            Region::Ptr(HashSet::from_iter(Some(t)))
        }
        Type::AnyInt | Type::Any => panic!(),
    };
    regions.new_region(region)
}

#[derive(Debug, Clone)]
pub struct RegionData {
    regions: Vec<Region>,
}

impl Region {
    fn combine(&self, other: &Region) -> Region {
        match (self, other) {
            (Region::Ptr(a), Region::Ptr(b)) => {
                let mut c = a.clone();
                c.extend(b);
                Region::Ptr(c)
            }
            (Region::Local(a), Region::Local(b)) => Region::Local(*a && *b),
            _ => panic!(),
        }
    }
    fn update(&mut self, other: &Region) -> bool {
        match (self, other) {
            (Region::Ptr(a), Region::Ptr(b)) => {
                let mut updated = false;
                for r in b.iter() {
                    updated = updated || a.insert(*r);
                }
                updated
            }
            (Region::Local(a), Region::Local(b)) => {
                let res = *a && !*b;
                *a = *a && *b;
                res
            }
            _ => panic!(),
        }
    }
}

impl RegionData {
    pub fn new() -> RegionData {
        RegionData { regions: vec![] }
    }
    pub fn get_region_mut(&mut self, id: RegionId) -> &mut Region {
        &mut self.regions[id.0]
    }
    pub fn get_region(&self, id: RegionId) -> &Region {
        &self.regions[id.0]
    }
    pub fn new_region(&mut self, region: Region) -> RegionId {
        let id = self.regions.len();
        self.regions.push(region);
        RegionId(id)
    }
    pub fn update(&mut self, other: &RegionData) -> bool {
        let mut updated = false;
        for (a, b) in self.regions.iter_mut().zip(&other.regions) {
            updated = updated || a.update(b);
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
    map: HashMap<ir::BlockRef, RegionData>,
}

pub fn check_func(func: &ir::Func) {
    let mut scope = HashMap::new();

    let mut state = State {
        map: HashMap::new(),
        queue: vec![],
    };

    let mut initial_regions = RegionData::new();

    for param in &func.params {
        scope.insert(
            param.clone(),
            param
                .ty()
                .apply(|ty| create_region_param(ty, &mut initial_regions)),
        );
    }

    state.queue.push(func.entry);
    state.map.insert(func.entry, initial_regions);

    loop {
        let block = match state.queue.pop() {
            Some(block) => block,
            None => break,
        };
        let new_regions =
            propagate_regions(block, state.map.get(&block).unwrap(), &mut scope, func);
        println!("{:?}", new_regions);

        match &func.get_block(block).branch {
            ir::Branch::Static(target) => {
                queue_if_changed(*target, new_regions, &mut state);
            }
            ir::Branch::Cond {
                if_true, if_false, ..
            } => {
                queue_if_changed(*if_true, new_regions.clone(), &mut state);
                queue_if_changed(*if_false, new_regions, &mut state);
            }
            ir::Branch::Return(_) => (),
            ir::Branch::Call { .. } => todo!(),
        }
    }
}

fn queue_if_changed(block: ir::BlockRef, new_regions: RegionData, state: &mut State) {
    state
        .map
        .entry(block)
        .and_modify(|old_regions| {
            if old_regions.update(&new_regions) {
                state.queue.push(block);
            }
        })
        .or_insert_with(|| {
            state.queue.push(block);
            new_regions
        });
}

pub fn propagate_regions(
    block: ir::BlockRef,
    initial_regions: &RegionData,
    scope: &mut HashMap<ir::VarRef, RegionId>,
    func: &ir::Func,
) -> RegionData {
    let mut regions = initial_regions.clone();
    for stmt in &func.get_block(block).stmts {
        match stmt {
            ir::Stmt::Decl(var) => {
                let region = regions.new_region(var.ty().apply(create_region));
                scope.insert(var.clone(), region);
            }
            ir::Stmt::Drop(_) => (),
            ir::Stmt::Assign { ref_expr, expr } => {
                let region = get_expr_region(expr, &regions, scope);
                assign_region_to(ref_expr, region, &mut regions, scope);
            }
        }
    }
    regions
}

fn assign_region_to<'f>(
    ref_expr: &ir::Expr,
    region: Region,
    regions: &mut RegionData,
    scope: &mut HashMap<ir::VarRef, RegionId>,
) {
    match ref_expr {
        ir::Expr::Bool(_) | ir::Expr::Int(_) | ir::Expr::Infix { .. } => panic!(),
        ir::Expr::Ref(ref_expr) => match ref_expr {
            ir::RefExpr::Var(var) => {
                let region_id = *scope.get(var).unwrap();
                *regions.get_region_mut(region_id) = region.clone();
            }
        },
        ir::Expr::Deref(ref_expr) => match region {
            Region::Local(_) => panic!(),
            Region::Ptr(ptr_regions) => {
                for region in ptr_regions {
                    assign_region_to(
                        &*ref_expr,
                        regions.get_region(region).clone(),
                        regions,
                        scope,
                    )
                }
            }
        },
        ir::Expr::Var(var) => match regions.get_region(*scope.get(&var).unwrap()) {
            Region::Local(_) => panic!(),
            Region::Ptr(dst_regions) => {
                for dst_region in dst_regions.clone() {
                    *regions.get_region_mut(dst_region) = region.clone();
                }
            }
        },
        ir::Expr::Returned => todo!(),
    }
}

fn get_expr_region<'f>(
    expr: &ir::Expr,
    regions: &RegionData,
    scope: &mut HashMap<ir::VarRef, RegionId>,
) -> Region {
    match expr {
        ir::Expr::Infix { left, right, .. } => {
            let left = get_expr_region(left, regions, scope);
            let right = get_expr_region(right, regions, scope);
            match (left, right) {
                (Region::Local(a), Region::Local(b)) => {
                    if a && b {
                        Region::Local(true)
                    } else {
                        println!("invalid operation!");
                        Region::Local(false)
                    }
                }
                _ => panic!(),
            }
        }
        ir::Expr::Bool(_) | ir::Expr::Int(_) => Region::Local(true),
        ir::Expr::Ref(ref_expr) => match ref_expr {
            ir::RefExpr::Var(var) => {
                let region = scope.get(&var).unwrap();
                Region::Ptr(HashSet::from_iter(Some(*region)))
            }
        },
        ir::Expr::Deref(expr) => match get_expr_region(expr, regions, scope) {
            Region::Local(_) => panic!(),
            Region::Ptr(ptr_regions) => ptr_regions
                .iter()
                .map(|id| regions.get_region(*id))
                .cloned()
                .reduce(|a, b| a.combine(&b))
                .unwrap(),
        },
        ir::Expr::Var(var) => regions.get_region(*scope.get(&var).unwrap()).clone(),
        ir::Expr::Returned => todo!(),
    }
}

// #[cfg(test)]
// mod tests {
//     use std::collections::HashMap;

//     use crate::{
//         ast::{IntSize, IntType},
//         ir::{Expr, Func, RefExpr, Stmt, Variable},
//         ty::Type,
//     };

//     use super::{propagate_regions, Regions};

//     const I32: IntType = IntType {
//         size: IntSize::B32,
//         signed: true,
//     };

//     #[test]
//     fn test_region() {
//         let func = Func::new();
//         let block = func.alloc_block();

//         let i32 = func.types.alloc_type_var(Type::Int(I32));
//         let ptr_i32 = func.types.alloc_type_var(Type::Ptr(i32));

//         let x = func.alloc_var(Variable {
//             name: "x".to_string(),
//             ty: i32,
//         });
//         let y = func.alloc_var(Variable {
//             name: "y".to_string(),
//             ty: ptr_i32,
//         });
//         block.get_mut().stmts.extend(
//             [
//                 Stmt::Decl(x),
//                 Stmt::Decl(y),
//                 Stmt::Assign {
//                     ref_expr: Expr::Ref(RefExpr::Var(x)),
//                     expr: Expr::Int(5),
//                 },
//             ]
//             .iter()
//             .cloned(),
//         );
//         let regions = Regions {
//             regions: vec![],
//             scope: HashMap::new(),
//         };
//         check_block(block, &regions);
//     }
// }
