use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

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

#[derive(Debug, Clone)]
pub struct Regions<'f> {
    regions: Vec<Region>,
    scope: HashMap<&'f str, RegionId>,
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
}

impl<'f> Regions<'f> {
    pub fn new() -> Regions<'f> {
        Regions {
            regions: vec![],
            scope: HashMap::new(),
        }
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
}

pub fn check_func<'f>(func: &'f ir::Func<'f>, entry: ir::BlockRef<'f>) {
    let mut map = HashMap::new();
    let mut queue = vec![];
    queue.push(entry);
    map.insert(entry, Regions::new());
    loop {
        let block = match queue.pop() {
            Some(block) => block,
            None => break,
        };
        let new_regions = propagate_regions(block, map.get(&block).unwrap());
        println!("{:?}", new_regions);
        for target in block.get().branch.iter_branch_targets() {
            if map
                .get(&target)
                .map_or(true, |past| past.regions != new_regions.regions)
            {
                queue.push(target);
                map.insert(
                    target,
                    match map.get(&target) {
                        Some(last_regions) => {
                            if last_regions.regions.len() != new_regions.regions.len() {
                                panic!()
                            }
                            Regions {
                                scope: new_regions.scope.clone(),
                                regions: last_regions
                                    .regions
                                    .iter()
                                    .zip(new_regions.regions.iter())
                                    .map(|(last, new)| last.combine(new))
                                    .collect(),
                            }
                        }
                        None => new_regions.clone(),
                    },
                );
            }
        }
    }
}

pub fn propagate_regions<'f>(
    block: ir::BlockRef<'f>,
    initial_regions: &Regions<'f>,
) -> Regions<'f> {
    let mut regions: Regions = initial_regions.clone();
    for stmt in &block.get().stmts {
        match stmt {
            ir::Stmt::Decl(var) => {
                let region = regions.new_region(var.ty.apply(create_region));
                regions.scope.insert(var.name.as_str(), region);
            }
            ir::Stmt::Drop(var) => {
                regions.scope.remove(var.name.as_str());
            }
            ir::Stmt::Assign { ref_expr, expr } => {
                let region = get_expr_region(expr, &regions);
                assign_region_to(ref_expr, region, &mut regions);
            }
        }
    }
    regions
}

fn assign_region_to<'f>(ref_expr: &ir::Expr<'f>, region: Region, regions: &mut Regions) {
    match ref_expr {
        ir::Expr::Bool(_) | ir::Expr::Int(_) | ir::Expr::Infix { .. } => panic!(),
        ir::Expr::Ref(ref_expr) => match ref_expr {
            ir::RefExpr::Var(var) => {
                let region_id = *regions.scope.get(var.name.as_str()).unwrap();
                *regions.get_region_mut(region_id) = region.clone();
            }
        },
        ir::Expr::Deref(ref_expr) => match region {
            Region::Local(_) => panic!(),
            Region::Ptr(ptr_regions) => {
                for region in ptr_regions {
                    assign_region_to(&*ref_expr, regions.get_region(region).clone(), regions)
                }
            }
        },
        ir::Expr::Var(var) => {
            match regions.get_region(*regions.scope.get(var.name.as_str()).unwrap()) {
                Region::Local(_) => panic!(),
                Region::Ptr(dst_regions) => {
                    for dst_region in dst_regions.clone() {
                        *regions.get_region_mut(dst_region) = region.clone();
                    }
                }
            }
        }
    }
}

fn get_expr_region<'f>(expr: &ir::Expr<'f>, regions: &Regions) -> Region {
    match expr {
        ir::Expr::Infix { left, right, .. } => {
            let left = get_expr_region(left, regions);
            let right = get_expr_region(right, regions);
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
                let region = regions.scope.get(var.name.as_str()).unwrap();
                Region::Ptr(HashSet::from_iter(Some(*region)))
            }
        },
        ir::Expr::Deref(expr) => match get_expr_region(expr, regions) {
            Region::Local(_) => panic!(),
            Region::Ptr(ptr_regions) => ptr_regions
                .iter()
                .map(|id| regions.get_region(*id))
                .cloned()
                .reduce(|a, b| a.combine(&b))
                .unwrap(),
        },
        ir::Expr::Var(var) => regions
            .get_region(*regions.scope.get(var.name.as_str()).unwrap())
            .clone(),
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
