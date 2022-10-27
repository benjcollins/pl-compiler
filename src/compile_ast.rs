use std::collections::HashMap;

use crate::{
    ast, ir,
    ty::{Type, TypeVarRef},
};

struct Compiler<'f, 's> {
    scope: HashMap<&'s str, &'f ir::Variable<'f>>,
    block: ir::BlockRef<'f>,
    func: &'f ir::Func<'f>,
}

pub fn compile_ast<'f>(func: &ast::Func, ir_func: &'f ir::Func<'f>) -> ir::BlockRef<'f> {
    let entry = ir_func.alloc_block();
    let mut compiler = Compiler {
        scope: HashMap::new(),
        block: entry,
        func: &ir_func,
    };
    compiler.compile_block(&func.block.as_ref().unwrap());
    entry
}

impl<'f, 's> Compiler<'f, 's> {
    fn compile_ast_ty(&self, ty: &ast::Type) -> TypeVarRef<'f> {
        self.func.types.alloc_type_var(match ty {
            ast::Type::Int(ty) => Type::Int(*ty),
            ast::Type::Ptr(ty) => Type::Ptr(self.compile_ast_ty(ty)),
            ast::Type::Bool => Type::Bool,
        })
    }
    pub fn compile_block(&mut self, block: &'s ast::Block) {
        let mut decls = vec![];
        for stmt in &block.0 {
            match stmt {
                ast::Stmt::Decl { name, ty, expr } => {
                    let ty_var = self.func.types.alloc_type_var(Type::Any);
                    let var = &*self.func.vars.alloc(ir::Variable {
                        name: name.clone(),
                        ty: ty_var,
                    });
                    decls.push(var);
                    self.scope.insert(&name, var);
                    self.block.get_mut().stmts.push(ir::Stmt::Decl(var));
                    if let Some(ty) = ty {
                        self.func.types.unify(ty_var, self.compile_ast_ty(&ty));
                    }
                    if let Some(expr) = expr {
                        let (expr, expr_ty) = self.compile_expr(&expr);
                        self.func.types.unify(expr_ty, ty_var);
                        self.block.get_mut().stmts.push(ir::Stmt::Assign {
                            ref_expr: ir::Expr::Ref(ir::RefExpr::Var(var)),
                            expr,
                        })
                    }
                }
                ast::Stmt::Assign { ref_expr, expr } => {
                    let (expr, expr_ty) = self.compile_expr(&expr);
                    let (ref_expr, ref_expr_ty) = self.compile_ref_expr(&ref_expr);
                    self.func.types.unify(expr_ty, ref_expr_ty);
                    self.block.get_mut().stmts.push(ir::Stmt::Assign {
                        ref_expr: ir::Expr::Ref(ref_expr),
                        expr,
                    });
                }
                ast::Stmt::DerefAssign { ref_expr, expr } => {
                    let (expr, expr_ty) = self.compile_expr(&expr);
                    let (ref_expr, ref_expr_ty) = self.compile_expr(&ref_expr);
                    let ref_ty = self.func.types.alloc_type_var(Type::Ptr(expr_ty));
                    self.func.types.unify(ref_expr_ty, ref_ty);
                    self.block
                        .get_mut()
                        .stmts
                        .push(ir::Stmt::Assign { ref_expr, expr });
                }
                ast::Stmt::If(if_stmt) => self.compile_if(if_stmt),
                ast::Stmt::While { cond, block } => {
                    let cond_block = self.func.alloc_block();
                    let loop_block = self.func.alloc_block();
                    let exit_block = self.func.alloc_block();

                    let bool_ty = self.func.types.alloc_type_var(Type::Bool);
                    let (cond, cond_ty) = self.compile_expr(&cond);
                    self.func.types.unify(cond_ty, bool_ty);

                    self.block.get_mut().branch = ir::Branch::Static(cond_block);

                    cond_block.get_mut().branch = ir::Branch::Cond {
                        cond,
                        if_true: loop_block,
                        if_false: exit_block,
                    };

                    self.block = loop_block;
                    self.compile_block(&block);
                    self.block.get_mut().branch = ir::Branch::Static(cond_block);

                    self.block = exit_block;
                }
                ast::Stmt::Return(expr) => {
                    self.block.get_mut().branch =
                        ir::Branch::Return(expr.as_ref().map(|expr| self.compile_expr(&expr).0))
                }
            }
        }
        for var in decls {
            self.block.get_mut().stmts.push(ir::Stmt::Drop(var));
            self.scope.remove(var.name.as_str());
        }
    }
    fn compile_if(&mut self, if_stmt: &'s ast::If) {
        let if_block = self.func.alloc_block();
        let else_block = self.func.alloc_block();
        let exit_block = self.func.alloc_block();

        let bool_ty = self.func.types.alloc_type_var(Type::Bool);
        let (cond, cond_ty) = self.compile_expr(&if_stmt.cond);
        self.func.types.unify(cond_ty, bool_ty);

        self.block.get_mut().branch = ir::Branch::Cond {
            cond,
            if_true: if_block,
            if_false: else_block,
        };

        self.block = if_block;
        self.compile_block(&if_stmt.if_block);
        self.block.get_mut().branch = ir::Branch::Static(exit_block);

        self.block = else_block;
        match &if_stmt.else_block {
            ast::Else::Block(block) => self.compile_block(&block),
            ast::Else::If(if_stmt) => self.compile_if(&*if_stmt),
            ast::Else::None => (),
        }
        self.block.get_mut().branch = ir::Branch::Static(exit_block);

        self.block = exit_block;
    }
    fn compile_expr(&self, expr: &ast::Expr) -> (ir::Expr<'f>, TypeVarRef<'f>) {
        match expr {
            ast::Expr::Bool(value) => (
                ir::Expr::Bool(*value),
                self.func.types.alloc_type_var(Type::Bool),
            ),
            ast::Expr::Int(value) => (
                ir::Expr::Int(*value),
                self.func.types.alloc_type_var(Type::AnyInt),
            ),
            ast::Expr::Infix { left, right, op } => {
                let (left_expr, left_ty) = self.compile_expr(left);
                let (right_expr, right_ty) = self.compile_expr(right);
                let ty = self.func.types.alloc_type_var(Type::AnyInt);
                self.func.types.unify(ty, left_ty);
                self.func.types.unify(ty, right_ty);
                let expr = ir::Expr::Infix {
                    left: Box::new(left_expr),
                    right: Box::new(right_expr),
                    op: *op,
                };
                (expr, ty)
            }
            ast::Expr::Ref(ref_expr) => {
                let (ref_expr, expr_ty) = self.compile_ref_expr(ref_expr);
                let ty = self.func.types.alloc_type_var(Type::Ptr(expr_ty));
                (ir::Expr::Ref(ref_expr), ty)
            }
            ast::Expr::Ident(name) => {
                let var = self.scope.get(name.as_str()).unwrap();
                (ir::Expr::Var(*var), var.ty)
            }
            ast::Expr::Deref(expr) => {
                let (expr, expr_ty) = self.compile_expr(expr);
                let any = self.func.types.alloc_type_var(Type::Any);
                let any_ref = self.func.types.alloc_type_var(Type::Ptr(any));
                self.func.types.unify(expr_ty, any_ref);
                (ir::Expr::Deref(Box::new(expr)), any)
            }
        }
    }
    fn compile_ref_expr(&self, ref_expr: &ast::RefExpr) -> (ir::RefExpr<'f>, TypeVarRef<'f>) {
        match ref_expr {
            ast::RefExpr::Ident(name) => {
                let var = self.scope.get(name.as_str()).unwrap();
                (ir::RefExpr::Var(*var), var.ty)
            }
        }
    }
}
