use std::collections::HashMap;

use crate::{
    ast, ir,
    ty::{Type, TypeVarRef},
};

struct Compiler<'f, 's> {
    scope: HashMap<&'s str, &'f ir::Variable>,
    block: ir::BlockRef<'f>,
    func: &'f ir::Func<'f>,
}

pub fn compile_func<'f>(func: &ast::Func, ir_func: &'f ir::Func<'f>) -> ir::BlockRef<'f> {
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
    fn compile_ast_ty(&self, ty: &ast::Type) -> Type {
        match ty {
            ast::Type::Int(ty) => Type::Int(*ty),
            ast::Type::Ptr(ty) => Type::Ptr(TypeVarRef::new(self.compile_ast_ty(ty))),
            ast::Type::Bool => Type::Bool,
        }
    }
    pub fn compile_block(&mut self, block: &'s ast::Block) {
        let mut decls = vec![];
        for stmt in &block.0 {
            match stmt {
                ast::Stmt::Decl { name, ty, expr } => {
                    let ty_var = TypeVarRef::new(Type::Any);
                    let var = &*self.func.vars.alloc(ir::Variable {
                        name: name.clone(),
                        ty: ty_var.clone(),
                    });
                    decls.push(var);
                    self.scope.insert(&name, var);
                    self.block.get_mut().stmts.push(ir::Stmt::Decl(var));
                    if let Some(ty) = ty {
                        ty_var.unify_ty(self.compile_ast_ty(&ty));
                    }
                    if let Some(expr) = expr {
                        let (expr, expr_ty) = self.compile_expr(&expr);
                        expr_ty.unify_var(&ty_var);
                        self.block.get_mut().stmts.push(ir::Stmt::Assign {
                            ref_expr: ir::Expr::Ref(ir::RefExpr::Var(var)),
                            expr,
                        })
                    }
                }
                ast::Stmt::Assign { ref_expr, expr } => {
                    let (expr, expr_ty) = self.compile_expr(&expr);
                    let (ref_expr, ref_expr_ty) = self.compile_ref_expr(&ref_expr);
                    expr_ty.unify_var(&ref_expr_ty);
                    self.block.get_mut().stmts.push(ir::Stmt::Assign {
                        ref_expr: ir::Expr::Ref(ref_expr),
                        expr,
                    });
                }
                ast::Stmt::DerefAssign { ref_expr, expr } => {
                    let (expr, expr_ty) = self.compile_expr(&expr);
                    let (ref_expr, ref_expr_ty) = self.compile_expr(&ref_expr);
                    ref_expr_ty.unify_ty(Type::Ptr(expr_ty));
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

                    let (cond, cond_ty) = self.compile_expr(&cond);
                    cond_ty.unify_ty(Type::Bool);

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

        let (cond, cond_ty) = self.compile_expr(&if_stmt.cond);
        cond_ty.unify_ty(Type::Bool);

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
    fn compile_expr(&self, expr: &ast::Expr) -> (ir::Expr<'f>, TypeVarRef) {
        match expr {
            ast::Expr::Bool(value) => (ir::Expr::Bool(*value), TypeVarRef::new(Type::Bool)),
            ast::Expr::Int(value) => (ir::Expr::Int(*value), TypeVarRef::new(Type::AnyInt)),
            ast::Expr::Infix { left, right, op } => {
                let (left_expr, left_ty) = self.compile_expr(left);
                let (right_expr, right_ty) = self.compile_expr(right);
                left_ty.unify_ty(Type::AnyInt);
                right_ty.unify_var(&left_ty);
                let expr = ir::Expr::Infix {
                    left: Box::new(left_expr),
                    right: Box::new(right_expr),
                    op: *op,
                };
                (expr, left_ty)
            }
            ast::Expr::Ref(ref_expr) => {
                let (ref_expr, expr_ty) = self.compile_ref_expr(ref_expr);
                let ty = TypeVarRef::new(Type::Ptr(expr_ty));
                (ir::Expr::Ref(ref_expr), ty)
            }
            ast::Expr::Ident(name) => {
                let var = self.scope.get(name.as_str()).unwrap();
                (ir::Expr::Var(*var), var.ty.clone())
            }
            ast::Expr::Deref(expr) => {
                let (expr, expr_ty) = self.compile_expr(expr);
                let any = TypeVarRef::new(Type::Any);
                expr_ty.unify_ty(Type::Ptr(any.clone()));
                (ir::Expr::Deref(Box::new(expr)), any)
            }
            ast::Expr::Call(_) => todo!(),
        }
    }
    fn compile_ref_expr(&self, ref_expr: &ast::RefExpr) -> (ir::RefExpr<'f>, TypeVarRef) {
        match ref_expr {
            ast::RefExpr::Ident(name) => {
                let var = self.scope.get(name.as_str()).unwrap();
                (ir::RefExpr::Var(*var), var.ty.clone())
            }
        }
    }
}
