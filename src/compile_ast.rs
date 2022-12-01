use std::collections::HashMap;

use crate::{
    ast, ir,
    ty::{Type, TypeVarRef},
};

struct Compiler<'a> {
    scope: HashMap<&'a str, ir::VarRef>,
    block: ir::BlockRef,
    func: ir::Func,
    program_ast: &'a ast::Program,
    func_ast: &'a ast::Func,
}

pub fn compile_program(program_ast: &ast::Program) -> ir::Program {
    let mut funcs = vec![];
    for func in &program_ast.funcs {
        funcs.push(compile_func(func, program_ast))
    }
    ir::Program { funcs }
}

pub fn compile_func(func_ast: &ast::Func, program_ast: &ast::Program) -> ir::Func {
    let func = ir::Func::new(func_ast.name.to_string());
    let mut compiler = Compiler {
        scope: HashMap::new(),
        block: func.entry.clone(),
        func,
        program_ast,
        func_ast,
    };
    for param in &func_ast.params {
        let ty = TypeVarRef::new(compiler.compile_ast_ty(&param.ty));
        let var = ir::VarRef::new(param.name.clone(), ty);
        compiler.scope.insert(&param.name, var.clone());
        compiler.func.params.push(var)
    }
    compiler.compile_block(&func_ast.block);
    compiler.func
}

// Types not same due to
// * assignment
// * equality test
// * not a numeric type
// * not the same numeric type
// * incorrect return type
// * function does not return
// * cannot deref non-pointer

// enum TypeError {
//     Assignment,
//     Compare,
//     NotNumeric,
//     TypesDontMatch,
//     IncorrectReturn,
//     UnexpectedReturn,
//     ExpectedPointer,
// }

impl<'s> Compiler<'s> {
    fn compile_ast_ty(&self, ty: &ast::Span<ast::Type>) -> Type {
        match &ty.ty {
            ast::Type::Int(ty) => Type::Int(*ty),
            ast::Type::Ptr(ty) => Type::Ptr(TypeVarRef::new(self.compile_ast_ty(ty))),
            ast::Type::Bool => Type::Bool,
        }
    }
    pub fn push_inst(&mut self, inst: ir::Stmt) {
        self.func.get_block_mut(self.block).stmts.push(inst);
    }
    pub fn set_branch(&mut self, branch: ir::Branch) {
        self.func.get_block_mut(self.block).branch = branch;
    }
    pub fn compile_block(&mut self, block: &'s ast::Block) {
        let mut decls = vec![];
        for stmt in &block.0 {
            match &stmt.ty {
                ast::Stmt::Decl { name, ty, expr } => {
                    let ty_var = TypeVarRef::new(match ty {
                        Some(ty) => self.compile_ast_ty(&ty),
                        None => Type::Any,
                    });
                    let var = ir::VarRef::new(name, ty_var.clone());
                    decls.push(var.clone());
                    self.scope.insert(&name, var.clone());
                    self.push_inst(ir::Stmt::Decl(var.clone()));

                    if let Some(expr) = expr {
                        let (expr, expr_ty) = self.compile_expr(&expr);
                        expr_ty
                            .unify_var(&ty_var)
                            .expect("initialiser expr has incorrect type");
                        self.push_inst(ir::Stmt::Assign {
                            ref_expr: ir::Expr::Ref(ir::RefExpr::Var(var)),
                            expr,
                        })
                    }
                }
                ast::Stmt::Assign { ref_expr, expr } => {
                    let (expr, expr_ty) = self.compile_expr(&expr);
                    let (ref_expr, ref_expr_ty) = self.compile_ref_expr(&ref_expr);
                    expr_ty
                        .unify_var(&ref_expr_ty)
                        .expect("assign to variable with wrong type");
                    self.push_inst(ir::Stmt::Assign {
                        ref_expr: ir::Expr::Ref(ref_expr),
                        expr,
                    });
                }
                ast::Stmt::DerefAssign { ref_expr, expr } => {
                    let (expr, expr_ty) = self.compile_expr(&expr);
                    let (ref_expr, ref_expr_ty) = self.compile_expr(&ref_expr);
                    ref_expr_ty
                        .unify_ty(Type::Ptr(expr_ty))
                        .expect("invalid pointer expression");
                    self.push_inst(ir::Stmt::Assign { ref_expr, expr });
                }
                ast::Stmt::If(if_stmt) => self.compile_if(if_stmt),
                ast::Stmt::While { cond, block } => {
                    let cond_block = self.func.alloc_block();
                    let loop_block = self.func.alloc_block();
                    let exit_block = self.func.alloc_block();

                    self.set_branch(ir::Branch::Static(cond_block));

                    self.block = cond_block;
                    self.compile_cond(&cond, loop_block, exit_block);

                    self.block = loop_block;
                    self.compile_block(&block);
                    self.set_branch(ir::Branch::Static(cond_block));

                    self.block = exit_block;
                }
                ast::Stmt::Return(expr) => {
                    let expr = expr.as_ref().map(|expr| {
                        let (expr, ty) = self.compile_expr(&expr);
                        ty.unify_ty(
                            self.compile_ast_ty(
                                &self
                                    .func_ast
                                    .returns
                                    .as_ref()
                                    .expect("func signature does not specify return type"),
                            ),
                        )
                        .expect("returns wrong type for function");
                        expr
                    });
                    self.set_branch(ir::Branch::Return(expr));
                }
            }
        }
        for var in decls {
            self.push_inst(ir::Stmt::Drop(var.clone()));
            self.scope.remove(var.name());
        }
    }
    fn compile_cond(
        &mut self,
        cond: &ast::Span<ast::Expr>,
        if_true: ir::BlockRef,
        if_false: ir::BlockRef,
    ) {
        let (cond, cond_ty) = self.compile_expr(&cond);
        cond_ty
            .unify_ty(Type::Bool)
            .expect("expected a boolean in conditional expr");

        self.set_branch(ir::Branch::Cond {
            cond,
            if_true,
            if_false,
        });
    }
    fn compile_if(&mut self, if_stmt: &'s ast::If) {
        let if_block = self.func.alloc_block();
        let else_block = self.func.alloc_block();
        let exit_block = self.func.alloc_block();

        self.compile_cond(&if_stmt.cond, if_block, else_block);

        self.block = if_block;
        self.compile_block(&if_stmt.if_block);
        self.set_branch(ir::Branch::Static(exit_block));

        self.block = else_block;
        match &if_stmt.else_block {
            ast::Else::Block(block) => self.compile_block(&block),
            ast::Else::If(if_stmt) => self.compile_if(&*if_stmt),
            ast::Else::None => (),
        }
        self.set_branch(ir::Branch::Static(exit_block));

        self.block = exit_block;
    }
    fn compile_expr(&mut self, expr: &ast::Span<ast::Expr>) -> (ir::Expr, TypeVarRef) {
        match &expr.ty {
            ast::Expr::Bool(value) => (ir::Expr::Bool(*value), TypeVarRef::new(Type::Bool)),
            ast::Expr::Int(value) => (ir::Expr::Int(*value), TypeVarRef::new(Type::AnyInt)),
            ast::Expr::Infix { left, right, op } => {
                let (left_expr, left_ty) = self.compile_expr(left);
                let (right_expr, right_ty) = self.compile_expr(right);
                left_ty
                    .unify_ty(Type::AnyInt)
                    .expect("left side of infix expr must be numeric");
                right_ty
                    .unify_ty(Type::AnyInt)
                    .expect("right side of infix expr must be numeric");

                right_ty
                    .unify_var(&left_ty)
                    .expect("left and right side of infix expr must have same numeric type");
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
                let var = self.scope.get(name.as_str()).expect("undefined variable");
                (ir::Expr::Var(var.clone()), var.ty().clone())
            }
            ast::Expr::Deref(expr) => {
                let (expr, expr_ty) = self.compile_expr(expr);
                let any = TypeVarRef::new(Type::Any);
                expr_ty
                    .unify_ty(Type::Ptr(any.clone()))
                    .expect("trying to deref a non-pointer expr");
                (ir::Expr::Deref(Box::new(expr)), any)
            }
            ast::Expr::Call(func_call) => {
                let func = self
                    .program_ast
                    .funcs
                    .iter()
                    .find(|func| func.name == func_call.name)
                    .expect("called func does not exist");
                if func_call.args.len() != func.params.len() {
                    panic!("wrong number of arguments in func call");
                }
                let mut args = vec![];
                for (arg, param) in func_call.args.iter().zip(&func.params) {
                    let (arg_expr, arg_ty) = self.compile_expr(arg);
                    args.push(arg_expr);
                    arg_ty
                        .unify_ty(self.compile_ast_ty(&param.ty))
                        .expect("incorrect argument in function call");
                }
                let return_block = self.func.alloc_block();
                self.set_branch(ir::Branch::Call {
                    name: func.name.clone(),
                    args,
                    return_to: return_block,
                });
                self.block = return_block;
                (
                    ir::Expr::Returned,
                    TypeVarRef::new(
                        self.compile_ast_ty(
                            &func
                                .returns
                                .as_ref()
                                .expect("return statement in func that does not return anything"),
                        ),
                    ),
                )
            }
        }
    }
    fn compile_ref_expr(&self, ref_expr: &ast::Span<ast::RefExpr>) -> (ir::RefExpr, TypeVarRef) {
        match &ref_expr.ty {
            ast::RefExpr::Ident(name) => {
                let var = self.scope.get(name.as_str()).expect("undefined variable");
                (ir::RefExpr::Var(var.clone()), var.ty().clone())
            }
        }
    }
}
