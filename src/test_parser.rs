#[cfg(test)]
mod tests {
    use crate::{
        ast::{
            Block, Else, Expr, Func, FuncCall, If, InfixOp, IntSize, IntType, Param, RefExpr, Stmt,
            Type,
        },
        parser::{Parser, Prec},
    };

    fn infix(left: Expr, op: InfixOp, right: Expr) -> Expr {
        Expr::Infix {
            left: Box::new(left),
            right: Box::new(right),
            op,
        }
    }

    fn empty_block() -> Block {
        Block(vec![])
    }

    #[test]
    fn test_parse_expr() {
        let mut parser = Parser::new("a + (2 - 9)");
        let expr = parser.parse_expr(Prec::Bracket).unwrap();
        assert!(parser.peek().is_none());
        assert_eq!(
            expr,
            infix(
                Expr::Ident("a".to_string()),
                InfixOp::Add,
                infix(Expr::Int(2), InfixOp::Subtract, Expr::Int(9),)
            )
        )
    }

    #[test]
    fn test_parse_decl() {
        let mut parser = Parser::new("var x: i32 = 2;");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.peek().is_none());
        assert_eq!(
            stmt,
            Stmt::Decl {
                name: "x".to_string(),
                ty: Some(Type::Int(IntType {
                    size: IntSize::B32,
                    signed: true
                })),
                expr: Some(Expr::Int(2)),
            }
        );
    }

    #[test]
    fn test_parse_assign() {
        let mut parser = Parser::new("x = 4;");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.peek().is_none());
        assert_eq!(
            stmt,
            Stmt::Assign {
                ref_expr: RefExpr::Ident("x".to_string()),
                expr: Expr::Int(4)
            }
        )
    }

    #[test]
    fn test_parse_return() {
        let mut parser = Parser::new("return 4;");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.peek().is_none());
        assert_eq!(stmt, Stmt::Return(Some(Expr::Int(4))))
    }

    #[test]
    fn test_parse_while() {
        let mut parser = Parser::new("while true { }");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.peek().is_none());
        assert_eq!(
            stmt,
            Stmt::While {
                cond: Expr::Bool(true),
                block: Block(vec![])
            }
        )
    }

    #[test]
    fn test_parse_if() {
        let mut parser = Parser::new("if true { }");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.peek().is_none());
        assert_eq!(
            stmt,
            Stmt::If(If {
                cond: Expr::Bool(true),
                if_block: Block(vec![]),
                else_block: Else::None,
            })
        )
    }

    #[test]
    fn test_parse_if_else() {
        let mut parser = Parser::new("if true { } else { }");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.peek().is_none());
        assert_eq!(
            stmt,
            Stmt::If(If {
                cond: Expr::Bool(true),
                if_block: Block(vec![]),
                else_block: Else::Block(Block(vec![]))
            })
        )
    }

    #[test]
    fn test_parse_if_else_if() {
        let mut parser = Parser::new("if x { } else if y { }");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.peek().is_none());
        assert_eq!(
            stmt,
            Stmt::If(If {
                cond: Expr::Ident("x".to_string()),
                if_block: Block(vec![]),
                else_block: Else::If(Box::new(If {
                    cond: Expr::Ident("y".to_string()),
                    if_block: Block(vec![]),
                    else_block: Else::None
                }))
            })
        )
    }

    #[test]
    fn test_parse_deref_assign() {
        let mut parser = Parser::new("*x = 3;");
        let stmt = parser.parse_stmt().unwrap();
        assert!(parser.peek().is_none());
        assert_eq!(
            stmt,
            Stmt::DerefAssign {
                ref_expr: Expr::Ident("x".to_string()),
                expr: Expr::Int(3)
            }
        )
    }

    #[test]
    fn test_parse_func() {
        let mut parser = Parser::new("func f() { }");
        let func = parser.parse_func().unwrap();
        assert!(parser.peek().is_none());
        assert_eq!(
            func,
            Func {
                name: "f".to_string(),
                block: empty_block(),
                params: vec![],
                returns: None,
            }
        )
    }

    #[test]
    fn test_parse_ref() {
        let mut parser = Parser::new("&x");
        let expr = parser.parse_expr(Prec::Bracket).unwrap();
        assert!(parser.peek().is_none());
        assert_eq!(expr, Expr::Ref(RefExpr::Ident("x".to_string())));
    }

    #[test]
    fn test_parse_param() {
        let mut parser = Parser::new("func f(x: i32) { }");
        let func = parser.parse_func().unwrap();
        assert!(parser.peek().is_none());
        assert_eq!(
            func,
            Func {
                name: "f".to_string(),
                block: empty_block(),
                params: vec![Param {
                    name: "x".to_string(),
                    ty: Type::Int(IntType {
                        signed: true,
                        size: IntSize::B32
                    })
                }],
                returns: None,
            }
        );
    }

    #[test]
    fn test_parse_params() {
        let mut parser = Parser::new("func f(x: i32, y: u32) { }");
        let func = parser.parse_func().unwrap();
        assert!(parser.peek().is_none());
        assert_eq!(
            func,
            Func {
                name: "f".to_string(),
                block: empty_block(),
                params: vec![
                    Param {
                        name: "x".to_string(),
                        ty: Type::Int(IntType {
                            signed: true,
                            size: IntSize::B32
                        })
                    },
                    Param {
                        name: "y".to_string(),
                        ty: Type::Int(IntType {
                            signed: false,
                            size: IntSize::B32
                        })
                    }
                ],
                returns: None,
            }
        );
    }

    #[test]
    fn test_parse_call() {
        let mut parser = Parser::new("f(5, 2)");
        let expr = parser.parse_expr(Prec::Bracket).unwrap();
        assert!(parser.peek().is_none());
        assert_eq!(
            expr,
            Expr::Call(FuncCall {
                name: "f".to_string(),
                args: vec![Expr::Int(5), Expr::Int(2),]
            })
        )
    }
}
