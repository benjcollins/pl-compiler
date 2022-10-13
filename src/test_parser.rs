#[cfg(test)]
use crate::{
    ast::{Block, Else, Expr, If, InfixOp, RefExpr, Stmt, Type},
    idents::Ident,
    parser::{Parser, Prec},
};

#[test]
fn test_parse_expr() {
    let mut parser = Parser::new("a + (2 - 9)");
    let expr = parser.parse_expr(Prec::Bracket).unwrap();
    assert!(parser.peek().is_none());
    assert_eq!(
        expr,
        Expr::infix(
            Expr::ident("a"),
            InfixOp::Add,
            Expr::infix(Expr::Int(2), InfixOp::Subtract, Expr::Int(9),)
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
            name: Ident::new("x"),
            ty: Some(Type::I32),
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
            ref_expr: RefExpr::ident("x"),
            expr: Expr::Int(4)
        }
    )
}

#[test]
fn test_parse_return() {
    let mut parser = Parser::new("return x;");
    let stmt = parser.parse_stmt().unwrap();
    assert!(parser.peek().is_none());
    assert_eq!(stmt, Stmt::Return(Some(Expr::ident("x"))))
}

#[test]
fn test_parse_while() {
    let mut parser = Parser::new("while a { }");
    let stmt = parser.parse_stmt().unwrap();
    assert!(parser.peek().is_none());
    assert_eq!(
        stmt,
        Stmt::While {
            cond: Expr::ident("a"),
            block: Block(vec![])
        }
    )
}

#[test]
fn test_parse_if() {
    let mut parser = Parser::new("if x { }");
    let stmt = parser.parse_stmt().unwrap();
    assert!(parser.peek().is_none());
    assert_eq!(
        stmt,
        Stmt::If(If {
            cond: Expr::ident("x"),
            if_block: Block(vec![]),
            else_block: Else::None,
        })
    )
}

#[test]
fn test_parse_if_else() {
    let mut parser = Parser::new("if x { } else { }");
    let stmt = parser.parse_stmt().unwrap();
    assert!(parser.peek().is_none());
    assert_eq!(
        stmt,
        Stmt::If(If {
            cond: Expr::ident("x"),
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
            cond: Expr::ident("x"),
            if_block: Block(vec![]),
            else_block: Else::If(Box::new(If {
                cond: Expr::ident("y"),
                if_block: Block(vec![]),
                else_block: Else::None
            }))
        })
    )
}

#[test]
fn test_parse_deref_assign() {
    let mut parser = Parser::new("*x = 3");
    let stmt = parser.parse_stmt().unwrap();
    assert!(parser.peek().is_none());
    assert_eq!(stmt, Stmt::DerefAssign { ptr: Expr::ident("x"), expr: Expr::Int(3) })
}
