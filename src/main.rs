#![feature(hash_set_entry)]

use std::fs;

use crate::{compile_ast::compile_program, mem::check_program, parser::Parser};

mod ast;
mod compile_ast;
mod ir;
mod lexer;
mod mem;
mod parser;
mod refr;
mod token;
mod ty;
mod unify;

fn main() {
    let source = fs::read_to_string("example.txt").unwrap();
    let mut parser = Parser::new(&source);
    let program_ast = match parser.parse_program() {
        Ok(func) => func,
        Err(err) => {
            println!("{:?}", err);
            println!("{:?}", parser.peek());
            return;
        }
    };
    let program_ir = compile_program(&program_ast);

    println!("{}", program_ir);

    check_program(&program_ir);
}
