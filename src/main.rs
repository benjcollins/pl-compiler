#![feature(hash_set_entry)]

use std::fs;

use compile_ast::compile_ast;

use crate::{
    parser::Parser,
    region::{propagate_regions, Regions},
};

mod ast;
mod compile_ast;
mod ir;
mod lexer;
mod parser;
mod region;
mod test_lexer;
mod test_parser;
mod token;
mod ty;
mod unify;

fn main() {
    let source = fs::read_to_string("example.txt").unwrap();
    let mut parser = Parser::new(&source);
    let func = match parser.parse_func() {
        Ok(func) => func,
        Err(err) => {
            println!("{:?}", err);
            println!("{:?}", parser.peek());
            return;
        }
    };
    let ir_func = ir::Func::new();
    let entry = compile_ast(&func, &ir_func);

    println!("{}", entry);

    let regions = propagate_regions(entry, &Regions::new());

    println!("{:?}", regions);
}
