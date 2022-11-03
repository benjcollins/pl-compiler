#![feature(hash_set_entry)]

use std::fs;

use compile_ast::compile_func;

use crate::{parser::Parser, region::check_func};

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
    let func_ast = match parser.parse_func() {
        Ok(func) => func,
        Err(err) => {
            println!("{:?}", err);
            println!("{:?}", parser.peek());
            return;
        }
    };
    let func_ir = compile_func(&func_ast);

    println!("{}", func_ir.entry);

    check_func(&func_ir);

    // let regions = propagate_regions(entry, &Regions::new());

    // println!("{:?}", regions);
}
