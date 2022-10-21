#![feature(hash_set_entry)]

use std::fs;

use typed_arena::Arena;

use crate::parser::Parser;

mod ast;
mod ast_cfg;
mod cfg;
mod idents;
mod lexer;
mod parser;
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
    let arena = Arena::new();
    //    println!("{:?}", func);
    let cfg_func = ast_cfg::create_cfg(func, &arena).unwrap();
    println!("{}", cfg_func.entry);
}
