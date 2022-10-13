#![feature(hash_set_entry)]

use std::fs;

use crate::parser::Parser;

mod ast;
mod cfg;
mod idents;
mod lexer;
mod parser;
mod test_parser;
mod token;
//mod unify;

fn main() {
    let source = fs::read_to_string("example.txt").unwrap();
    let mut parser = Parser::new(&source);
    let stmt = match parser.parse_stmt() {
        Ok(stmt) => stmt,
        Err(err) => {
            println!("{:?}", err);
            println!("{:?}", parser.peek());
            return;
        }
    };
    println!("{:?}", stmt);
}
