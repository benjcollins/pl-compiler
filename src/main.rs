#![feature(hash_set_entry)]

use std::fs;

mod ast;
mod cfg;
mod idents;
mod lexer;
mod parser;
mod token;
mod unify;

fn main() {
    // let source = fs::read_to_string("example.txt").unwrap();
    // let ast = parse_expr(&source);
    // println!("{:?}", ast);
}
