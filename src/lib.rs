#![allow(dead_code)]
#[macro_use]
extern crate iota;
extern crate byteorder;

mod lexer;
mod token;
mod ast;
mod parser;
mod bytecode;
