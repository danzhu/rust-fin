mod common;
mod token;
mod ast;
mod ir;
mod def;

mod lexer;
mod parser;
mod name_res;
mod type_chk;
mod ir_gen;
mod code_gen;
mod compiler;

use std::{io, process};

use compiler::Compiler;

fn run() -> Result<(), i32> {
    let mut comp = Compiler::new();
    comp.compile(io::stdin(), io::stdout()).map_err(|err| {
        eprintln!("{}", err);
        1
    })
}

fn main() {
    if let Err(code) = run() {
        process::exit(code);
    }
}
