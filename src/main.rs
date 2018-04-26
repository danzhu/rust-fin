mod ast;
mod common;
mod ctx;
mod error;
mod ir;
mod llvm;
mod ptree;
mod token;

mod ast_gen;
mod code_gen;
mod compiler;
mod ir_gen;
mod parser;

use std::{io, process};

use compiler::Compiler;

const COMPILE_FAILED: i32 = 2;

fn run() -> Result<(), i32> {
    let mut comp = Compiler::new();
    comp.compile(io::stdin(), io::stdout()).map_err(|err| {
        comp.explain(&err, &mut io::stderr())
            .expect("cannot write error to stderr");
        COMPILE_FAILED
    })
}

fn main() {
    if let Err(code) = run() {
        process::exit(code);
    }
}
