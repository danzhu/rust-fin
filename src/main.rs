mod common;
mod token;
mod ast;
mod store;

mod compiler;
mod lexer;
mod parser;
mod resolver;
mod type_checker;

use std::io;
use std::process;

use compiler::Compiler;

fn run() -> i32 {
    let mut comp = Compiler::new();
    let res = comp.compile(io::stdin(), io::stdout());

    if let Err(err) = res {
        println!("{}", err);
        return 1;
    }

    0
}

fn main() {
    process::exit(run());
}
