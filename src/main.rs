mod ast;
mod compiler;
mod lexer;
mod parser;

use std::io;
use std::process;

use compiler::Compiler;

fn run() -> i32 {
    let comp = Compiler::new();
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
