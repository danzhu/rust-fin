mod ast;
mod generator;
mod lexer;
mod parser;
mod util;

use std::io;
use std::io::Read;

use generator::Generator;
use lexer::Lexer;
use parser::Parser;

fn main() {
    // lex
    let mut source = String::new();
    io::stdin().read_to_string(&mut source).expect("cannot read from stdin");
    let tokens = Lexer::new(source.chars());

    // parse
    let mut par = Parser::new(tokens);
    let module = par.parse().expect("parser error");

    // generate
    let mut out = &mut io::stdout();
    let mut gen = Generator::new(out);
    gen.generate(&module).expect("code generator error");
}
