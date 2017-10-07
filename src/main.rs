mod ast;
mod generator;
mod lexer;
mod operator;
mod parser;

use std::io;
use std::io::Read;

use generator::Generator;
use lexer::Lexer;
use parser::Parser;

fn main() {
    // lex
    let mut source = String::new();
    io::stdin().read_to_string(&mut source).unwrap();
    let tokens = Lexer::new(source.chars());
    // for token in lex {
    //     println!("{:?}", token);
    // }

    // parse
    let mut par = Parser::new(tokens);
    let module = par.parse().expect("failed to parse");

    // generate
    let mut out = &mut io::stdout();
    let mut gen = Generator::new(out);
    gen.generate(&module).expect("failed to generate");
}
