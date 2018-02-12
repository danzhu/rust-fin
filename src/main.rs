mod ast;
mod generator;
mod lexer;
mod symbol;
mod parser;

use std::io::Read;

use generator::Generator;
use lexer::Lexer;
use parser::Parser;

fn main() {
    // lex
    let tokens = {
        let mut source = String::new();
        std::io::stdin().read_to_string(&mut source).expect("cannot read from stdin");
        let lex = Lexer::new(source.chars());
        lex.collect::<Result<Vec<_>, _>>().expect("lexer error")
    };

    // parse
    let module = {
        let mut par = Parser::new(tokens.into_iter());
        par.parse().expect("parser error")
    };

    // generate
    let out = &mut std::io::stdout();
    let mut gen = Generator::new(out);
    gen.generate(&module).expect("code generator error");
}
