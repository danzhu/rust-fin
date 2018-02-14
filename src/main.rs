mod compiler;
mod lexer;
mod parser;
// mod generator;

use std::io::Read;

fn main() {
    let mut source = String::new();
    std::io::stdin()
        .read_to_string(&mut source)
        .expect("cannot read from stdin");

    let out = &mut std::io::stdout();

    match compiler::compile(source.chars(), out) {
        Err(err) => println!("{:?}", err),
        _ => {}
    }
}
