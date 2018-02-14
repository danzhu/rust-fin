mod compiler;
mod lexer;
mod parser;
// mod generator;

fn run() -> i32 {
    if let Err(err) = compiler::compile(std::io::stdin(), std::io::stdout()) {
        println!("{}", err);
        1
    } else {
        0
    }
}

fn main() {
    std::process::exit(run());
}
