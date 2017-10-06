mod ast;
mod generator;

use std::io;

use ast::{Module, Func, Expr, BinOp};
use generator::Generator;

fn main() {
    // let mut line = String::new();
    // while let Ok() = io::stdin().read_line(&mut line) {
    // }

    let expr = Expr::Binary {
        op: BinOp::Add,
        left: Box::new(Expr::Int(1)),
        right: Box::new(Expr::Binary {
            op: BinOp::Sub,
            left: Box::new(Expr::Int(2)),
            right: Box::new(Expr::Int(3)),
        }),
    };
    let func = Func { name: "main".to_string(), expr };
    let module = Module { functions: vec![func] };

    let mut out = &mut io::stdout();
    let mut gen = Generator::new(out);
    gen.generate(&module).expect("failed to generate");
}
