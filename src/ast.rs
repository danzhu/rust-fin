#[derive(Debug, Clone, Copy)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

#[derive(Debug, Clone, Copy)]
pub enum Paren {
    Paren,
    Bracket,
    Brace,
}

pub struct Module {
    pub functions: Vec<Func>,
}

pub struct Func {
    pub name: String,
    pub expr: Expr,
}

pub enum Expr {
    Int(i32),
    Binary {
        op: Op,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}
