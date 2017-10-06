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
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}
