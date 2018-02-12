#[derive(Debug)]
pub struct Decl {
    pub name: String,
}

#[derive(Debug)]
pub struct Module {
    pub functions: Vec<Func>,
}

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub params: Vec<Decl>,
    pub body: Expr,
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Block {
        exprs: Vec<Expr>,
    },
    Let {
        value: Box<Expr>,
        var: String,
    },
    Function {
        name: String,
        args: Vec<Expr>,
    },
    Method {
        expr: Box<Expr>,
        name: String,
        args: Vec<Expr>,
    },
    Int(i32),
    Id(String),
}
