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

pub struct Decl {
    pub name: String,
}

pub struct Module {
    pub functions: Vec<Func>,
}

pub struct Func {
    pub name: String,
    pub params: Vec<Decl>,
    pub body: Expr,
}

pub struct Expr {
    kind: ExprKind,
}

pub enum ExprKind {
    Block {
        exprs: Vec<Expr>,
    },
    Binary {
        op: Op,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Call {
        name: String,
        args: Vec<Expr>,
    },
    Int(i32),
    Id(String),
}

impl Expr {
    pub fn new(kind: ExprKind) -> Expr {
        Expr { kind }
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }
}
