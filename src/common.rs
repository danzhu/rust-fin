use std::fmt;

#[derive(Copy, Clone, Debug)]
pub struct Pos {
    pub line: i32,
    pub column: i32,
}

#[derive(Copy, Clone, Debug)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

#[derive(Copy, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Op::Add => write!(f, "Add"),
            Op::Sub => write!(f, "Sub"),
            Op::Mul => write!(f, "Mul"),
            Op::Div => write!(f, "Div"),
            Op::Mod => write!(f, "Mod"),
            Op::Eq => write!(f, "Eq"),
            Op::Ne => write!(f, "Ne"),
        }
    }
}
