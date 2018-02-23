use std::fmt;

const INDENT: &str = "  ";

pub struct Source {
    pub defs: Vec<Def>,
}

pub struct Def {
    pub kind: DefKind,
}

pub enum DefKind {
    Function(Function),
}

pub struct Function {
    pub name: String,
    pub params: Vec<Binding>,
    pub ret: Type,
    pub body: Expr,
}

pub struct Expr {
    pub kind: ExprKind,
}

pub enum ExprKind {
    Seq {
        first: Box<Expr>,
        second: Box<Expr>,
    },
    Let {
        value: Box<Expr>,
        var: String,
    },
    Function {
        path: Path,
        args: Vec<Expr>,
    },
    Binary {
        op: String,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        succ: Box<Expr>,
        fail: Box<Expr>,
    },
    Int(i32),
    Id(String),
    Noop,
}

pub enum Type {
    Named { path: Path },
    Void,
}

pub struct Path {
    pub name: String,
}

pub struct Binding {
    pub name: String,
    pub tp: Type,
}

impl fmt::Debug for Source {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for func in &self.defs {
            write!(f, "{:?}", func)?;
        }
        Ok(())
    }
}

impl fmt::Debug for Def {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            DefKind::Function(ref func) => writeln!(f, "{:?}", func),
        }
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Function {}", self.name)?;
        for param in &self.params {
            writeln!(f, "{}Param {:?}", INDENT, param)?;
        }
        writeln!(f, "{}Ret {:?}", INDENT, self.ret)?;
        write!(f, "{:?}", self.body)
    }
}

impl Expr {
    fn debug_fmt(&self, f: &mut fmt::Formatter, ind: i32) -> fmt::Result {
        for _ in 0..ind {
            write!(f, "{}", INDENT)?;
        }
        match self.kind {
            ExprKind::Seq {
                ref first,
                ref second,
            } => {
                writeln!(f, "Seq")?;
                first.debug_fmt(f, ind + 1)?;
                second.debug_fmt(f, ind + 1)?;
            }
            ExprKind::Let { ref value, ref var } => {
                writeln!(f, "Let {}", var)?;
                value.debug_fmt(f, ind + 1)?;
            }
            ExprKind::Function { ref path, ref args } => {
                writeln!(f, "Function {:?}", path)?;
                for arg in args {
                    arg.debug_fmt(f, ind + 1)?;
                }
            }
            ExprKind::Binary {
                ref op,
                ref left,
                ref right,
            } => {
                writeln!(f, "Binary {}", op)?;
                left.debug_fmt(f, ind + 1)?;
                right.debug_fmt(f, ind + 1)?;
            }
            ExprKind::If {
                ref cond,
                ref succ,
                ref fail,
            } => {
                writeln!(f, "If")?;
                cond.debug_fmt(f, ind + 1)?;
                succ.debug_fmt(f, ind + 1)?;
                fail.debug_fmt(f, ind + 1)?;
            }
            ExprKind::Int(i) => {
                writeln!(f, "Int {}", i)?;
            }
            ExprKind::Id(ref id) => {
                writeln!(f, "Id {}", id)?;
            }
            ExprKind::Noop => {
                writeln!(f, "Noop")?;
            }
        }
        Ok(())
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.debug_fmt(f, 1)
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Named { ref path } => write!(f, "{:?}", path),
            Type::Void => write!(f, "Void"),
        }
    }
}

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Debug for Binding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {:?}", self.name, self.tp)
    }
}
