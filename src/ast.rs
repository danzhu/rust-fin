use std::fmt;
use std::usize;

const INDENT: &str = "  ";

pub struct Source {
    pub defs: Vec<Def>,
}

pub struct Def {
    pub kind: DefKind,
}

pub enum DefKind {
    Type(TypeDef),
    Func(FuncDef),
}

pub struct TypeDef {
    pub name: String,
    pub kind: TypeDefKind,
}

pub enum TypeDefKind {
    Int,
}

pub struct FuncDef {
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
    Id(Path),
    Noop,
}

pub struct Type {
    kind: TypeKind,
}

pub enum TypeKind {
    Named { path: Path },
    Void,
}

pub struct Path {
    pub name: String,
    pub id: Index,
}

pub struct Binding {
    pub name: String,
    pub tp: Type,
}

pub struct Index(usize);

impl Source {
    pub fn new() -> Self {
        Self {
            defs: Vec::new(),
        }
    }
}

impl fmt::Debug for Source {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for func in &self.defs {
            write!(f, "{:?}", func)?;
        }
        Ok(())
    }
}

impl Def {
    pub fn new(kind: DefKind) -> Self {
        Self { kind }
    }
}

impl fmt::Debug for Def {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            DefKind::Type(ref tp) => writeln!(f, "{:?}", tp),
            DefKind::Func(ref func) => writeln!(f, "{:?}", func),
        }
    }
}

impl fmt::Debug for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Type ")?;
        match self.kind {
            TypeDefKind::Int => writeln!(f, "Int"),
        }
    }
}

impl fmt::Debug for FuncDef {
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
    pub fn new(kind: ExprKind) -> Self {
        Self { kind }
    }

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
                writeln!(f, "Id {:?}", id)?;
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

impl Type {
    pub fn new(kind: TypeKind) -> Self {
        Self { kind }
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            TypeKind::Named { ref path } => write!(f, "{:?}", path),
            TypeKind::Void => write!(f, "Void"),
        }
    }
}

impl Path {
    pub fn new(name: String) -> Self {
        Self { name, id: Index::INVALID }
    }
}

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Binding {
    pub fn new(name: String, tp: Type) -> Self {
        Self { name, tp }
    }
}

impl fmt::Debug for Binding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {:?}", self.name, self.tp)
    }
}

impl Index {
    const INVALID: Index = Index(usize::MAX);

    pub fn new(val: usize) -> Self {
        Index(val)
    }
}
