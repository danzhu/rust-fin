use std::fmt;
use std::usize;

use common::*;

const INDENT: &str = "  ";

#[derive(Clone)]
pub struct Source {
    pub defs: Vec<Def>,
}

#[derive(Clone)]
pub struct Def {
    pub kind: DefKind,
}

#[derive(Clone)]
pub enum DefKind {
    Type(TypeDef),
    Func(FuncDef),
}

#[derive(Clone)]
pub struct TypeDef {
    pub name: String,
    pub kind: TypeDefKind,
}

#[derive(Clone)]
pub enum TypeDefKind {
    Int,
}

#[derive(Clone)]
pub struct FuncDef {
    pub name: String,
    pub params: Vec<Binding>,
    pub ret: Type,
    pub kind: FuncDefKind,
}

#[derive(Clone)]
pub enum FuncDefKind {
    Body(Expr),
}

#[derive(Clone)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Clone)]
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
        func: Func,
        args: Vec<Expr>,
    },
    Binary {
        op: Op,
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

#[derive(Clone)]
pub struct Type {
    pub kind: TypeKind,
}

#[derive(Clone)]
pub enum TypeKind {
    Named { path: Path },
    Void,
}

#[derive(Clone)]
pub struct Func {
    pub path: Path,
}

#[derive(Clone)]
pub struct Path {
    pub name: String,
    pub index: Index,
}

#[derive(Clone)]
pub struct Binding {
    pub name: String,
    pub tp: Type,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Index(usize);

impl Source {
    pub fn new() -> Self {
        Self { defs: Vec::new() }
    }
}

impl fmt::Debug for Source {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for def in &self.defs {
            write!(f, "{:?}", def)?;
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
            DefKind::Type(ref tp) => writeln!(f, "Type {:?}", tp),
            DefKind::Func(ref func) => writeln!(f, "Function {:?}", func),
        }
    }
}

impl fmt::Debug for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            TypeDefKind::Int => writeln!(f, "Int"),
        }
    }
}

impl fmt::Debug for FuncDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.name)?;
        for param in &self.params {
            writeln!(f, "{}Param {:?}", INDENT, param)?;
        }
        writeln!(f, "{}Ret {:?}", INDENT, self.ret)?;
        match self.kind {
            FuncDefKind::Body(ref body) => write!(f, "{:?}", body),
        }
    }
}

impl Expr {
    pub fn new(kind: ExprKind) -> Self {
        Self { kind }
    }

    pub fn for_each_expr<E, Act>(&mut self, mut act: Act) -> Result<(), E>
    where
        Act: FnMut(&mut Expr) -> Result<(), E>,
    {
        match self.kind {
            ExprKind::Seq {
                ref mut first,
                ref mut second,
            } => {
                act(first)?;
                act(second)?;
            }
            ExprKind::Let { ref mut value, .. } => {
                act(value)?;
            }
            ExprKind::Function { ref mut args, .. } => for arg in args {
                act(arg)?;
            },
            ExprKind::Binary {
                ref mut left,
                ref mut right,
                ..
            } => {
                act(left)?;
                act(right)?;
            }
            ExprKind::If {
                ref mut cond,
                ref mut succ,
                ref mut fail,
            } => {
                act(cond)?;
                act(succ)?;
                act(fail)?;
            }
            ExprKind::Int(_) | ExprKind::Id(_) | ExprKind::Noop => {}
        }
        Ok(())
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
            ExprKind::Function { ref func, ref args } => {
                writeln!(f, "Function {:?}", func)?;
                for arg in args {
                    arg.debug_fmt(f, ind + 1)?;
                }
            }
            ExprKind::Binary {
                ref op,
                ref left,
                ref right,
            } => {
                writeln!(f, "Binary {:?}", op)?;
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

impl Type {
    pub fn new(kind: TypeKind) -> Self {
        Self { kind }
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            TypeKind::Named { ref path } => write!(f, "{}", path),
            TypeKind::Void => write!(f, "Void"),
        }
    }
}

impl Func {
    pub fn new(path: Path) -> Self {
        Self { path }
    }
}

impl fmt::Debug for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.path)
    }
}

impl Path {
    pub fn new(name: String) -> Self {
        Self {
            name,
            index: Index::INVALID,
        }
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if self.index != Index::INVALID {
            write!(f, "[{:?}]", self.index)?;
        }
        Ok(())
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
    pub const INVALID: Index = Index(usize::MAX);

    pub fn new(val: usize) -> Self {
        Index(val)
    }
}

impl fmt::Debug for Index {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
