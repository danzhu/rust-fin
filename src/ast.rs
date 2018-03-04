use std::fmt;
use std::usize;
use std::slice;
use std::ops;

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
    pub body: Expr,
    pub locals: List<Binding>,
}

#[derive(Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub tp: Type,
}

#[derive(Clone)]
pub enum ExprKind {
    Block {
        stmts: Vec<Expr>,
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

#[derive(Clone, PartialEq, Eq)]
pub struct Type {
    pub kind: TypeKind,
}

#[derive(Clone, PartialEq, Eq)]
pub enum TypeKind {
    Named { path: Path },
    Void,
    Unknown,
}

#[derive(Clone)]
pub struct Func {
    pub path: Path,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Path {
    pub name: String,
    pub index: Index,
}

#[derive(Clone)]
pub struct Binding {
    pub name: String,
    pub tp: Type,
}

#[derive(Clone)]
pub struct List<T> {
    items: Vec<T>,
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
            DefKind::Type(ref tp) => writeln!(f, "{:?}", tp),
            DefKind::Func(ref func) => writeln!(f, "{:?}", func),
        }
    }
}

impl TypeDef {
    pub fn new<Str>(name: Str, kind: TypeDefKind) -> Self
    where
        Str: Into<String>,
    {
        Self {
            name: name.into(),
            kind,
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

impl FuncDef {
    pub fn new<Str>(name: Str, params: Vec<Binding>, ret: Type, body: Expr) -> Self
    where
        Str: Into<String>,
    {
        Self {
            name: name.into(),
            params,
            ret,
            body,
            locals: List::new(),
        }
    }
}

impl fmt::Debug for FuncDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Function {}", self.name)?;
        writeln!(f, "Params")?;
        for param in &self.params {
            writeln!(f, "{}{:?}", INDENT, param)?;
        }
        writeln!(f, "Ret {:?}", self.ret)?;
        writeln!(f, "Locals")?;
        for local in &self.locals {
            writeln!(f, "{}{:?}", INDENT, local)?;
        }
        write!(f, "{:?}", self.body)
    }
}

impl Expr {
    pub fn new(kind: ExprKind) -> Self {
        Self {
            kind,
            tp: Type::new(TypeKind::Unknown),
        }
    }

    fn debug_fmt(&self, f: &mut fmt::Formatter, ind: i32) -> fmt::Result {
        for _ in 0..ind {
            write!(f, "{}", INDENT)?;
        }

        write!(f, "{:?} ", self.tp)?;

        match self.kind {
            ExprKind::Block { ref stmts } => {
                writeln!(f, "Block")?;
                for stmt in stmts {
                    stmt.debug_fmt(f, ind + 1)?;
                }
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
        self.debug_fmt(f, 0)
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
            TypeKind::Unknown => write!(f, "Unknown"),
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
    pub fn new<Str>(name: Str) -> Self
    where
        Str: Into<String>,
    {
        Self {
            name: name.into(),
            index: Index::UNKNOWN,
        }
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if self.index != Index::UNKNOWN {
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

impl<T> List<T> {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
        }
    }

    pub fn push(&mut self, item: T) -> Index {
        let idx = Index(self.items.len());
        self.items.push(item);
        idx
    }
}

impl<T> ops::Index<Index> for List<T> {
    type Output = T;

    fn index(&self, idx: Index) -> &Self::Output {
        &self.items[idx.0]
    }
}

impl<'a, T> IntoIterator for &'a List<T> {
    type IntoIter = slice::Iter<'a, T>;
    type Item = &'a T;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut List<T> {
    type IntoIter = slice::IterMut<'a, T>;
    type Item = &'a mut T;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter_mut()
    }
}

impl Index {
    pub const UNKNOWN: Index = Index(usize::MAX);
}

impl fmt::Debug for Index {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
