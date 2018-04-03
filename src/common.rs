use std::{fmt, ops, slice, usize};

use ctx::*;

pub const INDENT: &str = "  ";

#[derive(Copy, Clone, Debug)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub fn new(start: Pos, end: Pos) -> Self {
        Span { start, end }
    }

    pub fn zero(file: Index) -> Self {
        let pos = Pos {
            file,
            line: 0,
            column: 0,
        };
        Span {
            start: pos,
            end: pos,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Pos {
    pub file: Index,
    pub line: usize,
    pub column: usize,
}

impl Pos {
    pub fn format(&self, ctx: &Context) -> String {
        let filename = &ctx.sources[self.file].filename;
        format!("{}:{}:{}", filename, self.line + 1, self.column + 1)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UnaryOp::Neg => write!(f, "Neg"),
            UnaryOp::Not => write!(f, "Not"),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum BinaryOp {
    Arith(ArithOp),
    Comp(CompOp),
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BinaryOp::Arith(op) => write!(f, "{}", op),
            BinaryOp::Comp(op) => write!(f, "{}", op),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl fmt::Display for ArithOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ArithOp::Add => write!(f, "Add"),
            ArithOp::Sub => write!(f, "Sub"),
            ArithOp::Mul => write!(f, "Mul"),
            ArithOp::Div => write!(f, "Div"),
            ArithOp::Mod => write!(f, "Mod"),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum CompOp {
    Eq,
    Ne,
    Lt,
    Gt,
}

impl fmt::Display for CompOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CompOp::Eq => write!(f, "Eq"),
            CompOp::Ne => write!(f, "Ne"),
            CompOp::Lt => write!(f, "Lt"),
            CompOp::Gt => write!(f, "Gt"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Path {
    pub name: Name,
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

pub type Name = String;

#[derive(Clone, Debug)]
pub struct List<T> {
    items: Vec<T>,
}

impl<T> List<T> {
    pub fn new() -> Self {
        List { items: Vec::new() }
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn push(&mut self, item: T) -> Index {
        let idx = Index(self.items.len());
        self.items.push(item);
        idx
    }

    pub fn iter(&self) -> slice::Iter<T> {
        self.items.iter()
    }

    pub fn iter_mut(&mut self) -> slice::IterMut<T> {
        self.items.iter_mut()
    }
}

impl<T> Default for List<T> {
    fn default() -> Self {
        List::new()
    }
}

impl<T> ops::Index<Index> for List<T> {
    type Output = T;

    fn index(&self, idx: Index) -> &Self::Output {
        &self.items[idx.0]
    }
}

impl<T> ops::IndexMut<Index> for List<T> {
    fn index_mut(&mut self, idx: Index) -> &mut Self::Output {
        &mut self.items[idx.0]
    }
}

impl<'a, T> IntoIterator for &'a List<T> {
    type IntoIter = slice::Iter<'a, T>;
    type Item = &'a T;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut List<T> {
    type IntoIter = slice::IterMut<'a, T>;
    type Item = &'a mut T;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Index(usize);

impl Index {
    pub fn new(idx: usize) -> Self {
        Index(idx)
    }

    pub fn value(&self) -> usize {
        self.0
    }
}

impl fmt::Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
