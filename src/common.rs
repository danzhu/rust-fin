use std::{fmt, ops, slice, usize};

use ctx::*;

pub const INDENT: &str = "  ";

#[derive(Copy, Clone, Debug)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

#[derive(Copy, Clone, Debug)]
pub struct Pos {
    pub file: Index,
    pub line: usize,
    pub column: usize,
}

#[derive(Copy, Clone, Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Copy, Clone, Debug)]
pub enum BinaryOp {
    Arith(ArithOp),
    Comp(CompOp),
}

#[derive(Copy, Clone, Debug)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Copy, Clone, Debug)]
pub enum CompOp {
    Eq,
    Ne,
    Lt,
    Gt,
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Index(usize);

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

impl Pos {
    pub fn format(&self, ctx: &Context) -> String {
        let filename = &ctx.sources[self.file].filename;
        format!("{}:{}:{}", filename, self.line + 1, self.column + 1)
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UnaryOp::Neg => write!(f, "Neg"),
            UnaryOp::Not => write!(f, "Not"),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BinaryOp::Arith(ArithOp::Add) => write!(f, "Add"),
            BinaryOp::Arith(ArithOp::Sub) => write!(f, "Sub"),
            BinaryOp::Arith(ArithOp::Mul) => write!(f, "Mul"),
            BinaryOp::Arith(ArithOp::Div) => write!(f, "Div"),
            BinaryOp::Arith(ArithOp::Mod) => write!(f, "Mod"),
            BinaryOp::Comp(CompOp::Eq) => write!(f, "Eq"),
            BinaryOp::Comp(CompOp::Ne) => write!(f, "Ne"),
            BinaryOp::Comp(CompOp::Lt) => write!(f, "Lt"),
            BinaryOp::Comp(CompOp::Gt) => write!(f, "Gt"),
        }
    }
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
