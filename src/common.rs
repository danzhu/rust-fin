use std::fmt;
use std::ops;
use std::slice;
use std::usize;

pub const INDENT: &str = "  ";

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
    Lt,
    Gt,
}

#[derive(Clone)]
pub struct List<T> {
    items: Vec<T>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Index(usize);

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

#[derive(Clone)]
pub struct Bind {
    pub path: Path,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Path {
    pub name: String,
    pub index: Index,
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
            Op::Lt => write!(f, "Lt"),
            Op::Gt => write!(f, "Gt"),
        }
    }
}

impl<T> List<T> {
    pub fn new() -> Self {
        Self { items: Vec::new() }
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

    pub fn new(idx: usize) -> Self {
        Index(idx)
    }
}

impl fmt::Debug for Index {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if *self != Index::UNKNOWN {
            write!(f, "{}", self.0)
        } else {
            write!(f, "?")
        }
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

impl Bind {
    pub fn new(path: Path) -> Self {
        Self { path }
    }
}

impl fmt::Debug for Bind {
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