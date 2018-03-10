use std::{fmt, ops, slice, usize};

pub const INDENT: &str = "  ";

#[derive(Copy, Clone, Debug)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

#[derive(Copy, Clone, Debug)]
pub struct Pos {
    pub line: i32,
    pub column: i32,
}

#[derive(Copy, Clone)]
pub enum Op {
    Arith(ArithOp),
    Comp(CompOp),
}

#[derive(Copy, Clone)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Copy, Clone)]
pub enum CompOp {
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
pub struct Member {
    pub path: Path,
}

#[derive(Clone)]
pub struct Bind {
    pub path: Path,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Path {
    Unresolved(Segs),
    Resolved(Index),
}

// TODO: remove Eq and add custom impl for Path
#[derive(Clone, PartialEq, Eq)]
pub struct Segs {
    pub name: String,
}

impl Span {
    pub const ZERO: Span = Span {
        start: Pos::ZERO,
        end: Pos::ZERO,
    };

    pub fn new(start: Pos, end: Pos) -> Self {
        Span { start, end }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} - {}", self.start, self.end)
    }
}

impl Pos {
    pub const ZERO: Pos = Pos { line: 0, column: 0 };
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Op::Arith(ArithOp::Add) => write!(f, "Add"),
            Op::Arith(ArithOp::Sub) => write!(f, "Sub"),
            Op::Arith(ArithOp::Mul) => write!(f, "Mul"),
            Op::Arith(ArithOp::Div) => write!(f, "Div"),
            Op::Arith(ArithOp::Mod) => write!(f, "Mod"),
            Op::Comp(CompOp::Eq) => write!(f, "Eq"),
            Op::Comp(CompOp::Ne) => write!(f, "Ne"),
            Op::Comp(CompOp::Lt) => write!(f, "Lt"),
            Op::Comp(CompOp::Gt) => write!(f, "Gt"),
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

impl fmt::Debug for Index {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Type {
    pub fn new(kind: TypeKind) -> Self {
        Self { kind }
    }

    pub fn path(&self) -> &Path {
        match self.kind {
            TypeKind::Named { ref path } => path,
            TypeKind::Void | TypeKind::Unknown => panic!("type with no path"),
        }
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

impl Member {
    pub fn new(path: Path) -> Self {
        Self { path }
    }
}

impl fmt::Debug for Member {
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
        Path::Unresolved(Segs { name: name.into() })
    }

    pub fn name(&self) -> &String {
        &self.segs().name
    }

    pub fn segs(&self) -> &Segs {
        match *self {
            Path::Unresolved(ref segs) => segs,
            Path::Resolved(_) => panic!("calling segs on resolved path"),
        }
    }

    pub fn index(&self) -> Index {
        match *self {
            Path::Unresolved(_) => panic!("calling index on unresolved path"),
            Path::Resolved(idx) => idx,
        }
    }
}

// TODO: change to Debug
impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Path::Unresolved(ref segs) => write!(f, "{:?}", segs.name),
            Path::Resolved(idx) => write!(f, "#{:?}", idx),
        }
    }
}
