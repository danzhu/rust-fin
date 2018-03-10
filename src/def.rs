use std::fmt;
use std::collections::HashMap;

use common::*;
use ast::*;
use ir::*;

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
    Struct {
        fields: Vec<BindDef>,
        sym_table: HashMap<String, Index>,
    },
    Builtin(BuiltinType),
}

#[derive(Clone)]
pub enum BuiltinType {
    Int,
    Bool,
}

#[derive(Clone)]
pub struct FuncDef {
    pub name: String,
    pub params: Vec<BindDef>,
    pub ret: Type,
    pub body: Expr,
    pub span: Span,
    pub locals: List<BindDef>,
    pub ir: Ir,
}

#[derive(Clone)]
pub struct BindDef {
    pub name: String,
    pub tp: Type,
    pub span: Span,
}

#[derive(Copy, Clone)]
pub enum Symbol {
    Type(Index),
    Func(Index),
}

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

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Symbol::Func(idx) => write!(f, "function {:?}", idx),
            Symbol::Type(idx) => write!(f, "type {:?}", idx),
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
        writeln!(f, "Type {}", self.name)?;
        match self.kind {
            TypeDefKind::Struct { ref fields, .. } => {
                writeln!(f, "Struct")?;
                for field in fields {
                    writeln!(f, "{}{:?}", INDENT, field)?;
                }
                Ok(())
            }
            TypeDefKind::Builtin(ref tp) => writeln!(f, "{:?}", tp),
        }
    }
}

impl fmt::Debug for BuiltinType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BuiltinType::Int => writeln!(f, "Int"),
            BuiltinType::Bool => writeln!(f, "Bool"),
        }
    }
}
impl FuncDef {
    pub fn new<Str>(name: Str, params: Vec<BindDef>, ret: Type, body: Expr, span: Span) -> Self
    where
        Str: Into<String>,
    {
        Self {
            name: name.into(),
            params,
            ret,
            body,
            span,
            locals: List::new(),
            ir: Ir::new(),
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
        write!(f, "{:?}", self.body)?;
        write!(f, "{:?}", self.ir)
    }
}

impl BindDef {
    pub fn new(name: String, tp: Type, span: Span) -> Self {
        Self { name, tp, span }
    }
}

impl fmt::Debug for BindDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {:?}", self.name, self.tp)
    }
}
