use std::io;
use std::collections::HashMap;

use common::*;
use ast::*;
use ctx::*;

#[derive(Clone)]
pub struct Source {
    pub filename: String,
    pub lines: Vec<String>,
    pub defs: Vec<Def>,
}

#[derive(Clone)]
pub struct Def {
    pub kind: DefKind,
}

#[derive(Clone)]
pub enum DefKind {
    Type(Index),
    Func(Index),
}

#[derive(Clone)]
pub struct TypeDef {
    pub name: Name,
    pub span: Span,
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
    pub name: Name,
    pub params: Vec<BindDef>,
    pub ret: Type,
    pub body: Expr,
    pub span: Span,
    pub locals: List<BindDef>,
    pub ir: Option<Index>,
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
    pub fn new<Str>(filename: Str, src: &str) -> Self
    where
        Str: Into<String>,
    {
        Self {
            filename: filename.into(),
            lines: src.lines().map(|s| s.to_string()).collect(),
            defs: Vec::new(),
        }
    }
}

impl Def {
    pub fn new(kind: DefKind) -> Self {
        Self { kind }
    }
}

impl TypeDef {
    pub fn new<Str>(name: Str, span: Span, kind: TypeDefKind) -> Self
    where
        Str: Into<String>,
    {
        Self {
            name: Name::new(name.into()),
            span,
            kind,
        }
    }

    pub fn fields(&self) -> &Vec<BindDef> {
        match self.kind {
            TypeDefKind::Struct { ref fields, .. } => fields,
            TypeDefKind::Builtin(_) => panic!("builtin type has no fields"),
        }
    }

    pub fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        writeln!(f, "Type {}", self.name)?;
        match self.kind {
            TypeDefKind::Struct { ref fields, .. } => {
                writeln!(f, "Struct")?;
                for field in fields {
                    writeln!(f, "{}{}", INDENT, field.format(ctx))?;
                }
                Ok(())
            }
            TypeDefKind::Builtin(ref tp) => tp.print(f, ctx),
        }
    }
}

impl BuiltinType {
    pub fn print<Out>(&self, f: &mut Out, _ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        match *self {
            BuiltinType::Int => writeln!(f, "Int"),
            BuiltinType::Bool => writeln!(f, "Bool"),
        }
    }
}
impl FuncDef {
    pub fn new(name: Name, params: Vec<BindDef>, ret: Type, body: Expr, span: Span) -> Self {
        Self {
            name,
            params,
            ret,
            body,
            span,
            locals: List::new(),
            ir: None,
        }
    }

    pub fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        writeln!(f, "Function {}", self.name)?;

        writeln!(f, "Params")?;
        for param in &self.params {
            writeln!(f, "{}{}", INDENT, param.format(ctx))?;
        }

        writeln!(f, "Ret {}", self.ret.format(ctx))?;

        writeln!(f, "Locals")?;
        for local in &self.locals {
            writeln!(f, "{}{}", INDENT, local.format(ctx))?;
        }

        self.body.print(f, ctx, self, 0)?;

        if let Some(ir) = self.ir {
            ctx.irs[ir].print(f, ctx, self)?;
        }

        Ok(())
    }
}

impl BindDef {
    pub fn new(name: String, tp: Type, span: Span) -> Self {
        Self { name, tp, span }
    }

    pub fn format(&self, ctx: &Context) -> String {
        format!("{} {}", self.name, self.tp.format(ctx))
    }
}

impl Symbol {
    pub fn format(&self, ctx: &Context) -> String {
        match *self {
            Symbol::Type(idx) => format!("type {}", ctx.type_defs[idx].name),
            Symbol::Func(idx) => format!("function {}", ctx.func_defs[idx].name),
        }
    }
}
