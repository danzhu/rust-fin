use std::collections::HashMap;
use std::io;

use common::*;
use ctx::*;
use error::*;

#[derive(Clone, Debug)]
pub struct TypeDef {
    pub path: Path,
    pub span: Span,
    pub kind: TypeDefKind,
}

impl Print for TypeDef {
    fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        writeln!(f, "Type {}", self.path)?;
        match self.kind {
            TypeDefKind::Struct { ref fields, .. } => {
                writeln!(f, "Struct")?;
                for field in fields {
                    writeln!(f, "{}{}", INDENT, field.format(ctx))?;
                }
                Ok(())
            }
            TypeDefKind::Builtin(ref tp) => tp.print(f, ctx),
            TypeDefKind::Opaque => writeln!(f, "Opaque"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeDefKind {
    Struct {
        fields: List<BindDef>,
        sym_table: HashMap<String, Index>,
    },
    Builtin(BuiltinType),
    Opaque,
}

#[derive(Clone, Debug)]
pub enum BuiltinType {
    Int,
    Bool,
}

impl Print for BuiltinType {
    fn print<Out>(&self, f: &mut Out, _ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        match *self {
            BuiltinType::Int => writeln!(f, "Int"),
            BuiltinType::Bool => writeln!(f, "Bool"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FuncDef {
    pub path: Path,
    pub params: List<BindDef>,
    pub ret: Type,
    pub span: Span,
    pub body: Option<Index>,
    pub ir: Option<Index>,
}

impl Print for FuncDef {
    fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        writeln!(f, "Function {}", self.path)?;

        writeln!(f, "Params")?;
        for param in &self.params {
            writeln!(f, "{}{}", INDENT, param.format(ctx))?;
        }

        writeln!(f, "Ret {}", self.ret.format(ctx))?;

        if let Some(body) = self.body {
            ctx.bodies[body].print(f, ctx, self)?;
        };

        if let Some(ir) = self.ir {
            ctx.irs[ir].print(f, ctx)?;
        };

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct BindDef {
    pub name: Name,
    pub tp: Type,
    pub span: Span,
}

impl BindDef {
    pub fn format(&self, ctx: &Context) -> String {
        format!("{} {}", self.name, self.tp.format(ctx))
    }
}

#[derive(Clone, Debug)]
pub struct Body {
    pub expr: Expr,
    pub locals: List<BindDef>,
}

impl Body {
    pub fn print<Out>(&self, f: &mut Out, ctx: &Context, def: &FuncDef) -> io::Result<()>
    where
        Out: io::Write,
    {
        writeln!(f, "Locals")?;
        for local in &self.locals {
            writeln!(f, "{}{}", INDENT, local.format(ctx))?;
        }
        self.expr.print(f, ctx, def, self, 0)
    }
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub tp: Type,
    pub span: Span,
    pub kind: ExprKind,
}

impl Expr {
    pub fn for_each<Act, E>(&self, mut act: Act) -> Result<(), E>
    where
        Act: FnMut(&Expr) -> Result<(), E>,
    {
        match self.kind {
            ExprKind::Block { ref stmts } => for stmt in stmts {
                act(stmt)?;
            },
            ExprKind::Let { ref value, .. }
            | ExprKind::Deref { ref value }
            | ExprKind::Member { ref value, .. }
            | ExprKind::Unary { ref value, .. } => {
                act(value)?;
            }
            ExprKind::Assign { ref value, ref var } => {
                act(value)?;
                act(var)?;
            }
            ExprKind::Construct { ref args, .. } | ExprKind::Function { ref args, .. } => {
                for arg in args {
                    act(arg)?;
                }
            }
            ExprKind::Binary {
                ref left,
                ref right,
                ..
            } => {
                act(left)?;
                act(right)?;
            }
            ExprKind::If {
                ref cond,
                ref succ,
                ref fail,
            } => {
                act(cond)?;
                act(succ)?;
                act(fail)?;
            }
            ExprKind::Var { .. }
            | ExprKind::Int { .. }
            | ExprKind::Bind { .. }
            | ExprKind::Noop => {}
        }

        Ok(())
    }

    fn info<Out>(&self, f: &mut Out, ctx: &Context, def: &FuncDef, body: &Body) -> io::Result<()>
    where
        Out: io::Write,
    {
        match self.kind {
            ExprKind::Block { .. } => {
                write!(f, "Block")?;
            }
            ExprKind::Let { ref bind, .. } => {
                write!(f, "Let {}", bind.format(ctx, def, body))?;
            }
            ExprKind::Var { ref tp } => {
                write!(f, "Var {}", tp.format(ctx))?;
            }
            ExprKind::Deref { .. } => {
                write!(f, "Deref")?;
            }
            ExprKind::Assign { .. } => {
                write!(f, "Assign")?;
            }
            ExprKind::Construct { ref tp, .. } => {
                write!(f, "Construct {}", tp.format(ctx))?;
            }
            ExprKind::Function { ref func, .. } => {
                write!(f, "Function {}", func.format(ctx))?;
            }
            ExprKind::Member { ref mem, .. } => {
                write!(f, "Member {}", mem.format(ctx))?;
            }
            ExprKind::Unary { ref op, .. } => {
                write!(f, "Unary {}", op)?;
            }
            ExprKind::Binary { ref op, .. } => {
                write!(f, "Binary {}", op)?;
            }
            ExprKind::If { .. } => {
                write!(f, "If")?;
            }
            ExprKind::Int { value } => {
                write!(f, "Int {}", value)?;
            }
            ExprKind::Bind { ref bind } => {
                write!(f, "Bind {}", bind.format(ctx, def, body))?;
            }
            ExprKind::Noop => {
                write!(f, "Noop")?;
            }
        }

        write!(f, " -> {}", self.tp.format(ctx))
    }

    pub fn print<Out>(
        &self,
        f: &mut Out,
        ctx: &Context,
        def: &FuncDef,
        body: &Body,
        ind: i32,
    ) -> io::Result<()>
    where
        Out: io::Write,
    {
        for _ in 0..ind {
            write!(f, "{}", INDENT)?;
        }

        self.info(f, ctx, def, body)?;
        writeln!(f)?;

        self.for_each(|expr| expr.print(f, ctx, def, body, ind + 1))
    }
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Block {
        stmts: Vec<Expr>,
    },
    Let {
        value: Box<Expr>,
        bind: Bind,
    },
    Var {
        tp: Type,
    },
    Deref {
        value: Box<Expr>,
    },
    Assign {
        value: Box<Expr>,
        var: Box<Expr>,
    },
    Construct {
        tp: Type,
        args: Vec<Expr>,
    },
    Function {
        func: Func,
        args: Vec<Expr>,
    },
    Member {
        value: Box<Expr>,
        mem: Member,
    },
    Unary {
        op: UnaryOp,
        value: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        succ: Box<Expr>,
        fail: Box<Expr>,
    },
    Int {
        value: i32,
    },
    Bind {
        bind: Bind,
    },
    Noop,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    pub kind: TypeKind,
}

impl Type {
    pub fn is_void(&self) -> bool {
        match self.kind {
            TypeKind::Void => true,
            _ => false,
        }
    }

    pub fn format(&self, ctx: &Context) -> String {
        match self.kind {
            TypeKind::Named { index } => format!("{}", ctx.type_defs[index].path),
            TypeKind::Ref { ref tp } => format!("&{}", tp.format(ctx)),
            TypeKind::Void => "Void".to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeKind {
    Named { index: Index },
    Ref { tp: Box<Type> },
    Void,
}

#[derive(Clone, Debug)]
pub struct Func {
    pub index: Index,
}

impl Func {
    pub fn format(&self, ctx: &Context) -> String {
        format!("{}", ctx.func_defs[self.index].path)
    }
}

#[derive(Clone, Debug)]
pub struct Member {
    pub tp: Type,
    pub index: Index,
}

impl Member {
    pub fn format(&self, ctx: &Context) -> String {
        let def = &ctx.get_type(&self.tp);
        match def.kind {
            TypeDefKind::Struct { ref fields, .. } => {
                format!("{}:{}", def.path, fields[self.index].name)
            }
            _ => panic!("member on non-struct type"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Bind {
    pub kind: BindKind,
}

impl Bind {
    pub fn format(&self, _ctx: &Context, _func: &FuncDef, body: &Body) -> String {
        match self.kind {
            BindKind::Local { index } => format!("{}", body.locals[index].name),
        }
    }
}

#[derive(Clone, Debug)]
pub enum BindKind {
    Local { index: Index },
}
