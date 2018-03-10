use std::{fmt, result};
use std::collections::HashMap;

use common::*;
use ast::*;
use def::*;
use ctx::*;

struct Resolver<'a> {
    refs: &'a RefTable,
    locals: List<BindDef>,
}

type RefTable = Context;

struct SymTable<'a> {
    symbols: HashMap<String, Index>,
    parent: SymTableParent<'a>,
}

enum SymTableParent<'a> {
    Table(&'a SymTable<'a>),
    Context(&'a Context),
}

pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

pub enum ErrorKind {
    SymbolNotFound(&'static str, Path),
    WrongSymbolKind(&'static str, Symbol),
}

pub type Result = result::Result<(), Error>;

macro_rules! resolve_path {
    ($ctx:expr, $path:expr, $span:expr, $kind:ident) => {{
        let ctx: &Context = $ctx;
        let path: &mut Path = $path;
        let span: Span = $span;
        *path = match ctx.get_sym(path.segs()) {
            Some(Symbol::$kind(idx)) => Path::Resolved(idx),
            Some(sym) => return Err(Error{
                kind: ErrorKind::WrongSymbolKind(stringify!($kind), sym),
                span,
            }),
            None => return Err(Error {
                kind: ErrorKind::SymbolNotFound(stringify!($kind), path.clone()),
                span,
            }),
        };
    }}
}

pub fn resolve_decls(ctx: &mut Context) -> Result {
    let mut type_defs = ctx.type_defs.clone();
    let mut func_defs = ctx.func_defs.clone();
    {
        let refs = &ctx;
        for tp in &mut type_defs {
            match tp.kind {
                TypeDefKind::Struct { ref mut fields, .. } => for field in fields {
                    resolve_type(&mut field.tp, field.span, refs)?;
                },
                TypeDefKind::Builtin(_) => {}
            }
        }
        for func in &mut func_defs {
            for param in &mut func.params {
                resolve_type(&mut param.tp, param.span, refs)?;
            }
            // TODO: use span of return type, not function
            resolve_type(&mut func.ret, func.span, refs)?;
        }
    }
    ctx.type_defs = type_defs;
    ctx.func_defs = func_defs;
    Ok(())
}

pub fn resolve_defs(ctx: &mut Context) -> Result {
    let mut type_defs = ctx.type_defs.clone();
    let mut func_defs = ctx.func_defs.clone();

    for tp in &mut type_defs {
        match tp.kind {
            TypeDefKind::Struct {
                ref fields,
                ref mut sym_table,
            } => for (i, field) in fields.iter().enumerate() {
                sym_table.insert(field.name.clone(), Index::new(i));
            },
            TypeDefKind::Builtin(_) => {}
        }
    }

    for func in &mut func_defs {
        let mut res = Resolver::new(ctx);
        res.resolve(func)?;
        func.locals = res.locals;
    }

    ctx.type_defs = type_defs;
    ctx.func_defs = func_defs;
    Ok(())
}

fn resolve_type(tp: &mut Type, span: Span, refs: &RefTable) -> Result {
    match tp.kind {
        TypeKind::Named { ref mut path } => {
            resolve_path!(refs, path, span, Type);
        }
        TypeKind::Void | TypeKind::Unknown => {}
    }
    Ok(())
}

fn resolve_func(func: &mut Func, span: Span, refs: &RefTable) -> Result {
    resolve_path!(refs, &mut func.path, span, Func);
    Ok(())
}

impl<'a> Resolver<'a> {
    fn new(refs: &'a RefTable) -> Self {
        Self {
            refs,
            locals: List::new(),
        }
    }

    fn resolve(&mut self, func: &mut FuncDef) -> Result {
        let mut syms = SymTable::root(self.refs);
        for param in &mut func.params {
            let idx = self.locals.push(param.clone());
            syms.add(param.name.clone(), idx);
        }

        self.resolve_expr(&mut func.body, &mut syms)
    }

    fn resolve_expr(&mut self, expr: &mut Expr, syms: &mut SymTable) -> Result {
        match expr.kind {
            ExprKind::Block { ref mut stmts } => for stmt in stmts.iter_mut() {
                self.resolve_expr(stmt, syms)?;
            },
            ExprKind::Let {
                ref mut value,
                ref mut var,
            } => {
                self.resolve_expr(value, syms)?;

                let idx = {
                    let name = var.path.name();
                    let tp = Type::new(TypeKind::Unknown);
                    let bind = BindDef::new(name.clone(), tp, expr.span);
                    let idx = self.locals.push(bind);
                    syms.add(name.clone(), idx);
                    idx
                };
                var.path = Path::Resolved(idx);
            }
            ExprKind::Construct {
                ref mut tp,
                ref mut args,
            } => {
                resolve_type(tp, expr.span, self.refs)?;

                for arg in args {
                    self.resolve_expr(arg, syms)?;
                }
            }
            ExprKind::Function {
                ref mut func,
                ref mut args,
            } => {
                resolve_func(func, expr.span, self.refs)?;

                for arg in args {
                    self.resolve_expr(arg, syms)?;
                }
            }
            ExprKind::Member { ref mut value, .. } => {
                self.resolve_expr(value, syms)?;
            }
            ExprKind::Binary {
                ref mut left,
                ref mut right,
                ..
            } => {
                self.resolve_expr(left, syms)?;
                self.resolve_expr(right, syms)?;
            }
            ExprKind::If {
                ref mut cond,
                ref mut succ,
                ref mut fail,
            } => {
                self.resolve_expr(cond, syms)?;

                let mut succ_syms = SymTable::new(syms);
                self.resolve_expr(succ, &mut succ_syms)?;

                let mut fail_syms = SymTable::new(syms);
                self.resolve_expr(fail, &mut fail_syms)?;
            }
            ExprKind::Id(ref mut bind) => {
                let idx = match syms.get(bind.path.name()) {
                    Some(idx) => idx,
                    None => {
                        return Err(Error {
                            kind: ErrorKind::SymbolNotFound("Id", bind.path.clone()),
                            span: expr.span,
                        });
                    }
                };
                bind.path = Path::Resolved(idx);
            }
            ExprKind::Int(_) | ExprKind::Noop => {}
        }
        Ok(())
    }
}

impl<'a> SymTable<'a> {
    fn root(ctx: &'a Context) -> Self {
        SymTable {
            symbols: HashMap::new(),
            parent: SymTableParent::Context(ctx),
        }
    }

    fn new(parent: &'a SymTable) -> Self {
        SymTable {
            symbols: HashMap::new(),
            parent: SymTableParent::Table(parent),
        }
    }

    fn add(&mut self, name: String, index: Index) {
        self.symbols.insert(name, index);
    }

    fn get(&self, name: &str) -> Option<Index> {
        match (self.symbols.get(name), &self.parent) {
            (Some(&idx), _) => Some(idx),
            (None, &SymTableParent::Table(parent)) => parent.get(name),
            (None, &SymTableParent::Context(_)) => None,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: error: ", self.span.start)?;
        match self.kind {
            ErrorKind::SymbolNotFound(exp, ref path) => {
                write!(f, "{} symbol {} not found", exp, path)
            }
            ErrorKind::WrongSymbolKind(exp, got) => {
                write!(f, "expect {} symbol, got {:?}", exp, got)
            }
        }
    }
}
