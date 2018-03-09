use std::{fmt, result};
use std::collections::HashMap;

use common::*;
use ast::*;
use def::*;

struct Resolver<'a> {
    refs: &'a RefTable,
    locals: List<BindDef>,
}

type RefTable = Store;

struct SymTable<'a> {
    symbols: HashMap<String, Index>,
    parent: SymTableParent<'a>,
}

enum SymTableParent<'a> {
    Table(&'a SymTable<'a>),
    Store(&'a Store),
}

pub enum Error {
    SymbolNotFound(&'static str, Path),
    WrongSymbolKind(&'static str, Symbol),
}

pub type Result = result::Result<(), Error>;

macro_rules! resolve_path {
    ($store:expr, $path:expr, $kind:ident) => {{
        let path: &mut Path = $path;
        *path = match $store.get_sym(path.segs()) {
            Some(Symbol::$kind(idx)) => Path::Resolved(idx),
            Some(sym) => return Err(Error::WrongSymbolKind(stringify!($kind), sym)),
            None => return Err(Error::SymbolNotFound(stringify!($kind), path.clone())),
        };
    }}
}

pub fn resolve_decls(store: &mut Store) -> Result {
    let mut type_defs = store.type_defs.clone();
    let mut func_defs = store.func_defs.clone();
    {
        let refs = &store;
        for tp in &mut type_defs {
            match tp.kind {
                TypeDefKind::Struct { ref mut fields, .. } => for field in fields {
                    resolve_type(&mut field.tp, refs)?;
                },
                TypeDefKind::Builtin(_) => {}
            }
        }
        for func in &mut func_defs {
            for param in &mut func.params {
                resolve_type(&mut param.tp, refs)?;
            }
            resolve_type(&mut func.ret, refs)?;
        }
    }
    store.type_defs = type_defs;
    store.func_defs = func_defs;
    Ok(())
}

pub fn resolve_defs(store: &mut Store) -> Result {
    let mut type_defs = store.type_defs.clone();
    let mut func_defs = store.func_defs.clone();

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
        let mut res = Resolver::new(store);
        res.resolve(func)?;
        func.locals = res.locals;
    }

    store.type_defs = type_defs;
    store.func_defs = func_defs;
    Ok(())
}

fn resolve_type(tp: &mut Type, refs: &RefTable) -> Result {
    match tp.kind {
        TypeKind::Named { ref mut path } => {
            resolve_path!(refs, path, Type);
        }
        TypeKind::Void | TypeKind::Unknown => {}
    }
    Ok(())
}

fn resolve_func(func: &mut Func, refs: &RefTable) -> Result {
    resolve_path!(refs, &mut func.path, Func);
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
                    let bind = BindDef::new(name.clone());
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
                resolve_type(tp, self.refs)?;

                for arg in args {
                    self.resolve_expr(arg, syms)?;
                }
            }
            ExprKind::Function {
                ref mut func,
                ref mut args,
            } => {
                resolve_func(func, self.refs)?;

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
                let idx = syms.get(bind.path.name())
                    .ok_or_else(|| Error::SymbolNotFound("Id", bind.path.clone()))?;
                bind.path = Path::Resolved(idx);
            }
            ExprKind::Int(_) | ExprKind::Noop => {}
        }
        Ok(())
    }
}

impl<'a> SymTable<'a> {
    fn root(store: &'a Store) -> Self {
        SymTable {
            symbols: HashMap::new(),
            parent: SymTableParent::Store(store),
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
            (None, &SymTableParent::Store(_)) => None,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::SymbolNotFound(exp, ref path) => {
                write!(f, "{} symbol not found: '{}'", exp, path)
            }
            Error::WrongSymbolKind(exp, got) => write!(f, "expect {} symbol, got {:?}", exp, got),
        }
    }
}
