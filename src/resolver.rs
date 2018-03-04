use std::fmt;
use std::result;
use std::collections::HashMap;

use ast::*;
use store::*;

struct Resolver<'a> {
    store: &'a Store,
    refs: &'a RefTable,
    locals: Vec<Binding>,
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
    TypeMismatch(Type, Type),
}

pub type Result = result::Result<(), Error>;

macro_rules! expect_sym {
    ($store:expr, $path:expr, $kind:ident) => {{
        let path: &Path = $path;
        match $store.get_sym(path) {
            Some(Symbol::$kind(idx)) => idx,
            Some(sym) => return Err(Error::WrongSymbolKind(stringify!($kind), sym)),
            None => return Err(Error::SymbolNotFound(stringify!($kind), path.clone())),
        }
    }}
}

pub fn resolve_decls(store: &mut Store) -> Result {
    let mut func_defs = store.func_defs.clone();
    {
        let refs = &store;
        for func in &mut func_defs {
            for param in &mut func.params {
                resolve_binding(param, refs)?;
            }
            resolve_type(&mut func.ret, refs)?;
        }
    }
    store.func_defs = func_defs;
    Ok(())
}

pub fn resolve_defs(store: &mut Store) -> Result {
    let mut func_defs = store.func_defs.clone();
    for func in &mut func_defs {
        let mut res = Resolver::new(store, store);
        res.resolve(func)?;
        func.locals = res.locals;
    }
    store.func_defs = func_defs;
    Ok(())
}

fn resolve_binding(bind: &mut Binding, refs: &RefTable) -> Result {
    resolve_type(&mut bind.tp, refs)
}

fn resolve_type(tp: &mut Type, refs: &RefTable) -> Result {
    match tp.kind {
        TypeKind::Named { ref mut path } => {
            path.index = expect_sym!(refs, path, Type);
        }
        TypeKind::Void | TypeKind::Unknown => {}
    }
    Ok(())
}

fn resolve_func(func: &mut Func, refs: &RefTable) -> Result {
    func.path.index = expect_sym!(refs, &func.path, Func);
    Ok(())
}

impl<'a> Resolver<'a> {
    fn new(store: &'a Store, refs: &'a RefTable) -> Self {
        Self {
            store,
            refs,
            locals: Vec::new(),
        }
    }

    fn resolve(&mut self, func: &mut FuncDef) -> Result {
        let mut sym = SymTable::root(self.refs);
        for param in &mut func.params {
            let index = self.locals.len();
            sym.add(param.name.clone(), Index::new(index));
            self.locals.push(param.clone());
        }
        self.resolve_expr(&mut func.body, &mut sym)
    }

    fn resolve_expr(&mut self, expr: &mut Expr, sym: &mut SymTable) -> Result {
        match expr.kind {
            ExprKind::Block { ref mut stmts } => {
                let mut sym = SymTable::new(sym);
                for stmt in stmts.iter_mut() {
                    self.resolve_expr(stmt, &mut sym)?;
                }

                expr.tp = match stmts.last() {
                    Some(stmt) => stmt.tp.clone(),
                    None => Type::new(TypeKind::Void),
                };
            }
            ExprKind::Let {
                ref mut value,
                ref var,
            } => {
                self.resolve_expr(value, sym)?;

                let index = self.locals.len();
                sym.add(var.clone(), Index::new(index));
                self.locals
                    .push(Binding::new(var.clone(), value.tp.clone()));

                expr.tp = Type::new(TypeKind::Void);
            }
            ExprKind::Function {
                ref mut func,
                ref mut args,
            } => {
                resolve_func(func, self.refs)?;
                for arg in args {
                    self.resolve_expr(arg, sym)?;
                }

                expr.tp = self.store.func_defs[func.path.index.value()].ret.clone();
            }
            ExprKind::Binary {
                ref mut left,
                ref mut right,
                ..
            } => {
                self.resolve_expr(left, sym)?;
                self.resolve_expr(right, sym)?;

                if left.tp != right.tp {
                    return Err(Error::TypeMismatch(left.tp.clone(), right.tp.clone()));
                }
                expr.tp = left.tp.clone();
            }
            ExprKind::If {
                ref mut cond,
                ref mut succ,
                ref mut fail,
            } => {
                self.resolve_expr(cond, sym)?;
                self.resolve_expr(succ, sym)?;
                self.resolve_expr(fail, sym)?;

                if succ.tp != fail.tp {
                    return Err(Error::TypeMismatch(succ.tp.clone(), fail.tp.clone()));
                }
                expr.tp = succ.tp.clone();
            }
            ExprKind::Id(ref mut path) => {
                path.index = sym.get(&path.name)
                    .ok_or_else(|| Error::SymbolNotFound("Id", path.clone()))?;

                expr.tp = self.locals[path.index.value()].tp.clone();
            }
            ExprKind::Int(_) => {
                expr.tp = self.refs.type_int.clone();
            }
            ExprKind::Noop => {}
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
        if let Some(&idx) = self.symbols.get(name) {
            return Some(idx);
        }

        if let SymTableParent::Table(parent) = self.parent {
            return parent.get(name);
        }

        None
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::SymbolNotFound(exp, ref path) => write!(f, "{} not found: '{}'", exp, path),
            Error::WrongSymbolKind(exp, got) => write!(f, "expect {}, got {:?}", exp, got),
            Error::TypeMismatch(ref left, ref right) => {
                write!(f, "{:?} and {:?} type mismatch", left, right)
            }
        }
    }
}
