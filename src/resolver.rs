use std::fmt;
use std::result;
use std::collections::HashMap;

use ast::*;
use store::*;

struct Resolver<'a> {
    store: &'a Store,
    locals: Vec<Binding>,
}

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
    for func in &mut func_defs {
        Resolver::new(store).resolve_decl(func)?;
    }
    store.func_defs = func_defs;
    Ok(())
}

pub fn resolve_defs(store: &mut Store) -> Result {
    let mut func_defs = store.func_defs.clone();
    for func in &mut func_defs {
        let mut res = Resolver::new(store);
        res.resolve_def(func)?;
        func.locals = res.locals;
    }
    store.func_defs = func_defs;
    Ok(())
}

impl<'a> Resolver<'a> {
    fn new(store: &'a Store) -> Self {
        Self {
            store,
            locals: Vec::new(),
        }
    }

    fn resolve_decl(&mut self, func: &mut FuncDef) -> Result {
        for param in &mut func.params {
            self.resolve_binding(param)?;
        }
        self.resolve_type(&mut func.ret)
    }

    fn resolve_def(&mut self, func: &mut FuncDef) -> Result {
        let sym = SymTable::root(self.store);
        let mut sym = SymTable::new(&sym);
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
                for stmt in stmts {
                    self.resolve_expr(stmt, &mut sym)?;
                }
            }
            ExprKind::Let {
                ref mut value,
                ref var,
            } => {
                let index = self.locals.len();
                sym.add(var.clone(), Index::new(index));
                self.locals
                    .push(Binding::new(var.clone(), Type::new(TypeKind::Unknown)));
                self.resolve_expr(value, sym)?;
            }
            ExprKind::Function {
                ref mut func,
                ref mut args,
            } => {
                self.resolve_func(func)?;
                for arg in args {
                    self.resolve_expr(arg, sym)?;
                }
            }
            ExprKind::Binary {
                ref mut left,
                ref mut right,
                ..
            } => {
                self.resolve_expr(left, sym)?;
                self.resolve_expr(right, sym)?;
            }
            ExprKind::If {
                ref mut cond,
                ref mut succ,
                ref mut fail,
            } => {
                self.resolve_expr(cond, sym)?;
                self.resolve_expr(succ, sym)?;
                self.resolve_expr(fail, sym)?;
            }
            ExprKind::Id(ref mut path) => {
                self.resolve_id(path, sym)?;
            }
            ExprKind::Int(_) | ExprKind::Noop => {}
        }
        Ok(())
    }

    fn resolve_binding(&self, bind: &mut Binding) -> Result {
        self.resolve_type(&mut bind.tp)
    }

    fn resolve_type(&self, tp: &mut Type) -> Result {
        match tp.kind {
            TypeKind::Named { ref mut path } => {
                path.index = expect_sym!(self.store, path, Type);
            }
            TypeKind::Void | TypeKind::Unknown => {}
        }
        Ok(())
    }

    fn resolve_func(&self, func: &mut Func) -> Result {
        func.path.index = expect_sym!(self.store, &func.path, Func);
        Ok(())
    }

    fn resolve_id(&self, path: &mut Path, sym: &SymTable) -> Result {
        path.index = sym.get(&path.name)
            .ok_or_else(|| Error::SymbolNotFound("id", path.clone()))?;
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
        }
    }
}
