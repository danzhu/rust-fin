use std::fmt;
use std::result;

use ast::*;
use store::*;

struct Resolver<'a> {
    store: &'a Store,
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
    {
        let res = Resolver { store };
        for func in &mut func_defs {
            res.resolve_decl(func)?;
        }
    }
    store.func_defs = func_defs;
    Ok(())
}

pub fn resolve_defs(store: &mut Store) -> Result {
    let mut func_defs = store.func_defs.clone();
    {
        let res = Resolver { store };
        for func in &mut func_defs {
            res.resolve_def(func)?;
        }
    }
    store.func_defs = func_defs;
    Ok(())
}

impl<'a> Resolver<'a> {
    fn resolve_decl(&self, func: &mut FuncDef) -> Result {
        for param in &mut func.params {
            self.resolve_binding(param)?;
        }
        self.resolve_type(&mut func.ret)?;
        Ok(())
    }

    fn resolve_def(&self, func: &mut FuncDef) -> Result {
        self.resolve_expr(&mut func.body)?;
        Ok(())
    }

    fn resolve_expr(&self, expr: &mut Expr) -> Result {
        match expr.kind {
            ExprKind::Function { ref mut func, .. } => {
                self.resolve_func(func)?;
            }
            _ => {}
        }
        expr.for_each_expr(|expr| self.resolve_expr(expr))?;
        Ok(())
    }

    fn resolve_binding(&self, bind: &mut Binding) -> Result {
        self.resolve_type(&mut bind.tp)?;
        Ok(())
    }

    fn resolve_type(&self, tp: &mut Type) -> Result {
        match tp.kind {
            TypeKind::Named { ref mut path } => {
                path.index = expect_sym!(self.store, path, Type);
            }
            TypeKind::Void => {}
        }
        Ok(())
    }

    fn resolve_func(&self, func: &mut Func) -> Result {
        func.path.index = expect_sym!(self.store, &func.path, Func);
        Ok(())
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
