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

pub type Result<T> = result::Result<T, Error>;

macro_rules! expect_sym {
    ($store:expr, $path:expr, $kind:ident) => {{
        let path = $path;
        match $store.get_sym(path) {
            Some(Symbol::$kind(idx)) => idx,
            Some(sym) => return Err(Error::WrongSymbolKind(stringify!($kind), sym)),
            None => return Err(Error::SymbolNotFound(stringify!($kind), path.clone())),
        }
    }}
}

pub fn resolve_decls(store: &mut Store) -> Result<()> {
    store.func_defs = {
        let res = Resolver { store };
        store
            .func_defs
            .clone()
            .into_iter()
            .map(|func| res.resolve_decl(func))
            .collect::<result::Result<Vec<_>, _>>()?
    };
    Ok(())
}

pub fn resolve_defs(store: &mut Store) -> Result<()> {
    store.func_defs = {
        let res = Resolver { store };
        store
            .func_defs
            .clone()
            .into_iter()
            .map(|func| res.resolve_def(func))
            .collect::<result::Result<Vec<_>, _>>()?
    };
    Ok(())
}

impl<'a> Resolver<'a> {
    fn resolve_decl(&self, func: FuncDef) -> Result<FuncDef> {
        let params = func.params
            .into_iter()
            .map(|param| self.resolve_binding(param))
            .collect::<result::Result<Vec<_>, _>>()?;

        let ret = self.resolve_type(func.ret)?;

        Ok(FuncDef {
            params,
            ret,
            ..func
        })
    }

    fn resolve_def(&self, func: FuncDef) -> Result<FuncDef> {
        let body = func.body.map_pre(&|expr: Expr| {
            let kind = match expr.kind {
                ExprKind::Function { func, args } => ExprKind::Function {
                    func: self.resolve_func(func)?,
                    args,
                },
                kind => kind,
            };
            Ok(Expr { kind, ..expr })
        })?;

        Ok(FuncDef {
            body,
            ..func
        })
    }

    fn resolve_binding(&self, bind: Binding) -> Result<Binding> {
        Ok(Binding {
            tp: self.resolve_type(bind.tp)?,
            ..bind
        })
    }

    fn resolve_type(&self, tp: Type) -> Result<Type> {
        let kind = match tp.kind {
            TypeKind::Named { path } => {
                let index = expect_sym!(self.store, &path, Type);
                TypeKind::Named {
                    path: Path { index, ..path },
                }
            }
            _ => unimplemented!(),
        };
        Ok(Type { kind, ..tp })
    }

    fn resolve_func(&self, func: Func) -> Result<Func> {
        let index = expect_sym!(self.store, &func.path, Func);
        Ok(Func {
            path: Path { index, ..func.path },
            ..func
        })
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
