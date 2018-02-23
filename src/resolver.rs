use std::fmt;
use std::result;

use ast::*;
use store::*;

struct Resolver<'a> {
    store: &'a Store,
}

pub enum Error {
    TypeNotFound(Path),
    FuncNotFound(Path),
}

pub type Result<T> = result::Result<T, Error>;

pub fn resolve(store: &mut Store) -> Result<()> {
    store.func_defs = {
        let res = Resolver { store };
        store
            .func_defs
            .clone()
            .into_iter()
            .map(|func| res.resolve(func))
            .collect::<result::Result<Vec<_>, _>>()?
    };
    Ok(())
}

impl<'a> Resolver<'a> {
    fn resolve(&self, func: FuncDef) -> Result<FuncDef> {
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

        let params = func.params
            .into_iter()
            .map(|param| self.resolve_binding(param))
            .collect::<result::Result<Vec<_>, _>>()?;

        let ret = self.resolve_type(func.ret)?;

        Ok(FuncDef {
            body,
            params,
            ret,
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
                let index = self.store
                    .type_index(&path)
                    .ok_or_else(|| Error::TypeNotFound(path.clone()))?;
                TypeKind::Named {
                    path: Path { index, ..path },
                }
            }
            _ => unimplemented!(),
        };
        Ok(Type { kind, ..tp })
    }

    fn resolve_func(&self, func: Func) -> Result<Func> {
        let index = self.store
            .func_index(&func.path)
            .ok_or_else(|| Error::FuncNotFound(func.path.clone()))?;
        Ok(Func {
            path: Path { index, ..func.path },
            ..func
        })
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::TypeNotFound(ref path) => write!(f, "type not found: '{}'", path),
            Error::FuncNotFound(ref path) => write!(f, "func not found: '{}'", path),
        }
    }
}
