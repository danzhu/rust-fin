use std::fmt;
use std::result;

use ast::*;
use store::*;

struct Checker<'a> {
    store: &'a Store,
    locals: &'a mut List<BindDef>,
}

pub enum Error {
    ArgCount { expect: usize, got: usize },
    TypeMismatch { expect: Type, got: Type },
}

pub type Result = result::Result<(), Error>;

pub fn type_check(store: &mut Store) -> Result {
    let mut func_defs = store.func_defs.clone();
    for func in &mut func_defs {
        let mut chk = Checker::new(store, &mut func.locals);
        chk.check(&mut func.body)?;
        expect_tp(&func.ret, &func.body.tp)?;
    }
    store.func_defs = func_defs;
    Ok(())
}

fn expect_tp(expect: &Type, got: &Type) -> Result {
    if expect == got {
        Ok(())
    } else {
        Err(Error::TypeMismatch {
            expect: expect.clone(),
            got: got.clone(),
        })
    }
}

impl<'a> Checker<'a> {
    fn new(store: &'a Store, locals: &'a mut List<BindDef>) -> Self {
        Self { store, locals }
    }

    fn check(&mut self, expr: &mut Expr) -> Result {
        match expr.kind {
            ExprKind::Block { ref mut stmts } => {
                for stmt in stmts.iter_mut() {
                    self.check(stmt)?;
                }

                expr.tp = match stmts.last() {
                    Some(stmt) => stmt.tp.clone(),
                    None => Type::new(TypeKind::Void),
                };
            }
            ExprKind::Let { ref mut value, ref var } => {
                self.check(value)?;

                let def = &mut self.locals[var.path.index];
                def.tp = value.tp.clone();
                expr.tp = Type::new(TypeKind::Void);
            }
            ExprKind::Function {
                ref mut func,
                ref mut args,
            } => {
                let def = &self.store.func_defs[func.path.index];

                if def.params.len() != args.len() {
                    return Err(Error::ArgCount {
                        expect: def.params.len(),
                        got: args.len(),
                    });
                }

                for (param, arg) in def.params.iter().zip(args) {
                    self.check(arg)?;

                    expect_tp(&param.tp, &arg.tp)?;
                }

                expr.tp = def.ret.clone();
            }
            ExprKind::Binary {
                ref mut left,
                ref mut right,
                ..
            } => {
                self.check(left)?;
                self.check(right)?;

                expect_tp(&left.tp, &right.tp)?;
                expr.tp = left.tp.clone();
            }
            ExprKind::If {
                ref mut cond,
                ref mut succ,
                ref mut fail,
            } => {
                self.check(cond)?;
                self.check(succ)?;
                self.check(fail)?;

                expect_tp(&self.store.type_int, &cond.tp)?;
                expect_tp(&succ.tp, &fail.tp)?;
                expr.tp = succ.tp.clone();
            }
            ExprKind::Id(ref mut bind) => {
                expr.tp = self.locals[bind.path.index].tp.clone();
            }
            ExprKind::Int(_) => {
                expr.tp = self.store.type_int.clone();
            }
            ExprKind::Noop => {
                expr.tp = Type::new(TypeKind::Void);
            }
        }
        Ok(())
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::ArgCount { expect, got } => {
                write!(f, "expect {} arguments, got {}", expect, got)
            }
            Error::TypeMismatch {
                ref expect,
                ref got,
            } => write!(f, "expect type {:?}, got {:?}", expect, got),
        }
    }
}
