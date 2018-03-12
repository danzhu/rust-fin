use std::{io, result};

use common::*;
use ast::*;
use def::*;
use ctx::*;

struct Checker<'a> {
    ctx: &'a Context,
    locals: &'a mut List<BindDef>,
}

pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

pub enum ErrorKind {
    ArgCount { expect: usize, got: usize },
    TypeMismatch { expect: Type, got: Type },
    ConstructPrimitive { tp: Type },
    MemberNotFound { tp: Type, mem: Member },
}

pub type Result = result::Result<(), Error>;

pub fn type_check(ctx: &mut Context) -> Result {
    let mut func_defs = ctx.func_defs.clone();
    for func in &mut func_defs {
        let mut chk = Checker::new(ctx, &mut func.locals);
        chk.check(&mut func.body)?;
        expect_tp(&func.ret, &func.body.tp, func.span)?;
    }
    ctx.func_defs = func_defs;
    Ok(())
}

fn expect_tp(expect: &Type, got: &Type, span: Span) -> Result {
    if expect == got {
        Ok(())
    } else {
        Err(Error {
            kind: ErrorKind::TypeMismatch {
                expect: expect.clone(),
                got: got.clone(),
            },
            span,
        })
    }
}

impl<'a> Checker<'a> {
    fn new(ctx: &'a Context, locals: &'a mut List<BindDef>) -> Self {
        Self { ctx, locals }
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
            ExprKind::Let {
                ref mut value,
                ref var,
            } => {
                self.check(value)?;

                let def = &mut self.locals[var.path.index()];
                def.tp = value.tp.clone();
                expr.tp = Type::new(TypeKind::Void);
            }
            ExprKind::Construct {
                ref mut tp,
                ref mut args,
            } => {
                let path = match tp.kind {
                    TypeKind::Named { ref path } => path,
                    _ => panic!("construction of non-named type"),
                };

                let def = &self.ctx.type_defs[path.index()];
                let fields = match def.kind {
                    TypeDefKind::Struct { ref fields, .. } => fields,
                    TypeDefKind::Builtin(_) => {
                        return Err(Error {
                            kind: ErrorKind::ConstructPrimitive { tp: tp.clone() },
                            span: expr.span,
                        });
                    }
                };

                if fields.len() != args.len() {
                    return Err(Error {
                        kind: ErrorKind::ArgCount {
                            expect: fields.len(),
                            got: args.len(),
                        },
                        span: expr.span,
                    });
                }

                for (field, arg) in fields.iter().zip(args) {
                    self.check(arg)?;

                    expect_tp(&field.tp, &arg.tp, arg.span)?;
                }

                expr.tp = tp.clone();
            }
            ExprKind::Function {
                ref mut func,
                ref mut args,
            } => {
                let def = &self.ctx.get_func(func);

                if def.params.len() != args.len() {
                    return Err(Error {
                        kind: ErrorKind::ArgCount {
                            expect: def.params.len(),
                            got: args.len(),
                        },
                        span: expr.span,
                    });
                }

                for (param, arg) in def.params.iter().zip(args) {
                    self.check(arg)?;

                    expect_tp(&param.tp, &arg.tp, arg.span)?;
                }

                expr.tp = def.ret.clone();
            }
            ExprKind::Member {
                ref mut value,
                ref mut mem,
            } => {
                self.check(value)?;

                let def = &self.ctx.get_type(&value.tp);
                match def.kind {
                    TypeDefKind::Struct {
                        ref fields,
                        ref sym_table,
                    } => {
                        let idx = match sym_table.get(mem.path.name()) {
                            Some(&idx) => idx,
                            None => {
                                return Err(Error {
                                    kind: ErrorKind::MemberNotFound {
                                        tp: value.tp.clone(),
                                        mem: mem.clone(),
                                    },
                                    span: expr.span,
                                })
                            }
                        };
                        mem.path = Path::Resolved(idx);
                        expr.tp = fields[idx.value()].tp.clone();
                    }
                    TypeDefKind::Builtin(_) => {
                        return Err(Error {
                            kind: ErrorKind::MemberNotFound {
                                tp: value.tp.clone(),
                                mem: mem.clone(),
                            },
                            span: expr.span,
                        });
                    }
                }
            }
            ExprKind::Binary {
                op,
                ref mut left,
                ref mut right,
            } => {
                self.check(left)?;
                self.check(right)?;

                expect_tp(&left.tp, &right.tp, expr.span)?;
                expr.tp = match op {
                    Op::Arith(_) => left.tp.clone(),
                    Op::Comp(_) => self.ctx.type_bool.clone(),
                };
            }
            ExprKind::If {
                ref mut cond,
                ref mut succ,
                ref mut fail,
            } => {
                self.check(cond)?;
                self.check(succ)?;
                self.check(fail)?;

                expect_tp(&self.ctx.type_bool, &cond.tp, expr.span)?;
                expect_tp(&succ.tp, &fail.tp, expr.span)?;
                expr.tp = succ.tp.clone();
            }
            ExprKind::Id(ref mut bind) => {
                expr.tp = self.locals[bind.path.index()].tp.clone();
            }
            ExprKind::Int(_) => {
                expr.tp = self.ctx.type_int.clone();
            }
            ExprKind::Noop => {
                expr.tp = Type::new(TypeKind::Void);
            }
        }
        Ok(())
    }
}

impl Error {
    pub fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        write!(f, "{}: error: ", self.span.start.format(ctx))?;
        match self.kind {
            ErrorKind::ArgCount { expect, got } => {
                write!(f, "expect {} arguments, got {}", expect, got)
            }
            ErrorKind::TypeMismatch {
                ref expect,
                ref got,
            } => write!(
                f,
                "expect type {}, got {}",
                expect.format(ctx),
                got.format(ctx)
            ),
            ErrorKind::ConstructPrimitive { ref tp } => {
                write!(f, "attempt to construct primitive type {}", tp.format(ctx))
            }
            ErrorKind::MemberNotFound { ref tp, ref mem } => write!(
                f,
                "type {} doesn't have member {}",
                tp.format(ctx),
                mem.format(ctx, tp)
            ),
        }
    }
}
