use std::{fmt, result};

use common::*;
use ast::*;
use ir::*;
use def::*;

struct Generator<'a> {
    func: &'a mut FuncDef,
}

pub enum Error {}

pub type Result<T> = result::Result<T, Error>;

pub fn generate(store: &mut Store) -> Result<()> {
    let mut func_defs = store.func_defs.clone();
    for (orig, func) in store.func_defs.iter().zip(func_defs.iter_mut()) {
        // entry block and params
        let mut block = func.ir.push_block();
        for (i, param) in func.params.iter_mut().enumerate() {
            let idx = Index::new(i);
            let stmt = Stmt::new(StmtKind::Param(idx), param.tp.clone());
            // TODO: find a way to sync params and locals
            func.locals[idx].reg = func.ir.write(block, stmt);
        }

        let res = {
            let mut gen = Generator { func };
            gen.gen(&orig.body, &mut block)?
        };
        func.ir.end(block, Term::Ret(res));
    }
    store.func_defs = func_defs;
    Ok(())
}

impl<'a> Generator<'a> {
    fn gen(&mut self, expr: &Expr, block: &mut Index) -> Result<Reg> {
        let kind = match expr.kind {
            ExprKind::Block { ref stmts } => {
                let mut reg = Reg::NONE;
                for stmt in stmts {
                    reg = self.gen(stmt, block)?;
                }
                return Ok(reg);
            }
            ExprKind::Let { ref value, ref var } => {
                self.func.locals[var.path.index()].reg = self.gen(value, block)?;
                return Ok(Reg::NONE);
            }
            ExprKind::Construct { ref tp, ref args } => {
                let args = args.iter()
                    .map(|arg| self.gen(arg, block))
                    .collect::<result::Result<Vec<_>, _>>()?;
                StmtKind::Construct {
                    tp: tp.clone(),
                    args,
                }
            }
            ExprKind::Function { ref func, ref args } => {
                let args = args.iter()
                    .map(|arg| self.gen(arg, block))
                    .collect::<result::Result<Vec<_>, _>>()?;
                StmtKind::Call {
                    func: func.clone(),
                    args,
                }
            }
            ExprKind::Member { ref value, ref mem } => {
                let value = self.gen(value, block)?;
                StmtKind::Member {
                    value,
                    mem: mem.clone(),
                }
            }
            ExprKind::Binary {
                op,
                ref left,
                ref right,
            } => {
                let left = self.gen(left, block)?;
                let right = self.gen(right, block)?;
                StmtKind::Binary { op, left, right }
            }
            ExprKind::If {
                ref cond,
                ref succ,
                ref fail,
            } => {
                let mut succ_block = self.func.ir.push_block();
                let mut fail_block = self.func.ir.push_block();
                let end_block = self.func.ir.push_block();

                let cond = self.gen(cond, block)?;
                self.func.ir.end(
                    *block,
                    Term::Br {
                        cond,
                        succ: succ_block,
                        fail: fail_block,
                    },
                );

                let succ = self.gen(succ, &mut succ_block)?;
                self.func.ir.end(succ_block, Term::Goto(end_block));

                let fail = self.gen(fail, &mut fail_block)?;
                self.func.ir.end(fail_block, Term::Goto(end_block));

                *block = end_block;
                StmtKind::Phi {
                    values: vec![(succ, succ_block), (fail, fail_block)],
                }
            }
            ExprKind::Id(ref bind) => {
                return Ok(self.func.locals[bind.path.index()].reg);
            }
            ExprKind::Int(val) => StmtKind::Int(val),
            ExprKind::Noop => {
                return Ok(Reg::NONE);
            }
        };

        Ok(self.func.ir.write(*block, Stmt::new(kind, expr.tp.clone())))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        Ok(())
    }
}
