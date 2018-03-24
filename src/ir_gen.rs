use common::*;
use ast::*;
use ir::*;
use ctx::*;

const NO_REG: &str = "no register";

pub fn generate(ctx: &mut Context) {
    for func in &mut ctx.func_defs {
        let ir = Ir {
            locals: List::new(),
            blocks: List::new(),
        };
        let mut gen = Generator { ir };

        if let Some(body) = func.body {
            let body = &ctx.bodies[body];

            // generate entry block
            let mut block = gen.ir.push_block();
            for local in &body.locals {
                let local = Local { tp: local.tp.clone() };
                gen.ir.locals.push(local);
            }

            // generate body
            let value = gen.gen_expr(&body.expr, &mut block);
            gen.ir.end(block, Term { kind: TermKind::Ret { value } });

            // store ir
            let idx = ctx.irs.push(gen.ir);
            func.ir = Some(idx);
        }
    }
}

struct Generator {
    ir: Ir,
}

impl Generator {
    fn gen_expr(&mut self, expr: &Expr, block: &mut Index) -> Option<Reg> {
        match expr.kind {
            ExprKind::Block { ref stmts } => {
                // TODO: separate last statement in ast
                for stmt in stmts.iter().take(stmts.len() - 1) {
                    self.gen_expr(stmt, block);
                }
                self.gen_expr(stmts.last().expect("no statement in block"), block)
            }
            ExprKind::Let { ref value, ref bind } => {
                let value = self.gen_expr(value, block).expect(NO_REG);
                let dest = match bind.kind {
                    BindKind::Local { index } => Reg::Local(index),
                };

                self.write(*block, StmtKind::Move { dest, value });
                None
            }
            ExprKind::Construct { ref tp, ref args } => {
                let args = args.iter().map(|arg| self.gen_expr(arg, block).expect(NO_REG)).collect();
                let dest = self.temp(&expr.tp);

                self.write(*block, StmtKind::Construct {
                    dest,
                    tp: tp.clone(),
                    args,
                });
                Some(dest)
            }
            ExprKind::Function { ref func, ref args } => {
                let args = args.iter().map(|arg| self.gen_expr(arg, block).expect(NO_REG)).collect();
                let dest = if !expr.tp.is_void() { Some(self.temp(&expr.tp)) } else { None };

                self.write(*block, StmtKind::Call {
                    dest,
                    func: func.clone(),
                    args,
                });
                dest
            }
            ExprKind::Member { ref value, ref mem } => {
                let value = self.gen_expr(value, block).expect(NO_REG);
                let dest = self.temp(&expr.tp);

                self.write(*block, StmtKind::Member {
                    dest,
                    value,
                    mem: mem.clone(),
                });
                Some(dest)
            }
            ExprKind::Binary {
                op,
                ref left,
                ref right,
            } => {
                let left = self.gen_expr(left, block).expect(NO_REG);
                let right = self.gen_expr(right, block).expect(NO_REG);
                let dest = self.temp(&expr.tp);

                self.write(*block, StmtKind::Binary {
                    dest,
                    op,
                    left,
                    right,
                });
                Some(dest)
            }
            ExprKind::If {
                ref cond,
                ref succ,
                ref fail,
            } => {
                let mut succ_block = self.ir.push_block();
                let mut fail_block = self.ir.push_block();
                let end_block = self.ir.push_block();

                let cond = self.gen_expr(cond, block).expect(NO_REG);
                self.ir.end(
                    *block,
                    Term {
                        kind: TermKind::Br {
                            cond,
                            succ: succ_block,
                            fail: fail_block,
                        },
                    },
                );

                let tp = &succ.tp;

                let succ = self.gen_expr(succ, &mut succ_block);
                let fail = self.gen_expr(fail, &mut fail_block);

                let dest = if !tp.is_void() {
                    // we only move the value to the same register if there is actually a value
                    let dest = self.temp(tp);

                    self.ir.write(succ_block, Stmt { kind: StmtKind::Move { dest, value: succ.expect(NO_REG) }});

                    self.ir.write(fail_block, Stmt { kind: StmtKind::Move { dest, value: fail.expect(NO_REG) }});

                    Some(dest)
                } else {
                    None
                };

                self.ir.end(succ_block, Term { kind: TermKind::Jump { block: end_block } });
                self.ir.end(fail_block, Term { kind: TermKind::Jump { block: end_block } });

                *block = end_block;
                dest
            }
            ExprKind::Bind { ref bind } => {
                let dest = match bind.kind {
                    BindKind::Local { index } => Reg::Local(index),
                };

                Some(dest)
            }
            ExprKind::Int { value } => {
                let dest = self.temp(&expr.tp);

                self.write(*block, StmtKind::Int { dest, value });
                Some(dest)
            },
            ExprKind::Noop => {
                None
            }
        }
    }

    fn write(&mut self, block: Index, kind: StmtKind) {
        self.ir.write(block, Stmt { kind });
    }

    fn temp(&mut self, tp: &Type) -> Reg {
        assert!(!tp.is_void());
        Reg::Local(self.ir.locals.push(Local { tp: tp.clone() }))
    }
}
