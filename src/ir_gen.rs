use common::*;
use ast::*;
use ir::*;
use ctx::*;

pub fn generate(ctx: &mut Context) {
    for func in &mut ctx.func_defs {
        let ir = Ir {
            blocks: List::new(),
            params: List::new(),
            locals: List::new(),
        };
        let mut gen = Generator { ir };

        // entry block and params
        let mut block = gen.ir.push_block();
        for (i, param) in func.params.iter().enumerate() {
            let idx = Index::new(i);
            let stmt = Stmt::new(StmtKind::Param(idx), param.tp.clone());
            let reg = gen.ir.write(block, stmt);
            gen.ir.params.push(reg);
        }

        let body = &ctx.bodies[func.body.expect("no body")];

        let res = gen.gen_expr(&body.expr, &mut block);
        gen.ir.end(block, Term::Ret(res));

        let idx = ctx.irs.push(gen.ir);
        func.ir = Some(idx);
    }
}

struct Generator {
    ir: Ir,
}

impl Generator {
    fn gen_expr(&mut self, expr: &Expr, block: &mut Index) -> Option<Reg> {
        let kind = match expr.kind {
            ExprKind::Block { ref stmts } => {
                let mut reg = None;
                for stmt in stmts {
                    reg = self.gen_expr(stmt, block);
                }
                return reg;
            }
            ExprKind::Let { ref value, .. } => {
                let reg = self.gen_expr(value, block).unwrap();
                // TODO: this assumes same visiting order - better solution?
                self.ir.locals.push(reg);
                return None;
            }
            ExprKind::Construct { ref tp, ref args } => {
                let args = args.iter()
                    .map(|arg| self.gen_expr(arg, block).unwrap())
                    .collect();
                StmtKind::Construct {
                    tp: tp.clone(),
                    args,
                }
            }
            ExprKind::Function { ref func, ref args } => {
                let args = args.iter()
                    .map(|arg| self.gen_expr(arg, block).unwrap())
                    .collect();
                StmtKind::Call {
                    func: func.clone(),
                    args,
                }
            }
            ExprKind::Member { ref value, ref mem } => {
                let value = self.gen_expr(value, block).unwrap();
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
                let left = self.gen_expr(left, block).unwrap();
                let right = self.gen_expr(right, block).unwrap();
                StmtKind::Binary { op, left, right }
            }
            ExprKind::If {
                ref cond,
                ref succ,
                ref fail,
            } => {
                let mut succ_block = self.ir.push_block();
                let mut fail_block = self.ir.push_block();
                let end_block = self.ir.push_block();

                let cond = self.gen_expr(cond, block).unwrap();
                self.ir.end(
                    *block,
                    Term::Br {
                        cond,
                        succ: succ_block,
                        fail: fail_block,
                    },
                );

                let succ = self.gen_expr(succ, &mut succ_block).unwrap();
                self.ir.end(succ_block, Term::Jump(end_block));

                let fail = self.gen_expr(fail, &mut fail_block).unwrap();
                self.ir.end(fail_block, Term::Jump(end_block));

                *block = end_block;
                StmtKind::Phi {
                    values: vec![(succ, succ_block), (fail, fail_block)],
                }
            }
            ExprKind::Bind { ref bind } => {
                return Some(match bind.kind {
                    BindKind::Param { index } => self.ir.params[index],
                    BindKind::Local { index } => self.ir.locals[index],
                });
            }
            ExprKind::Int { value } => StmtKind::Int(value),
            ExprKind::Noop => {
                return None;
            }
        };

        let stmt = Stmt::new(kind, expr.tp.clone());
        let reg = self.ir.write(*block, stmt);
        Some(reg)
    }
}
