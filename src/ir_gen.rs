use common::*;
use ast::*;
use ir::*;
use ctx::*;

struct Generator {
    ir: Ir,
}

pub fn generate(ctx: &mut Context) {
    for func in &mut ctx.func_defs {
        let mut gen = Generator { ir: Ir::new() };

        // entry block and params
        let mut block = gen.ir.push_block();
        for (i, param) in func.params.iter().enumerate() {
            let idx = Index::new(i);
            let stmt = Stmt::new(StmtKind::Param(idx), param.tp.clone());
            let reg = gen.ir.write(block, stmt);
            gen.ir.locals.push(reg);
        }

        let res = gen.gen(&func.body, &mut block);
        gen.ir.end(block, Term::Ret(res));

        let idx = ctx.irs.push(gen.ir);
        func.ir = Some(idx);
    }
}

impl Generator {
    fn gen(&mut self, expr: &Expr, block: &mut Index) -> Option<Reg> {
        let kind = match expr.kind {
            ExprKind::Block { ref stmts } => {
                let mut reg = None;
                for stmt in stmts {
                    reg = self.gen(stmt, block);
                }
                return reg;
            }
            ExprKind::Let { ref value, .. } => {
                let reg = self.gen(value, block).unwrap();
                // TODO: this assumes same visiting order - better solution?
                self.ir.locals.push(reg);
                return None;
            }
            ExprKind::Construct { ref tp, ref args } => {
                let args = args.iter()
                    .map(|arg| self.gen(arg, block).unwrap())
                    .collect();
                StmtKind::Construct {
                    tp: tp.clone(),
                    args,
                }
            }
            ExprKind::Function { ref func, ref args } => {
                let args = args.iter()
                    .map(|arg| self.gen(arg, block).unwrap())
                    .collect();
                StmtKind::Call {
                    func: func.clone(),
                    args,
                }
            }
            ExprKind::Member { ref value, ref mem } => {
                let value = self.gen(value, block).unwrap();
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
                let left = self.gen(left, block).unwrap();
                let right = self.gen(right, block).unwrap();
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

                let cond = self.gen(cond, block).unwrap();
                self.ir.end(
                    *block,
                    Term::Br {
                        cond,
                        succ: succ_block,
                        fail: fail_block,
                    },
                );

                let succ = self.gen(succ, &mut succ_block).unwrap();
                self.ir.end(succ_block, Term::Jump(end_block));

                let fail = self.gen(fail, &mut fail_block).unwrap();
                self.ir.end(fail_block, Term::Jump(end_block));

                *block = end_block;
                StmtKind::Phi {
                    values: vec![(succ, succ_block), (fail, fail_block)],
                }
            }
            ExprKind::Id(ref bind) => {
                return Some(self.ir.locals[bind.path.index()]);
            }
            ExprKind::Int(val) => StmtKind::Int(val),
            ExprKind::Noop => {
                return None;
            }
        };

        let reg = self.ir.write(*block, Stmt::new(kind, expr.tp.clone()));
        Some(reg)
    }
}
