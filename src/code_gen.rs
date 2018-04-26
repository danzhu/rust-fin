use ast::*;
use common::*;
use ctx::*;
use ir::*;
use llvm;

pub fn generate(ctx: &Context) -> llvm::Module {
    let mut items = Vec::new();

    for tp in &ctx.type_defs {
        if let Some(tp) = gen_type(ctx, tp) {
            items.push(llvm::Item {
                kind: llvm::ItemKind::Type(tp),
            });
        }
    }

    for func in &ctx.func_defs {
        let kind = if let Some(ir) = func.ir {
            let ir = &ctx.irs[ir];

            let mut gen = FuncGen::new(ctx, func, ir);
            gen.gen();
            llvm::ItemKind::Define(gen.def)
        } else {
            llvm::ItemKind::Declare(gen_decl(ctx, func))
        };

        items.push(llvm::Item { kind });
    }

    llvm::Module { items }
}

fn gen_type(ctx: &Context, tp: &TypeDef) -> Option<llvm::TypeDef> {
    match tp.kind {
        TypeDefKind::Struct { ref fields, .. } => {
            let name = typedef_name(tp);
            let fields = fields
                .iter()
                .map(|field| type_name(ctx, &field.tp))
                .collect();
            Some(llvm::TypeDef {
                name,
                kind: llvm::TypeDefKind::Struct { fields },
            })
        }
        TypeDefKind::Builtin(_) => None,
        TypeDefKind::Opaque => panic!("generating opaque type"),
    }
}

struct FuncGen<'a> {
    ctx: &'a Context,
    func: &'a FuncDef,
    ir: &'a Ir,
    def: llvm::FuncDef,
    vars: Vec<llvm::Value>,
    regs: Vec<llvm::Value>,
}

impl<'a> FuncGen<'a> {
    fn new(ctx: &'a Context, func: &'a FuncDef, ir: &'a Ir) -> Self {
        let decl = gen_decl(ctx, func);

        let def = llvm::FuncDef::new(decl);

        FuncGen {
            ctx,
            func,
            ir,
            def,
            vars: Vec::new(),
            regs: Vec::new(),
        }
    }

    fn gen(&mut self) {
        assert!(!self.ir.blocks.is_empty());

        let mut first = true;
        for block in &self.ir.blocks {
            let mut bb = self.def.add_block();

            if first {
                first = false;

                self.def.comment(bb, "variables");
                self.vars = self.ir
                    .vars
                    .iter()
                    .map(|var| {
                        let tp = type_name(self.ctx, &var.tp);
                        self.def.write(bb, llvm::InstrKind::Alloca { tp })
                    })
                    .collect();

                self.def.comment(bb, "registers");
                self.regs = self.ir
                    .regs
                    .iter()
                    .map(|reg| {
                        let tp = type_name(self.ctx, &reg.tp);
                        self.def.write(bb, llvm::InstrKind::Alloca { tp })
                    })
                    .collect();
            }

            for stmt in &block.stmts {
                self.gen_stmt(stmt, bb);
            }

            self.gen_term(&block.term, bb);
        }
    }

    fn gen_stmt(&mut self, stmt: &Stmt, bb: llvm::Block) {
        match stmt.kind {
            StmtKind::Param { dest, param } => {
                let def = &self.func.params[param];
                let tp = type_name(self.ctx, &def.tp);
                let value = llvm::Value::Param(param.value());
                let addr = self.reg_addr(dest);

                let instr = llvm::InstrKind::Store { tp, value, addr };

                self.def.write(bb, instr);
            }
            StmtKind::Var { dest, var } => {
                let def = &self.ir.vars[var.index];
                let tp = llvm::Type::Pointer(Box::new(type_name(self.ctx, &def.tp)));
                let value = self.vars[var.index.value()].clone();
                let addr = self.reg_addr(dest);

                let instr = llvm::InstrKind::Store { tp, value, addr };

                self.def.write(bb, instr);
            }
            StmtKind::Move { dest, value } => {
                let val = self.load(value, bb);
                self.store(val, dest, bb);
            }
            StmtKind::Load { dest, value } => {
                let tp = self.reg_type(dest);
                let addr = self.load(value, bb);

                let instr = llvm::InstrKind::Load { tp, addr };
                let res = self.def.write(bb, instr);

                self.store(res, dest, bb);
            }
            StmtKind::Store { value, var } => {
                let tp = self.reg_type(value);
                let value = self.load(value, bb);
                let addr = self.load(var, bb);

                let instr = llvm::InstrKind::Store { tp, value, addr };
                self.def.write(bb, instr);
            }
            StmtKind::Unary { dest, op, value } => {
                let tp = self.reg_type(value);
                let value = self.load(value, bb);

                let instr = match op {
                    UnaryOp::Neg => llvm::InstrKind::Arith {
                        op: llvm::ArithOp::Sub,
                        tp,
                        left: llvm::Value::Int(0),
                        right: value,
                    },
                    UnaryOp::Not => llvm::InstrKind::Arith {
                        op: llvm::ArithOp::Xor,
                        tp,
                        left: value,
                        right: llvm::Value::Bool(true),
                    },
                };
                let val = self.def.write(bb, instr);

                self.store(val, dest, bb);
            }
            StmtKind::Binary {
                dest,
                op,
                left,
                right,
            } => {
                let tp = self.reg_type(left);
                let left = self.load(left, bb);
                let right = self.load(right, bb);

                let instr = match op {
                    BinaryOp::Arith(op) => llvm::InstrKind::Arith {
                        op: match op {
                            ArithOp::Add => llvm::ArithOp::Add,
                            ArithOp::Sub => llvm::ArithOp::Sub,
                            ArithOp::Mul => llvm::ArithOp::Mul,
                            ArithOp::Div => llvm::ArithOp::Sdiv,
                            ArithOp::Mod => llvm::ArithOp::Srem,
                        },
                        tp,
                        left,
                        right,
                    },
                    BinaryOp::Comp(op) => llvm::InstrKind::Icmp {
                        op: match op {
                            CompOp::Eq => llvm::IcmpOp::Eq,
                            CompOp::Ne => llvm::IcmpOp::Ne,
                            CompOp::Lt => llvm::IcmpOp::Slt,
                            CompOp::Gt => llvm::IcmpOp::Sgt,
                        },
                        tp,
                        left,
                        right,
                    },
                };
                let val = self.def.write(bb, instr);

                self.store(val, dest, bb);
            }
            StmtKind::Construct {
                dest,
                ref tp,
                ref args,
            } => {
                let dest = self.reg_addr(dest);
                let tp = type_name(self.ctx, tp);

                for (idx, &arg) in args.iter().enumerate() {
                    let arg_tp = self.reg_type(arg);
                    let arg = self.load(arg, bb);

                    let instr = llvm::InstrKind::GetElementPtr {
                        tp: tp.clone(),
                        ptr: dest.clone(),
                        offsets: vec![
                            (llvm::Type::Builtin("i64"), llvm::Value::Int(0)),
                            (llvm::Type::Builtin("i32"), llvm::Value::Int(idx as i32)),
                        ],
                    };
                    let field = self.def.write(bb, instr);

                    let instr = llvm::InstrKind::Store {
                        tp: arg_tp,
                        value: arg,
                        addr: field,
                    };
                    self.def.write(bb, instr);
                }
            }
            StmtKind::Call {
                dest,
                ref func,
                ref args,
            } => {
                let func = &self.ctx.get_func(func);
                let name = llvm::Value::Func(func.path.to_string());
                let tp = type_name(self.ctx, &func.ret);

                let args = args.iter()
                    .map(|&arg| {
                        let tp = self.reg_type(arg);
                        let val = self.load(arg, bb);
                        (tp, val)
                    })
                    .collect();

                let instr = llvm::InstrKind::Call { tp, name, args };
                let val = self.def.write(bb, instr);

                if let Some(dest) = dest {
                    self.store(val, dest, bb);
                }
            }
            StmtKind::Member {
                dest,
                value,
                ref mem,
            } => {
                let mem_tp = self.reg_type(dest);
                let idx = mem.index.value();
                let tp = self.reg_type(value);
                let value = self.reg_addr(value);

                let instr = llvm::InstrKind::GetElementPtr {
                    tp,
                    ptr: value,
                    offsets: vec![
                        (llvm::Type::Builtin("i64"), llvm::Value::Int(0)),
                        (llvm::Type::Builtin("i32"), llvm::Value::Int(idx as i32)),
                    ],
                };
                let field = self.def.write(bb, instr);

                let instr = llvm::InstrKind::Load {
                    tp: mem_tp,
                    addr: field,
                };
                let val = self.def.write(bb, instr);

                self.store(val, dest, bb);
            }
            StmtKind::Int { dest, value } => {
                let tp = self.reg_type(dest);
                let value = llvm::Value::Int(value);
                let addr = self.reg_addr(dest);

                let instr = llvm::InstrKind::Store { tp, value, addr };
                self.def.write(bb, instr);
            }
        }
    }

    fn gen_term(&mut self, term: &Term, bb: llvm::Block) {
        match term.kind {
            TermKind::Br { cond, succ, fail } => {
                let cond = self.load(cond, bb);
                let succ = self.label(succ);
                let fail = self.label(fail);

                let instr = llvm::TermKind::BrCond { cond, succ, fail };
                self.def.end(bb, instr);
            }
            TermKind::Jump { block } => {
                let block = self.label(block);

                let instr = llvm::TermKind::Br { block };
                self.def.end(bb, instr);
            }
            TermKind::Ret { value: Some(value) } => {
                let tp = self.reg_type(value);
                let value = self.load(value, bb);

                let instr = llvm::TermKind::RetVal { tp, value };
                self.def.end(bb, instr);
            }
            TermKind::Ret { value: None } => {
                let instr = llvm::TermKind::Ret;
                self.def.end(bb, instr);
            }
            TermKind::Unreachable => {
                let instr = llvm::TermKind::Unreachable;
                self.def.end(bb, instr);
            }
        }
    }

    fn load(&mut self, reg: Reg, bb: llvm::Block) -> llvm::Value {
        let addr = self.reg_addr(reg);
        let tp = self.reg_type(reg);

        let instr = llvm::InstrKind::Load { tp, addr };
        self.def.write(bb, instr)
    }

    fn store(&mut self, value: llvm::Value, reg: Reg, bb: llvm::Block) {
        let addr = self.reg_addr(reg);
        let tp = self.reg_type(reg);

        let instr = llvm::InstrKind::Store { tp, value, addr };
        self.def.write(bb, instr);
    }

    fn reg_addr(&self, reg: Reg) -> llvm::Value {
        match reg {
            Reg::Local { index } => self.regs[index.value()].clone(),
        }
    }

    fn reg_type(&self, reg: Reg) -> llvm::Type {
        match reg {
            Reg::Local { index } => type_name(self.ctx, &self.ir.regs[index].tp),
        }
    }

    fn label(&self, block: Block) -> llvm::Block {
        llvm::Block {
            index: block.index.value(),
        }
    }
}

fn gen_decl(ctx: &Context, func: &FuncDef) -> llvm::FuncDecl {
    let name = llvm::Value::Func(func.path.to_string());
    let ret = type_name(ctx, &func.ret);

    let params = func.params
        .iter()
        .enumerate()
        .map(|(idx, param)| {
            let tp = type_name(ctx, &param.tp);
            llvm::ParamDef {
                name: llvm::Value::Param(idx),
                tp,
            }
        })
        .collect();

    llvm::FuncDecl { name, ret, params }
}

fn type_name(ctx: &Context, tp: &Type) -> llvm::Type {
    match tp.kind {
        TypeKind::Named { index } => {
            let def = &ctx.type_defs[index];
            typedef_name(def)
        }
        TypeKind::Ref { ref tp } => llvm::Type::Pointer(Box::new(type_name(ctx, tp))),
        TypeKind::Void => llvm::Type::Void,
    }
}

fn typedef_name(tp: &TypeDef) -> llvm::Type {
    match tp.kind {
        TypeDefKind::Struct { .. } => llvm::Type::Identified(tp.path.to_string()),
        TypeDefKind::Builtin(ref tp) => llvm::Type::Builtin(match *tp {
            BuiltinType::Int => "i32",
            BuiltinType::Bool => "i1",
        }),
        TypeDefKind::Opaque => panic!("getting name of opaque type"),
    }
}
