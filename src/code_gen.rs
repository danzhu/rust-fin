use std::{fmt, io, result};

use common::*;
use ast::*;
use ir::*;
use ctx::*;

pub fn generate<Out>(ctx: &Context, mut output: Out) -> Result<()>
where
    Out: io::Write,
{
    for tp in &ctx.type_defs {
        gen_type(ctx, tp, &mut output)?;
    }

    for func in &ctx.func_defs {
        if let Some(ir) = func.ir {
            let ir = &ctx.irs[ir];

            let mut gen = FuncGen {
                output: &mut output,
                ctx,
                func,
                ir,
                temp: 0,
            };
            gen.gen()?;
        } else {
            gen_extern(ctx, func, &mut output)?;
        }
    }
    Ok(())
}

fn gen_type<Out>(ctx: &Context, tp: &TypeDef, output: &mut Out) -> Result<()>
where
    Out: io::Write,
{
    match tp.kind {
        TypeDefKind::Struct { ref fields, .. } => {
            let name = typedef_name(tp);

            write!(output, "{} = type {{ ", name)?;

            let mut first = true;
            for field in fields {
                if first {
                    first = false;
                } else {
                    write!(output, ", ")?;
                }

                write!(output, "{}", type_name(ctx, &field.tp))?;
            }

            writeln!(output, " }}")?;
            writeln!(output)?;
        }
        TypeDefKind::Builtin(_) => {}
        TypeDefKind::Opaque => panic!("generating opaque type"),
    }
    Ok(())
}

fn gen_extern<Out>(ctx: &Context, func: &FuncDef, output: &mut Out) -> Result<()>
where
    Out: io::Write,
{
    write!(output, "declare ")?;

    gen_sig(ctx, func, output)?;

    writeln!(output)?;
    writeln!(output)?;

    Ok(())
}

struct FuncGen<'a, Out>
where
    Out: 'a + io::Write,
{
    output: &'a mut Out,
    ctx: &'a Context,
    func: &'a FuncDef,
    ir: &'a Ir,
    temp: usize,
}

impl<'a, Out> FuncGen<'a, Out>
where
    Out: io::Write,
{
    fn gen(&mut self) -> Result<()> {
        write!(self.output, "define ")?;

        gen_sig(self.ctx, self.func, self.output)?;

        writeln!(self.output, " {{")?;

        let mut first = true;
        for (i, block) in self.ir.blocks.iter().enumerate() {
            if first {
                writeln!(self.output, "; locals")?;
                for (idx, reg) in self.ir.regs.iter().enumerate() {
                    let tp = type_name(self.ctx, &reg.tp);
                    let reg = Value::Reg(Reg::Local(Index::new(idx)));

                    self.exec(format_args!("{} = alloca {}", reg, tp))?;
                }

                writeln!(self.output, "; params")?;
                for (idx, param) in self.func.params.iter().enumerate() {
                    let tp = type_name(self.ctx, &param.tp);
                    let reg = Value::Reg(Reg::Local(Index::new(idx)));

                    self.exec(format_args!(
                        "store {} {}, {}* {}",
                        tp,
                        Value::Param(idx),
                        tp,
                        reg,
                    ))?;
                }

                first = false;
            } else {
                writeln!(self.output)?;
                let index = Index::new(i);
                writeln!(self.output, "{}:", Block { index })?;
            }

            for stmt in &block.stmts {
                self.gen_stmt(stmt)?;
            }
            self.gen_term(&block.term)?;
        }

        writeln!(self.output, "}}")?;
        writeln!(self.output)?;
        Ok(())
    }

    fn gen_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        write!(self.output, "; ")?;
        stmt.print(self.output, self.ctx)?;
        writeln!(self.output)?;

        match stmt.kind {
            StmtKind::Move { dest, value } => {
                let val = self.load(value)?;
                self.store(val, dest)?;
            }
            StmtKind::Unary { dest, op, value } => {
                let tp = self.reg_type(value);
                let value = self.load(value)?;

                let val = match op {
                    UnaryOp::Neg => self.write(format_args!("sub {} 0, {}", tp, value))?,
                    UnaryOp::Not => self.write(format_args!("xor {} {}, true", tp, value))?,
                };
                self.store(val, dest)?;
            }
            StmtKind::Binary {
                dest,
                op,
                left,
                right,
            } => {
                let tp = self.reg_type(left);
                let left = self.load(left)?;
                let right = self.load(right)?;

                let op = match op {
                    BinaryOp::Arith(op) => match op {
                        ArithOp::Add => "add",
                        ArithOp::Sub => "sub",
                        ArithOp::Mul => "mul",
                        ArithOp::Div => "sdiv",
                        ArithOp::Mod => "srem",
                    }.to_string(),
                    BinaryOp::Comp(op) => {
                        "icmp ".to_string() + match op {
                            CompOp::Eq => "eq",
                            CompOp::Ne => "ne",
                            CompOp::Lt => "slt",
                            CompOp::Gt => "sgt",
                        }
                    }
                };

                let val = self.write(format_args!("{} {} {}, {}", op, tp, left, right))?;
                self.store(val, dest)?;
            }
            StmtKind::Construct {
                dest,
                ref tp,
                ref args,
            } => {
                let dest = self.addr(dest);
                let tp = type_name(self.ctx, tp);

                for (idx, &arg) in args.iter().enumerate() {
                    let arg_tp = self.reg_type(arg);
                    let arg = self.load(arg)?;
                    let field = self.write(format_args!(
                        "getelementptr {}, {}* {}, i64 0, i32 {}",
                        tp, tp, dest, idx
                    ))?;
                    self.exec(format_args!(
                        "store {} {}, {}* {}",
                        arg_tp, arg, arg_tp, field
                    ))?;
                }
            }
            StmtKind::Call {
                dest,
                ref func,
                ref args,
            } => {
                let func = &self.ctx.get_func(func);
                let name = Value::Func(func.path.clone());
                let tp = type_name(self.ctx, &func.ret);

                let args = args.iter()
                    .map(|&arg| {
                        let tp = self.reg_type(arg);
                        let val = self.load(arg)?;
                        Ok(format!("{} {}", tp, val))
                    })
                    .collect::<Result<Vec<_>>>()?;

                if let Some(dest) = dest {
                    let val =
                        self.write(format_args!("call {} {}({})", tp, name, args.join(", ")))?;

                    self.store(val, dest)?;
                } else {
                    self.exec(format_args!("call {} {}({})", tp, name, args.join(", ")))?;
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
                let value = self.addr(value);
                let field = self.write(format_args!(
                    "getelementptr {}, {}* {}, i64 0, i32 {}",
                    tp, tp, value, idx
                ))?;

                let val = self.write(format_args!("load {}, {}* {}", mem_tp, mem_tp, field))?;
                self.store(val, dest)?;
            }
            StmtKind::Int { dest, value } => {
                let tp = self.reg_type(dest);
                let dest = self.addr(dest);
                self.exec(format_args!("store {} {}, {}* {}", tp, value, tp, dest))?;
            }
        }
        Ok(())
    }

    fn gen_term(&mut self, term: &Term) -> Result<()> {
        write!(self.output, "; ")?;
        term.print(self.output, self.ctx)?;
        writeln!(self.output)?;

        match term.kind {
            TermKind::Br { cond, succ, fail } => {
                let tp = self.reg_type(cond);
                let cond = self.load(cond)?;
                let succ = self.label(succ);
                let fail = self.label(fail);

                self.exec(format_args!(
                    "br {} {}, label {}, label {}",
                    tp, cond, succ, fail
                ))?;
            }
            TermKind::Jump { block } => {
                let block = self.label(block);

                self.exec(format_args!("br label {}", block))?;
            }
            TermKind::Ret { value: Some(value) } => {
                let tp = self.reg_type(value);
                let value = self.load(value)?;

                self.exec(format_args!("ret {} {}", tp, value))?;
            }
            TermKind::Ret { value: None } => {
                self.exec(format_args!("ret void"))?;
            }
            TermKind::Unreachable => {
                self.exec(format_args!("unreachable"))?;
            }
        }
        Ok(())
    }

    fn addr(&mut self, reg: Reg) -> Value {
        Value::Reg(reg)
    }

    fn load(&mut self, reg: Reg) -> Result<Value> {
        let tp = match reg {
            Reg::Local(idx) => type_name(self.ctx, &self.ir.regs[idx].tp),
        };

        self.write(format_args!("load {}, {}* {}", tp, tp, Value::Reg(reg),))
    }

    fn store(&mut self, val: Value, reg: Reg) -> Result<()> {
        let tp = match reg {
            Reg::Local(idx) => type_name(self.ctx, &self.ir.regs[idx].tp),
        };

        self.exec(format_args!(
            "store {} {}, {}* {}",
            tp,
            val,
            tp,
            Value::Reg(reg),
        ))
    }

    fn write(&mut self, args: fmt::Arguments) -> Result<Value> {
        let val = self.temp();
        writeln!(self.output, "{}{} = {}", INDENT, val, args)?;
        Ok(val)
    }

    fn exec(&mut self, args: fmt::Arguments) -> Result<()> {
        writeln!(self.output, "{}{}", INDENT, args)?;
        Ok(())
    }

    fn temp(&mut self) -> Value {
        let res = Value::Temp(self.temp);
        self.temp += 1;
        res
    }

    fn label(&self, block: Block) -> Value {
        Value::Block(block)
    }

    fn reg_type(&self, reg: Reg) -> Value {
        match reg {
            Reg::Local(idx) => type_name(self.ctx, &self.ir.regs[idx].tp),
        }
    }
}

fn gen_sig<Out>(ctx: &Context, func: &FuncDef, output: &mut Out) -> Result<()>
where
    Out: io::Write,
{
    let name = Value::Func(func.path.clone());
    let ret = type_name(ctx, &func.ret);

    write!(output, "{} {}(", ret, name)?;

    let mut first = true;
    for (idx, param) in func.params.iter().enumerate() {
        if first {
            first = false;
        } else {
            write!(output, ", ")?;
        }

        let tp = type_name(ctx, &param.tp);
        write!(output, "{} {}", tp, Value::Param(idx))?;
    }

    write!(output, ")")?;

    Ok(())
}

fn type_name(ctx: &Context, tp: &Type) -> Value {
    match tp.kind {
        TypeKind::Named { index } => {
            let def = &ctx.type_defs[index];
            typedef_name(def)
        }
        TypeKind::Void => Value::Builtin("void"),
    }
}

fn typedef_name(tp: &TypeDef) -> Value {
    match tp.kind {
        TypeDefKind::Struct { .. } => Value::Type(tp.path.clone()),
        TypeDefKind::Builtin(ref tp) => Value::Builtin(match *tp {
            BuiltinType::Int => "i32",
            BuiltinType::Bool => "i1",
        }),
        TypeDefKind::Opaque => panic!("getting name of opaque type"),
    }
}

#[derive(Clone)]
enum Value {
    Builtin(&'static str),
    Type(Path),
    Func(Path),
    Block(Block),
    Reg(Reg),
    Param(usize),
    Temp(usize),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Builtin(name) => write!(f, "{}", name),
            Value::Type(ref name) => write!(f, "%{}", name),
            Value::Func(ref name) => write!(f, "@{}", name),
            Value::Block(ref lab) => write!(f, "%{}", lab),
            Value::Reg(idx) => write!(f, "%_{}", idx),
            Value::Param(idx) => write!(f, "%_p{}", idx),
            Value::Temp(idx) => write!(f, "%_t{}", idx),
        }
    }
}

pub enum Error {
    Io(io::Error),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Io(err)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Io(ref err) => write!(f, "{}", err),
        }
    }
}

pub type Result<T> = result::Result<T, Error>;
