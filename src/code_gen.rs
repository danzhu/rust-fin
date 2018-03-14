use std::{fmt, io, result};

use common::*;
use ir::*;
use def::*;
use ctx::*;

struct FuncGen<'a, Out>
where
    Out: 'a + io::Write,
{
    output: &'a mut Out,
    ctx: &'a Context,
    func: &'a FuncDef,
    ir: &'a Ir,
    blocks: &'a Vec<BlockValues>,
    temp: usize,
}

struct BlockValues {
    label: Label,
    values: Vec<Value>,
}

#[derive(Clone)]
struct Label {
    index: usize,
}

#[derive(Clone)]
enum Value {
    Builtin(&'static str),
    Type(Name),
    Func(Name),
    Param(String),
    Label(Label),
    Local(usize, usize),
    Temp(usize),
    Int(i32),
    Undefined,
    None,
}

pub enum Error {
    Io(io::Error),
}

pub type Result<T> = result::Result<T, Error>;

pub fn generate<Out>(ctx: &Context, mut output: Out) -> Result<()>
where
    Out: io::Write,
{
    let mut first = true;
    for tp in &ctx.type_defs {
        if first {
            first = false;
        } else {
            writeln!(&mut output)?;
        }

        gen_type(ctx, tp, &mut output)?;
    }

    for func in &ctx.func_defs {
        if first {
            first = false;
        } else {
            writeln!(&mut output)?;
        }

        let ir = &ctx.irs[func.ir.expect("ir not generated")];

        let blocks = alloc_values(ctx, func, ir);

        let mut gen = FuncGen {
            output: &mut output,
            ctx,
            func,
            ir,
            blocks: &blocks,
            temp: 0,
        };
        gen.gen()?;
    }
    Ok(())
}

fn alloc_values(ctx: &Context, func: &FuncDef, ir: &Ir) -> Vec<BlockValues> {
    ir.blocks
        .iter()
        .enumerate()
        .map(|(i, block)| {
            let label = Label { index: i };
            let values = block
                .stmts
                .iter()
                .enumerate()
                .map(|(j, stmt)| match stmt.kind {
                    StmtKind::Phi { .. } | StmtKind::Binary { .. } | StmtKind::Member { .. } => {
                        Value::Local(i, j)
                    }
                    StmtKind::Call { .. } => {
                        if stmt.tp.kind == TypeKind::Void {
                            Value::None
                        } else {
                            Value::Local(i, j)
                        }
                    }
                    StmtKind::Construct { ref tp, .. } => {
                        // TODO: remove the need to check fields for value allocation
                        let def = &ctx.get_type(tp);
                        match def.kind {
                            TypeDefKind::Struct { ref fields, .. } => if fields.is_empty() {
                                Value::Undefined
                            } else {
                                Value::Local(i, j)
                            },
                            TypeDefKind::Builtin(_) => panic!("constructing primtive type"),
                        }
                    }
                    StmtKind::Param(param) => {
                        let param = &func.params[param.value()];
                        param_name(param)
                    }
                    StmtKind::Int(val) => Value::Int(val),
                })
                .collect();
            BlockValues { label, values }
        })
        .collect()
}

fn gen_type<Out>(ctx: &Context, tp: &TypeDef, output: &mut Out) -> Result<()>
where
    Out: io::Write,
{
    writeln!(output, "; Type {}", tp.name)?;
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
        }
        TypeDefKind::Builtin(_) => {}
    }
    Ok(())
}

fn type_name(ctx: &Context, tp: &Type) -> Value {
    match tp.kind {
        TypeKind::Named { ref path } => {
            let def = &ctx.type_defs[path.index()];
            typedef_name(def)
        }
        TypeKind::Void => Value::Builtin("void"),
        TypeKind::Unknown => panic!("unresolved type"),
    }
}

fn typedef_name(tp: &TypeDef) -> Value {
    match tp.kind {
        TypeDefKind::Struct { .. } => Value::Type(tp.name.clone()),
        TypeDefKind::Builtin(ref tp) => Value::Builtin(match *tp {
            BuiltinType::Int => "i32",
            BuiltinType::Bool => "i1",
        }),
    }
}

fn funcdef_name(func: &FuncDef) -> Value {
    Value::Func(func.name.clone())
}

fn param_name(param: &BindDef) -> Value {
    Value::Param(param.name.clone())
}

impl<'a, Out> FuncGen<'a, Out>
where
    Out: io::Write,
{
    fn gen(&mut self) -> Result<()> {
        let name = funcdef_name(self.func);
        let ret = type_name(self.ctx, &self.func.ret);

        writeln!(self.output, "; Func {}", self.func.name)?;
        write!(self.output, "define {} {}(", ret, name)?;

        let mut first = true;
        for param in &self.func.params {
            if first {
                first = false;
            } else {
                write!(self.output, ", ")?;
            }

            let tp = type_name(self.ctx, &param.tp);
            let param = param_name(param);
            write!(self.output, "{} {}", tp, param)?;
        }

        writeln!(self.output, ") {{")?;

        let mut first = true;
        for (block, vals) in self.ir.blocks.iter().zip(self.blocks) {
            if first {
                first = false;
            } else {
                writeln!(self.output)?;
            }

            self.gen_block(block, vals)?;
        }

        writeln!(self.output, "}}")?;
        Ok(())
    }

    fn gen_block(&mut self, block: &Block, vals: &BlockValues) -> Result<()> {
        writeln!(self.output, "{}:", vals.label)?;

        for (stmt, val) in block.stmts.iter().zip(&vals.values) {
            self.gen_stmt(stmt, val)?;
        }
        self.gen_term(&block.term)?;
        Ok(())
    }

    fn gen_stmt(&mut self, stmt: &Stmt, val: &Value) -> Result<()> {
        match stmt.kind {
            StmtKind::Phi { ref values } => {
                let tp = type_name(self.ctx, &stmt.tp);

                write!(self.output, "{}{} = phi {} ", INDENT, val, tp)?;

                let mut first = true;
                for &(val, lab) in values {
                    if first {
                        first = false;
                    } else {
                        write!(self.output, ", ")?;
                    }

                    let val = self.reg(val);
                    let lab = self.label(lab);

                    write!(self.output, "[ {}, {} ]", val, lab)?;
                }

                writeln!(self.output)?;
            }
            StmtKind::Binary { op, left, right } => {
                let op = match op {
                    Op::Arith(op) => match op {
                        ArithOp::Add => "add",
                        ArithOp::Sub => "sub",
                        ArithOp::Mul => "mul",
                        ArithOp::Div => "sdiv",
                        ArithOp::Mod => "srem",
                    }.to_string(),
                    Op::Comp(op) => {
                        "icmp ".to_string() + match op {
                            CompOp::Eq => "eq",
                            CompOp::Ne => "ne",
                            CompOp::Lt => "slt",
                            CompOp::Gt => "sgt",
                        }
                    }
                };

                let tp = self.reg_type(left);
                let left = self.reg(left);
                let right = self.reg(right);

                writeln!(
                    self.output,
                    "{}{} = {} {} {}, {}",
                    INDENT, val, op, tp, left, right
                )?;
            }
            StmtKind::Construct { ref tp, ref args } => {
                let tp = type_name(self.ctx, tp);

                let last = args.len() - 1;
                let mut tmp_val = Value::Undefined;
                for (i, &arg) in args.iter().enumerate() {
                    let arg_tp = self.reg_type(arg);
                    let arg = self.reg(arg);
                    let new_val = if i != last {
                        self.temp()
                    } else {
                        val.clone()
                    };
                    writeln!(
                        self.output,
                        "{}{} = insertvalue {} {}, {} {}, {}",
                        INDENT, new_val, tp, tmp_val, arg_tp, arg, i
                    )?;
                    tmp_val = new_val;
                }
            }
            StmtKind::Call { ref func, ref args } => {
                let tp = type_name(self.ctx, &stmt.tp);

                let func = &self.ctx.get_func(func);
                let name = funcdef_name(func);

                if stmt.tp.kind == TypeKind::Void {
                    write!(self.output, "{}call {} {}(", INDENT, tp, name)?;
                } else {
                    write!(self.output, "{}{} = call {} {}(", INDENT, val, tp, name)?;
                }

                let mut first = true;
                for &arg in args {
                    if first {
                        first = false;
                    } else {
                        write!(self.output, ", ")?;
                    }

                    let tp = self.reg_type(arg);
                    let arg = self.reg(arg);

                    write!(self.output, "{} {}", tp, arg)?;
                }

                writeln!(self.output, ")")?;
            }
            StmtKind::Member { value, ref mem } => {
                let tp = self.reg_type(value);
                let value = self.reg(value);
                let idx = mem.path.index().value();

                writeln!(
                    self.output,
                    "{}{} = extractvalue {} {}, {}",
                    INDENT, val, tp, value, idx
                )?;
            }
            StmtKind::Param(_) | StmtKind::Int(_) => {}
        }
        Ok(())
    }

    fn gen_term(&mut self, term: &Term) -> Result<()> {
        match *term {
            Term::Br { cond, succ, fail } => {
                let tp = self.reg_type(cond);
                let cond = self.reg(cond);
                let succ = self.label(succ);
                let fail = self.label(fail);

                writeln!(
                    self.output,
                    "{}br {} {}, label {}, label {}",
                    INDENT, tp, cond, succ, fail
                )?;
            }
            Term::Jump(tar) => {
                let tar = self.label(tar);

                writeln!(self.output, "{}br label {}", INDENT, tar)?;
            }
            Term::Ret(Some(reg)) => {
                let tp = self.reg_type(reg);
                let reg = self.reg(reg);

                writeln!(self.output, "{}ret {} {}", INDENT, tp, reg)?;
            }
            Term::Ret(None) => {
                writeln!(self.output, "{}ret void", INDENT)?;
            }
            Term::Unreachable => {
                writeln!(self.output, "{}unreachable", INDENT)?;
            }
        }
        Ok(())
    }

    fn temp(&mut self) -> Value {
        let res = Value::Temp(self.temp);
        self.temp += 1;
        res
    }

    fn reg(&self, reg: Reg) -> Value {
        self.blocks[reg.block.value()].values[reg.stmt.value()].clone()
    }

    fn label(&self, idx: Index) -> Value {
        Value::Label(self.blocks[idx.value()].label.clone())
    }

    fn reg_type(&self, reg: Reg) -> Value {
        type_name(self.ctx, &self.ir.get(reg).tp)
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "b_{}", self.index)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Builtin(name) => write!(f, "{}", name),
            Value::Type(ref name) => write!(f, "%{}", name),
            Value::Func(ref name) => write!(f, "@{}", name),
            Value::Param(ref name) => write!(f, "%p_{}", name),
            Value::Label(ref lab) => write!(f, "%{}", lab),
            Value::Local(blk, idx) => write!(f, "%l_{}_{}", blk, idx),
            Value::Temp(idx) => write!(f, "%t_{}", idx),
            Value::Int(val) => write!(f, "{}", val),
            Value::Undefined => write!(f, "undef"),
            Value::None => panic!("attempt to use none value"),
        }
    }
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
