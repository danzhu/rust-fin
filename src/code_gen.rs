use std::fmt;
use std::io;
use std::result;

use common::*;
use ir::*;
use def::*;

struct Gen<'a, Out>
where
    Out: 'a + io::Write,
{
    output: &'a mut Out,
    store: &'a Store,
    func: &'a FuncDef,
    blocks: Vec<Blk>,
    temp: usize,
}

struct Blk {
    label: Label,
    values: Vec<Value>,
}

#[derive(Clone)]
struct Label {
    index: usize,
}

#[derive(Clone)]
enum Value {
    Type(String),
    Func(String),
    Param(String),
    Label(Label),
    Local(usize, usize),
    Temp(usize),
    Int(i32),
}

pub enum Error {
    Io(io::Error),
}

pub type Result<T> = result::Result<T, Error>;

pub fn generate<Out>(store: &Store, mut output: Out) -> Result<()>
where
    Out: io::Write,
{
    let mut first = true;
    for tp in &store.type_defs {
        if first {
            first = false;
        } else {
            writeln!(&mut output)?;
        }

        gen_type(store, tp, &mut output)?;
    }
    for func in &store.func_defs {
        if first {
            first = false;
        } else {
            writeln!(&mut output)?;
        }

        let mut gen = Gen {
            output: &mut output,
            store,
            func,
            blocks: Vec::new(),
            temp: 0,
        };
        gen.gen()?;
    }
    Ok(())
}

fn gen_type<Out>(store: &Store, tp: &TypeDef, output: &mut Out) -> Result<()>
where
    Out: io::Write,
{
    writeln!(output, "; Type {}", tp.name)?;
    match tp.kind {
        TypeDefKind::Struct { ref fields } => {
            let name = typedef_name(tp);

            write!(output, "{} = type {{ ", name)?;

            let mut first = true;
            for field in fields {
                if first {
                    first = false;
                } else {
                    write!(output, ", ")?;
                }

                write!(output, "{}", type_name(store, &field.tp))?;
            }

            writeln!(output, " }}")?;
        }
        TypeDefKind::Int | TypeDefKind::Bool => {}
    }
    Ok(())
}

fn type_name(store: &Store, tp: &Type) -> String {
    if *tp == store.type_int {
        "i32".to_string()
    } else if *tp == store.type_bool {
        "i1".to_string()
    } else {
        match tp.kind {
            TypeKind::Named { ref path } => {
                let def = &store.type_defs[path.index];
                typedef_name(def).to_string()
            }
            TypeKind::Void => "void".to_string(),
            TypeKind::Unknown => panic!("unresolved type"),
        }
    }
}

fn typedef_name(tp: &TypeDef) -> Value {
    Value::Type(tp.name.clone())
}

fn funcdef_name(func: &FuncDef) -> Value {
    Value::Func(func.name.clone())
}

impl<'a, Out> Gen<'a, Out>
where
    Out: io::Write,
{
    fn gen(&mut self) -> Result<()> {
        // allocate temporaries
        for (i, block) in self.func.ir.blocks.iter().enumerate() {
            let label = Label { index: i };
            let values = block
                .stmts
                .iter()
                .enumerate()
                .map(|(j, stmt)| match stmt.kind {
                    StmtKind::Phi { .. } | StmtKind::Binary { .. } | StmtKind::Call { .. } => {
                        Value::Local(i, j)
                    }
                    StmtKind::Param(param) => {
                        let param = &self.func.params[param.value()];
                        self.param(param)
                    }
                    StmtKind::Int(val) => Value::Int(val),
                })
                .collect();
            self.blocks.push(Blk { label, values });
        }

        // generate code
        let name = funcdef_name(self.func);
        let ret = type_name(self.store, &self.func.ret);

        writeln!(self.output, "; Func {}", self.func.name)?;
        write!(self.output, "define {} {}(", ret, name)?;
        let mut first = true;
        for param in &self.func.params {
            if first {
                first = false;
            } else {
                write!(self.output, ", ")?;
            }

            let tp = type_name(self.store, &param.tp);
            let param = self.param(param);
            write!(self.output, "{} {}", tp, param)?;
        }
        writeln!(self.output, ") {{")?;

        let mut first = true;
        for (i, block) in self.func.ir.blocks.iter().enumerate() {
            if first {
                first = false;
            } else {
                writeln!(self.output)?;
            }

            self.gen_block(block, i)?;
        }

        writeln!(self.output, "}}")?;
        Ok(())
    }

    fn gen_block(&mut self, block: &Block, idx: usize) -> Result<()> {
        // TODO: avoid this copy
        let values = self.blocks[idx].values.clone();

        writeln!(self.output, "{}:", self.blocks[idx].label)?;

        for (stmt, val) in block.stmts.iter().zip(&values) {
            self.gen_stmt(stmt, val)?;
        }
        self.gen_term(&block.term)?;
        Ok(())
    }

    fn gen_stmt(&mut self, stmt: &Stmt, val: &Value) -> Result<()> {
        match stmt.kind {
            StmtKind::Phi { ref values } => {
                let tp = type_name(self.store, &stmt.tp);

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
                let tp = type_name(self.store, &self.func.ir.get(left).tp);

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

                let left = self.reg(left);
                let right = self.reg(right);

                writeln!(
                    self.output,
                    "{}{} = {} {} {}, {}",
                    INDENT, val, op, tp, left, right
                )?;
            }
            StmtKind::Call { ref func, ref args } => {
                let tp = type_name(self.store, &stmt.tp);

                let func = &self.store.func_defs[func.path.index];
                let name = funcdef_name(func);

                write!(self.output, "{}{} = call {} {}(", INDENT, val, tp, name)?;

                let mut first = true;
                for &arg in args {
                    if first {
                        first = false;
                    } else {
                        write!(self.output, ", ")?;
                    }

                    let tp = type_name(self.store, &self.func.ir.get(arg).tp);
                    let arg = self.reg(arg);

                    write!(self.output, "{} {}", tp, arg)?;
                }

                writeln!(self.output, ")")?;
            }
            StmtKind::Param(_) | StmtKind::Int(_) => {}
        }

        Ok(())
    }

    fn gen_term(&mut self, term: &Term) -> Result<()> {
        match *term {
            Term::Br { cond, succ, fail } => {
                let tp = type_name(self.store, &self.func.ir.get(cond).tp);
                let cond = self.reg(cond);
                let succ = self.label(succ);
                let fail = self.label(fail);

                writeln!(
                    self.output,
                    "{}br {} {}, label {}, label {}",
                    INDENT, tp, cond, succ, fail
                )?;
            }
            Term::Goto(tar) => {
                let tar = self.label(tar);

                writeln!(self.output, "{}br label {}", INDENT, tar)?;
            }
            Term::Ret(reg) => {
                let tp = type_name(self.store, &self.func.ir.get(reg).tp);
                let reg = self.reg(reg);

                writeln!(self.output, "{}ret {} {}", INDENT, tp, reg)?;
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

    fn param(&self, param: &BindDef) -> Value {
        Value::Param(param.name.clone())
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
            Value::Type(ref name) => write!(f, "%{}", name),
            Value::Func(ref name) => write!(f, "@{}", name),
            Value::Param(ref name) => write!(f, "%p_{}", name),
            Value::Label(ref lab) => write!(f, "%{}", lab),
            Value::Local(blk, idx) => write!(f, "%l_{}_{}", blk, idx),
            Value::Temp(idx) => write!(f, "%t_{}", idx),
            Value::Int(val) => write!(f, "{}", val),
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
