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
    Func(String),
    Param(String),
    Label(Label),
    Local(usize),
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
                .map(|stmt| self.alloc_stmt(stmt))
                .collect();
            self.blocks.push(Blk { label, values });
        }

        // generate code
        let name = self.func(self.func);
        let ret = self.tp(&self.func.ret);

        write!(self.output, "define {} {}(", ret, name)?;
        let mut first = true;
        for param in &self.func.params {
            if first {
                first = false;
            } else {
                write!(self.output, ", ")?;
            }

            let tp = self.tp(&param.tp);
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

    fn alloc_stmt(&mut self, stmt: &Stmt) -> Value {
        match stmt.kind {
            StmtKind::Phi { .. } | StmtKind::Binary { .. } | StmtKind::Call { .. } => self.temp(),
            StmtKind::Param(param) => {
                let param = &self.func.params[param.value()];
                self.param(param)
            }
            StmtKind::Int(val) => Value::Int(val),
        }
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
                let tp = self.tp(&stmt.tp);

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
                let tp = self.tp(&self.func.ir.get(left).tp);

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
                let tp = self.tp(&stmt.tp);

                let func = &self.store.func_defs[func.path.index];
                let name = self.func(func);

                write!(self.output, "{}{} = call {} {}(", INDENT, val, tp, name)?;

                let mut first = true;
                for &arg in args {
                    if first {
                        first = false;
                    } else {
                        write!(self.output, ", ")?;
                    }

                    let tp = self.tp(&self.func.ir.get(arg).tp);
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
                let tp = self.tp(&self.func.ir.get(cond).tp);
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
                let tp = self.tp(&self.func.ir.get(reg).tp);
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
        let res = Value::Local(self.temp);
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

    fn func(&self, func: &FuncDef) -> Value {
        Value::Func(func.name.clone())
    }

    fn tp(&mut self, tp: &Type) -> String {
        if *tp == self.store.type_int {
            "i32".to_string()
        } else if *tp == self.store.type_bool {
            "i1".to_string()
        } else {
            match tp.kind {
                TypeKind::Named { path: ref _path } => unimplemented!(),
                TypeKind::Void => "void".to_string(),
                TypeKind::Unknown => panic!("unresolved type"),
            }
        }
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
            Value::Func(ref name) => write!(f, "@{}", name),
            Value::Param(ref name) => write!(f, "%p_{}", name),
            Value::Label(ref lab) => write!(f, "%{}", lab),
            Value::Local(idx) => write!(f, "%l_{}", idx),
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
