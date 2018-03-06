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
    label: Value,
    instrs: Vec<Value>,
}

type Value = String;

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
            let label = format!("b_{}", i);
            let instrs = block
                .stmts
                .iter()
                .map(|stmt| self.alloc_stmt(stmt))
                .collect();
            self.blocks.push(Blk { label, instrs });
        }

        // generate code
        let ret = self.tp(&self.func.ret);

        write!(self.output, "define {} @{}(", ret, self.func.name)?;
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
            StmtKind::Phi { .. } | StmtKind::Binary { .. } | StmtKind::Call { .. } => {
                format!("%{}", self.temp())
            }
            StmtKind::Param(param) => {
                let param = &self.func.params[param.value()];
                self.param(param)
            }
            StmtKind::Int(val) => val.to_string(),
        }
    }

    fn gen_block(&mut self, block: &Block, idx: usize) -> Result<()> {
        let instrs = self.blocks[idx].instrs.clone();

        writeln!(self.output, "{}:", self.blocks[idx].label)?;

        for (stmt, val) in block.stmts.iter().zip(instrs.into_iter()) {
            self.gen_stmt(stmt, &val)?;
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
                    let lab = &self.blocks[lab.value()].label;

                    write!(self.output, "[ {}, %{} ]", val, lab)?;
                }

                writeln!(self.output)?;
            }
            StmtKind::Binary { op, left, right } => {
                let tp = self.tp(&stmt.tp);

                let op = match op {
                    Op::Add => "add",
                    Op::Sub => "sub",
                    Op::Mul => "mul",
                    Op::Div => "sdiv",
                    Op::Mod => "srem",
                    Op::Eq => "icmp eq",
                    Op::Ne => "icmp ne",
                    Op::Lt => "icmp slt",
                    Op::Gt => "icmp sgt",
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

                // TODO: use arg type instead of param type
                let mut first = true;
                for (param, &arg) in func.params.iter().zip(args) {
                    if first {
                        first = false;
                    } else {
                        write!(self.output, ", ")?;
                    }

                    let tp = self.tp(&param.tp);
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
                let cond = self.reg(cond);
                let succ = self.label(succ);
                let fail = self.label(fail);

                // TODO: use proper type
                writeln!(
                    self.output,
                    "{}br i1 {}, label %{}, label %{}",
                    INDENT, cond, succ, fail
                )?;
            }
            Term::Goto(tar) => {
                let tar = self.label(tar);

                writeln!(self.output, "{}br label %{}", INDENT, tar)?;
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
        let res = format!("t_{}", self.temp);
        self.temp += 1;
        res
    }

    fn reg(&self, reg: Reg) -> Value {
        self.blocks[reg.block.value()].instrs[reg.instr.value()].clone()
    }

    fn label(&self, idx: Index) -> Value {
        self.blocks[idx.value()].label.clone()
    }

    fn param(&self, param: &BindDef) -> Value {
        format!("%p_{}", param.name)
    }

    fn func(&self, func: &FuncDef) -> Value {
        format!("@{}", func.name)
    }

    fn tp(&mut self, tp: &Type) -> Value {
        if *tp == self.store.type_int {
            "i32".to_string()
        } else {
            match tp.kind {
                TypeKind::Named { path: ref _path } => unimplemented!(),
                TypeKind::Void => "void".to_string(),
                TypeKind::Unknown => panic!("unresolved type"),
            }
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
