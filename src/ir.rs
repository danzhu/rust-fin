use std::{fmt, io};

use common::*;
use def::*;
use ctx::*;

#[derive(Clone)]
pub struct Ir {
    pub locals: List<Reg>,
    pub blocks: List<Block>,
}

#[derive(Clone)]
pub struct Block {
    pub stmts: List<Stmt>,
    pub term: Term,
}

#[derive(Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub tp: Type,
}

#[derive(Clone)]
pub enum StmtKind {
    Phi { values: Vec<(Reg, Index)> },
    Binary { op: Op, left: Reg, right: Reg },
    Construct { tp: Type, args: Vec<Reg> },
    Call { func: Func, args: Vec<Reg> },
    Member { value: Reg, mem: Member },
    Param(Index),
    Int(i32),
}

#[derive(Clone)]
pub enum Term {
    Br { cond: Reg, succ: Index, fail: Index },
    Jump(Index),
    Ret(Option<Reg>),
    Unreachable,
}

#[derive(Copy, Clone)]
pub struct Reg {
    pub block: Index,
    pub stmt: Index,
}

impl Ir {
    pub fn new() -> Self {
        Self {
            locals: List::new(),
            blocks: List::new(),
        }
    }

    pub fn write(&mut self, block: Index, stmt: Stmt) -> Reg {
        let stmt = self.blocks[block].stmts.push(stmt);
        Reg { block, stmt }
    }

    pub fn end(&mut self, block: Index, term: Term) {
        self.blocks[block].term = term;
    }

    pub fn push_block(&mut self) -> Index {
        self.blocks.push(Block::new())
    }

    pub fn get(&self, reg: Reg) -> &Stmt {
        &self.blocks[reg.block].stmts[reg.stmt]
    }

    pub fn print<Out>(&self, f: &mut Out, ctx: &Context, def: &FuncDef) -> io::Result<()>
    where
        Out: io::Write,
    {
        for (i, block) in self.blocks.iter().enumerate() {
            writeln!(f, "{}:", i)?;
            block.print(f, ctx, def)?;
        }
        Ok(())
    }
}

impl Block {
    pub fn new() -> Self {
        Self {
            stmts: List::new(),
            term: Term::Unreachable,
        }
    }

    pub fn print<Out>(&self, f: &mut Out, ctx: &Context, def: &FuncDef) -> io::Result<()>
    where
        Out: io::Write,
    {
        for (i, stmt) in self.stmts.iter().enumerate() {
            write!(f, "{}{} ", INDENT, i)?;
            stmt.print(f, ctx, def)?;
            writeln!(f)?;
        }
        write!(f, "{}{} ", INDENT, "-")?;
        self.term.print(f, ctx, def)?;
        writeln!(f)
    }
}

impl Stmt {
    pub fn new(kind: StmtKind, tp: Type) -> Self {
        Self { kind, tp }
    }

    pub fn print<Out>(&self, f: &mut Out, ctx: &Context, def: &FuncDef) -> io::Result<()>
    where
        Out: io::Write,
    {
        match self.kind {
            StmtKind::Phi { ref values } => {
                write!(f, "Phi")?;
                for &(value, reg) in values {
                    write!(f, " ({} {})", value, reg)?;
                }
            }
            StmtKind::Binary {
                op,
                ref left,
                ref right,
            } => {
                write!(f, "Binary {} {} {}", op, left, right)?;
            }
            StmtKind::Construct { ref tp, ref args } => {
                write!(f, "Construct {}", tp.format(ctx))?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
            }
            StmtKind::Call { ref func, ref args } => {
                write!(f, "Call {}", func.format(ctx))?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
            }
            StmtKind::Member { value, ref mem } => {
                let ir = &ctx.irs[def.ir.expect("ir not generated")];
                write!(f, "Member {} {}", value, mem.format(ctx, &ir.get(value).tp))?;
            }
            StmtKind::Param(idx) => {
                write!(f, "Param {}", def.params[idx.value()].format(ctx))?;
            }
            StmtKind::Int(val) => {
                write!(f, "Int {}", val)?;
            }
        }

        write!(f, " -> {}", self.tp.format(ctx))
    }
}

impl Term {
    pub fn print<Out>(&self, f: &mut Out, _ctx: &Context, _def: &FuncDef) -> io::Result<()>
    where
        Out: io::Write,
    {
        match *self {
            Term::Br { cond, succ, fail } => write!(f, "Br {} {} {}", cond, succ, fail),
            Term::Jump(block) => write!(f, "Jump {}", block),
            Term::Ret(Some(reg)) => write!(f, "Ret {}", reg),
            Term::Ret(None) => write!(f, "Ret"),
            Term::Unreachable => write!(f, "Unreachable"),
        }
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.block, self.stmt)
    }
}
