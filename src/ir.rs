use std::{fmt, io};

use common::*;
use ast::*;
use ctx::*;

#[derive(Clone, Debug)]
pub struct Ir {
    pub locals: List<Local>,
    pub blocks: List<Block>,
}

#[derive(Clone, Debug)]
pub struct Local {
    pub tp: Type,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: List<Stmt>,
    pub term: Term,
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub kind: StmtKind,
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    Move {
        dest: Reg,
        value: Reg,
    },
    Binary {
        dest: Reg,
        op: Op,
        left: Reg,
        right: Reg,
    },
    Construct {
        dest: Reg,
        tp: Type,
        args: Vec<Reg>,
    },
    Call {
        dest: Option<Reg>,
        func: Func,
        args: Vec<Reg>,
    },
    Member {
        dest: Reg,
        value: Reg,
        mem: Member,
    },
    Int {
        dest: Reg,
        value: i32,
    },
}

#[derive(Clone, Debug)]
pub struct Term {
    pub kind: TermKind,
}

#[derive(Clone, Debug)]
pub enum TermKind {
    Br { cond: Reg, succ: Index, fail: Index },
    Jump { block: Index },
    Ret { value: Option<Reg> },
    Unreachable,
}

#[derive(Copy, Clone, Debug)]
pub enum Reg {
    Local(Index),
}

impl Ir {
    pub fn write(&mut self, block: Index, stmt: Stmt) {
        self.blocks[block].stmts.push(stmt);
    }

    pub fn end(&mut self, block: Index, term: Term) {
        self.blocks[block].term = term;
    }

    pub fn push_block(&mut self) -> Index {
        self.blocks.push(Block::new())
    }

    pub fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        writeln!(f, "Locals:")?;
        for (i, local) in self.locals.iter().enumerate() {
            writeln!(f, "{}{} {}", INDENT, i, local.tp.format(ctx))?;
        }

        for (i, block) in self.blocks.iter().enumerate() {
            writeln!(f, "{}:", i)?;
            block.print(f, ctx)?;
        }
        Ok(())
    }
}

impl Block {
    pub fn new() -> Self {
        Self {
            stmts: List::new(),
            term: Term {
                kind: TermKind::Unreachable,
            },
        }
    }

    pub fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        for stmt in &self.stmts {
            write!(f, "{}", INDENT)?;
            stmt.print(f, ctx)?;
            writeln!(f)?;
        }
        write!(f, "{}", INDENT)?;
        self.term.print(f, ctx)?;
        writeln!(f)
    }
}

impl Stmt {
    pub fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        self.kind.print(f, ctx)
    }
}

impl StmtKind {
    pub fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        match *self {
            StmtKind::Move { dest, value } => {
                write!(f, "{} = Move {}", dest, value)?;
            }
            StmtKind::Binary {
                dest,
                op,
                left,
                right,
            } => {
                write!(f, "{} = Binary {} {} {}", dest, op, left, right)?;
            }
            StmtKind::Construct {
                dest,
                ref tp,
                ref args,
            } => {
                write!(f, "{} = Construct {}", dest, tp.format(ctx))?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
            }
            StmtKind::Call {
                dest,
                ref func,
                ref args,
            } => {
                if let Some(dest) = dest {
                    write!(f, "{} = ", dest)?;
                }
                write!(f, "Call {}", func.format(ctx))?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
            }
            StmtKind::Member {
                dest,
                value,
                ref mem,
            } => {
                write!(f, "{} = Member {} {}", dest, value, mem.format(ctx))?;
            }
            StmtKind::Int { dest, value } => {
                write!(f, "{} = Int {}", dest, value)?;
            }
        }
        Ok(())
    }
}

impl Term {
    pub fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        self.kind.print(f, ctx)
    }
}

impl TermKind {
    pub fn print<Out>(&self, f: &mut Out, _ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        match *self {
            TermKind::Br { cond, succ, fail } => write!(f, "Br {} {} {}", cond, succ, fail),
            TermKind::Jump { block } => write!(f, "Jump {}", block),
            TermKind::Ret { value: Some(reg) } => write!(f, "Ret {}", reg),
            TermKind::Ret { value: None } => write!(f, "Ret"),
            TermKind::Unreachable => write!(f, "Unreachable"),
        }
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Reg::Local(idx) => write!(f, "r{}", idx),
        }
    }
}
