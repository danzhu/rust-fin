use std::{fmt, io};

use common::*;
use ast::*;
use ctx::*;

#[derive(Clone, Debug)]
pub struct Ir {
    pub regs: List<RegDef>,
    pub blocks: List<BlockDef>,
}

impl Ir {
    pub fn write(&mut self, block: Block, stmt: Stmt) {
        self.blocks[block.index].stmts.push(stmt);
    }

    pub fn end(&mut self, block: Block, term: Term) {
        self.blocks[block.index].term = term;
    }

    pub fn push_block(&mut self) -> Block {
        let index = self.blocks.push(BlockDef::new());
        Block { index }
    }

    pub fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        writeln!(f, "Registers:")?;
        for (i, reg) in self.regs.iter().enumerate() {
            let idx = Reg::Local(Index::new(i));
            writeln!(f, "{}{} {}", INDENT, idx, reg.tp.format(ctx))?;
        }

        for (i, block) in self.blocks.iter().enumerate() {
            let idx = Block {
                index: Index::new(i),
            };
            writeln!(f, "{}:", idx)?;
            block.print(f, ctx)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct RegDef {
    pub tp: Type,
}

#[derive(Clone, Debug)]
pub struct BlockDef {
    pub stmts: List<Stmt>,
    pub term: Term,
}

impl BlockDef {
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

#[derive(Clone, Debug)]
pub struct Stmt {
    pub kind: StmtKind,
}

impl Stmt {
    pub fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        self.kind.print(f, ctx)
    }
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    Move {
        dest: Reg,
        value: Reg,
    },
    Unary {
        dest: Reg,
        op: UnaryOp,
        value: Reg,
    },
    Binary {
        dest: Reg,
        op: BinaryOp,
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

impl StmtKind {
    pub fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        match *self {
            StmtKind::Move { dest, value } => {
                write!(f, "{} = Move {}", dest, value)?;
            }
            StmtKind::Unary { dest, op, value } => {
                write!(f, "{} = Unary {} {}", dest, op, value)?;
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

#[derive(Clone, Debug)]
pub struct Term {
    pub kind: TermKind,
}

impl Term {
    pub fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        self.kind.print(f, ctx)
    }
}

#[derive(Clone, Debug)]
pub enum TermKind {
    Br { cond: Reg, succ: Block, fail: Block },
    Jump { block: Block },
    Ret { value: Option<Reg> },
    Unreachable,
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

#[derive(Copy, Clone, Debug)]
pub enum Reg {
    Local(Index),
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Reg::Local(idx) => write!(f, "r{}", idx),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Block {
    pub index: Index,
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "b{}", self.index)
    }
}
