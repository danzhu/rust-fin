use std::fmt;

use common::*;

#[derive(Clone)]
pub struct Ir {
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
    Call { func: Func, args: Vec<Reg> },
    Param(Index),
    Int(i32),
}

#[derive(Clone)]
pub enum Term {
    Br { cond: Reg, succ: Index, fail: Index },
    Goto(Index),
    Ret(Reg),
    Unreachable,
}

#[derive(Copy, Clone)]
pub struct Reg {
    pub block: Index,
    pub instr: Index,
}

impl Ir {
    pub fn new() -> Self {
        Self {
            blocks: List::new(),
        }
    }

    pub fn write(&mut self, block: Index, stmt: Stmt) -> Reg {
        let instr = self.blocks[block].stmts.push(stmt);
        Reg { block, instr }
    }

    pub fn end(&mut self, block: Index, term: Term) {
        self.blocks[block].term = term;
    }

    pub fn push_block(&mut self) -> Index {
        self.blocks.push(Block::new())
    }
}

impl fmt::Debug for Ir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, block) in self.blocks.iter().enumerate() {
            writeln!(f, "{}:", i)?;
            write!(f, "{:?}", block)?;
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
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, stmt) in self.stmts.iter().enumerate() {
            writeln!(f, "{}{} {:?}", INDENT, i, stmt)?;
        }
        writeln!(f, "{}{:?}", INDENT, self.term)
    }
}

impl Stmt {
    pub fn new(kind: StmtKind, tp: Type) -> Self {
        Self { kind, tp }
    }
}

impl fmt::Debug for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            StmtKind::Phi { ref values } => {
                write!(f, "Phi")?;
                for &(value, reg) in values {
                    write!(f, " ({:?} {:?})", value, reg)?;
                }
            }
            StmtKind::Binary {
                op,
                ref left,
                ref right,
            } => {
                write!(f, "Binary {:?} {:?} {:?}", op, left, right)?;
            }
            StmtKind::Call { ref func, ref args } => {
                write!(f, "Call {:?}", func)?;
                for arg in args {
                    write!(f, " {:?}", arg)?;
                }
            }
            StmtKind::Param(idx) => {
                write!(f, "Param {:?}", idx)?;
            }
            StmtKind::Int(val) => {
                write!(f, "Int {}", val)?;
            }
        }

        write!(f, " -> {:?}", self.tp)
    }
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Term::Br { cond, succ, fail } => write!(f, "Br {:?} {:?} {:?}", cond, succ, fail),
            Term::Goto(block) => write!(f, "Goto {:?}", block),
            Term::Ret(reg) => write!(f, "Ret {:?}", reg),
            Term::Unreachable => write!(f, "Unreachable"),
        }
    }
}

impl Reg {
    pub const NONE: Reg = Reg {
        block: Index::UNKNOWN,
        instr: Index::UNKNOWN,
    };
}

impl fmt::Debug for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}:{:?}", self.block, self.instr)
    }
}