use std::fmt;

use common::*;

#[derive(Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub tp: Type,
}

#[derive(Clone)]
pub enum ExprKind {
    Block {
        stmts: Vec<Expr>,
    },
    Let {
        value: Box<Expr>,
        var: Bind,
    },
    Construct {
        tp: Type,
        args: Vec<Expr>,
    },
    Function {
        func: Func,
        args: Vec<Expr>,
    },
    Binary {
        op: Op,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        succ: Box<Expr>,
        fail: Box<Expr>,
    },
    Int(i32),
    Id(Bind),
    Noop,
}

impl Expr {
    pub fn new(kind: ExprKind) -> Self {
        Self {
            kind,
            tp: Type::new(TypeKind::Unknown),
        }
    }

    pub fn for_each<Act, E>(&self, mut act: Act) -> Result<(), E>
    where
        Act: FnMut(&Expr) -> Result<(), E>,
    {
        match self.kind {
            ExprKind::Block { ref stmts } => for stmt in stmts {
                act(stmt)?;
            },
            ExprKind::Let { ref value, .. } => {
                act(value)?;
            }
            ExprKind::Construct { ref args, .. } => for arg in args {
                act(arg)?;
            },
            ExprKind::Function { ref args, .. } => for arg in args {
                act(arg)?;
            },
            ExprKind::Binary {
                ref left,
                ref right,
                ..
            } => {
                act(left)?;
                act(right)?;
            }
            ExprKind::If {
                ref cond,
                ref succ,
                ref fail,
            } => {
                act(cond)?;
                act(succ)?;
                act(fail)?;
            }
            ExprKind::Int(_) | ExprKind::Id(_) | ExprKind::Noop => {}
        }

        Ok(())
    }

    fn info(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ExprKind::Block { .. } => {
                write!(f, "Block")?;
            }
            ExprKind::Let { ref var, .. } => {
                write!(f, "Let {:?}", var)?;
            }
            ExprKind::Construct { ref tp, .. } => {
                write!(f, "Construct {:?}", tp)?;
            }
            ExprKind::Function { ref func, .. } => {
                write!(f, "Function {:?}", func)?;
            }
            ExprKind::Binary { ref op, .. } => {
                write!(f, "Binary {:?}", op)?;
            }
            ExprKind::If { .. } => {
                write!(f, "If")?;
            }
            ExprKind::Int(i) => {
                write!(f, "Int {}", i)?;
            }
            ExprKind::Id(ref id) => {
                write!(f, "Id {:?}", id)?;
            }
            ExprKind::Noop => {
                write!(f, "Noop")?;
            }
        }

        write!(f, " -> {:?}", self.tp)
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn print(expr: &Expr, f: &mut fmt::Formatter, ind: i32) -> fmt::Result {
            for _ in 0..ind {
                write!(f, "{}", INDENT)?;
            }

            expr.info(f)?;
            writeln!(f)?;

            expr.for_each(|expr| print(expr, f, ind + 1))
        }

        print(self, f, 0)
    }
}
