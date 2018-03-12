use std::io;

use common::*;
use def::*;
use ctx::*;

#[derive(Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
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
    Member {
        value: Box<Expr>,
        mem: Member,
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
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self {
            kind,
            span,
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
            ExprKind::Let { ref value, .. } | ExprKind::Member { ref value, .. } => {
                act(value)?;
            }
            ExprKind::Construct { ref args, .. } | ExprKind::Function { ref args, .. } => {
                for arg in args {
                    act(arg)?;
                }
            }
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

    fn info<Out>(&self, f: &mut Out, ctx: &Context, def: &FuncDef) -> io::Result<()>
    where
        Out: io::Write,
    {
        match self.kind {
            ExprKind::Block { .. } => {
                write!(f, "Block")?;
            }
            ExprKind::Let { ref var, .. } => {
                write!(f, "Let {}", var.format(ctx, def))?;
            }
            ExprKind::Construct { ref tp, .. } => {
                write!(f, "Construct {}", tp.format(ctx))?;
            }
            ExprKind::Function { ref func, .. } => {
                write!(f, "Function {}", func.format(ctx))?;
            }
            ExprKind::Member { ref value, ref mem } => {
                write!(f, "Member {}", mem.format(ctx, &value.tp))?;
            }
            ExprKind::Binary { ref op, .. } => {
                write!(f, "Binary {}", op)?;
            }
            ExprKind::If { .. } => {
                write!(f, "If")?;
            }
            ExprKind::Int(i) => {
                write!(f, "Int {}", i)?;
            }
            ExprKind::Id(ref id) => {
                write!(f, "Id {}", id.format(ctx, def))?;
            }
            ExprKind::Noop => {
                write!(f, "Noop")?;
            }
        }

        write!(f, " -> {}", self.tp.format(ctx))?;
        write!(f, " [{}]", self.span.format(ctx))
    }

    pub fn print<Out>(&self, f: &mut Out, ctx: &Context, def: &FuncDef, ind: i32) -> io::Result<()>
    where
        Out: io::Write,
    {
        for _ in 0..ind {
            write!(f, "{}", INDENT)?;
        }

        self.info(f, ctx, def)?;
        writeln!(f)?;

        self.for_each(|expr| expr.print(f, ctx, def, ind + 1))
    }
}
