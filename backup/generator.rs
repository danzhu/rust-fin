use std;

use symbol::Op;
use ast::{Module, Func, Expr, ExprKind};

#[derive(Debug)]
pub enum GeneratorError {
    IO(std::io::Error),
}

impl std::convert::From<std::io::Error> for GeneratorError {
    fn from(err: std::io::Error) -> Self {
        GeneratorError::IO(err)
    }
}

type GeneratorResult<T> = Result<T, GeneratorError>;

pub struct Generator<'a, Writer: 'a + std::io::Write> {
    writer: &'a mut Writer,
    reg_id: i32,
}

impl<'a, Writer: std::io::Write> Generator<'a, Writer> {
    pub fn new(writer: &mut Writer) -> Generator<Writer> {
        Generator {
            writer,
            reg_id: 0,
        }
    }

    pub fn generate(&mut self, module: &Module) -> GeneratorResult<()> {
        for func in module.functions.iter() {
            self.function(func)?;
        }
        Ok(())
    }

    fn function(&mut self, func: &Func) -> GeneratorResult<()> {
        self.reg_id = 0;

        let params = func.params.iter()
            .map(|p| format!("i32 %{}", p.name))
            .collect::<Vec<_>>()
            .join(", ");

        writeln!(self.writer, "define i32 @{}({}) {{", func.name, params)?;
        writeln!(self.writer, "entry:")?;

        let ret = self.expr(&func.body)?;

        writeln!(self.writer, "  ret i32 {}", ret)?;
        writeln!(self.writer, "}}")?;

        Ok(())
    }

    fn expr(&mut self, expr: &Expr) -> GeneratorResult<String> {
        match expr.kind() {
            &ExprKind::Block { ref exprs } => {
                // TODO: verify that at least one expr exists
                for expr in exprs.iter().take(exprs.len() - 1) {
                    self.expr(expr)?;
                }
                Ok(self.expr(exprs.last().unwrap())?)
            },
            &ExprKind::Let { ref var, ref value } => {
                let reg = format!("%{}", var);
                let value = self.expr(value)?;

                writeln!(self.writer, "  {} = add i32 {}, 0", reg, value)?;
                Ok(reg)
            },
            &ExprKind::Binary { op, ref left, ref right } => {
                let left = self.expr(left)?;
                let right = self.expr(right)?;

                let reg = self.temp();
                let op = match op {
                    Op::Add => "add",
                    Op::Sub => "sub",
                    Op::Mul => "mul",
                    Op::Div => "sdiv",
                    Op::Rem => "srem",
                };

                writeln!(self.writer, "  {} = {} i32 {}, {}", reg, op, left, right)?;
                Ok(reg)
            },
            &ExprKind::Call { ref name, ref args } => {
                let reg = self.temp();

                let mut arg_values = Vec::new();
                for arg in args {
                    arg_values.push(format!("i32 {}", self.expr(arg)?));
                }
                let args = arg_values.join(", ");

                writeln!(self.writer, "  {} = call i32 @{}({})", reg, name, args)?;

                Ok(reg)
            },
            &ExprKind::Int(value) => {
                Ok(format!("{}", value))
            },
            &ExprKind::Id(ref name) => {
                Ok(format!("%{}", name))
            },
        }
    }

    fn temp(&mut self) -> String {
        let res = format!("%{}", self.reg_id);
        self.reg_id += 1;
        res
    }
}
