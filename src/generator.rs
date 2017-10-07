use std::io;
use ast::{Module, Func, Expr};
use operator::Op;

type Error<T> = Result<T, io::Error>;

pub struct Generator<'a, Writer: 'a + io::Write> {
    writer: &'a mut Writer,
    reg_id: i32,
}

impl<'a, Writer: io::Write> Generator<'a, Writer> {
    pub fn new(writer: &mut Writer) -> Generator<Writer> {
        Generator {
            writer,
            reg_id: 1,
        }
    }

    pub fn generate(&mut self, module: &Module) -> Error<()> {
        for func in module.functions.iter() {
            self.function(&func)?;
        }
        Ok(())
    }

    fn function(&mut self, func: &Func) -> Error<()> {
        writeln!(self.writer, "define i32 @{}() {{", func.name)?;
        let ret = self.expr(&func.expr)?;
        writeln!(self.writer, "  ret i32 {}", ret)?;
        writeln!(self.writer, "}}")?;
        Ok(())
    }

    fn expr(&mut self, expr: &Expr) -> Error<String> {
        match expr {
            &Expr::Int(value) => {
                Ok(format!("{}", value))
            },
            &Expr::Binary { ref op, ref left, ref right } => {
                let left = self.expr(&*left)?;
                let right = self.expr(&*right)?;

                let reg = self.reg();
                let op = match op {
                    &Op::Add => "add",
                    &Op::Sub => "sub",
                    &Op::Mul => "mul",
                    &Op::Div => "sdiv",
                    &Op::Rem => "srem",
                };

                writeln!(self.writer, "  {} = {} i32 {}, {}", reg, op, left, right)?;
                Ok(reg)
            },
        }
    }

    fn reg(&mut self) -> String {
        let res = format!("%{}", self.reg_id);
        self.reg_id += 1;
        res
    }
}
