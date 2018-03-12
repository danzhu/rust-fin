use std::{io, result};

use ctx::*;

use parser;
use name_res;
use type_chk;
use ir_gen;
use code_gen;

pub struct Compiler {
    ctx: Context,
}

pub enum Error {
    Io(io::Error),
    Parser(parser::Error),
    NameRes(name_res::Error),
    TypeChecker(type_chk::Error),
    CodeGen(code_gen::Error),
}

type Result = result::Result<(), Error>;

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}

impl From<parser::Error> for Error {
    fn from(err: parser::Error) -> Error {
        Error::Parser(err)
    }
}

impl From<name_res::Error> for Error {
    fn from(err: name_res::Error) -> Error {
        Error::NameRes(err)
    }
}

impl From<type_chk::Error> for Error {
    fn from(err: type_chk::Error) -> Error {
        Error::TypeChecker(err)
    }
}

impl From<code_gen::Error> for Error {
    fn from(err: code_gen::Error) -> Error {
        Error::CodeGen(err)
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            ctx: Context::new(),
        }
    }

    pub fn compile<In, Out>(&mut self, mut input: In, output: Out) -> Result
    where
        In: io::Read,
        Out: io::Write,
    {
        let filename = "<stdin>".to_string();
        let mut content = String::new();
        input.read_to_string(&mut content)?;

        parser::parse(filename, &content, &mut self.ctx)?;
        name_res::resolve_decls(&mut self.ctx)?;
        name_res::resolve_defs(&mut self.ctx)?;
        type_chk::type_check(&mut self.ctx)?;
        ir_gen::generate(&mut self.ctx);

        self.ctx
            .print(&mut io::stderr())
            .expect("failed to dump context");

        code_gen::generate(&self.ctx, output)?;

        Ok(())
    }

    pub fn explain<Out>(&self, err: &Error, output: &mut Out) -> io::Result<()>
    where
        Out: io::Write,
    {
        match *err {
            Error::Io(ref err) => write!(output, "{}", err),
            Error::Parser(ref err) => err.print(output, &self.ctx),
            Error::NameRes(ref err) => err.print(output, &self.ctx),
            Error::TypeChecker(ref err) => err.print(output, &self.ctx),
            Error::CodeGen(ref err) => write!(output, "{}", err),
        }
    }
}
