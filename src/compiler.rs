use std::{io, result};

use error::*;
use ctx::*;

use parser;
use ast_gen;
use ir_gen;
use code_gen;

pub struct Compiler {
    ctx: Context,
}

pub enum Error {
    Io(io::Error),
    Parser(parser::Error),
    AstGen(ast_gen::Error),
    CodeGen(code_gen::Error),
}

pub type Result = result::Result<(), Error>;

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

impl From<ast_gen::Error> for Error {
    fn from(err: ast_gen::Error) -> Error {
        Error::AstGen(err)
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
        ast_gen::gen(&mut self.ctx)?;
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
            Error::AstGen(ref err) => err.print(output, &self.ctx),
            Error::CodeGen(ref err) => write!(output, "{}", err),
        }
    }
}
