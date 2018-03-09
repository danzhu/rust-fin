use std::{fmt, io, result};

use def::Store;

use lexer;
use parser;
use name_res;
use type_chk;
use ir_gen;
use code_gen;

pub struct Compiler {
    store: Store,
}

pub enum Error {
    Lexer(lexer::Error),
    Parser(parser::Error),
    NameRes(name_res::Error),
    TypeChecker(type_chk::Error),
    CodeGen(code_gen::Error),
}

type Result = result::Result<(), Error>;

impl From<lexer::Error> for Error {
    fn from(err: lexer::Error) -> Error {
        Error::Lexer(err)
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

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Lexer(ref err) => write!(f, "lexing error: {}", err),
            Error::Parser(ref err) => write!(f, "parsing error: {}", err),
            Error::NameRes(ref err) => write!(f, "name resolution error: {}", err),
            Error::TypeChecker(ref err) => write!(f, "type check error: {}", err),
            Error::CodeGen(ref err) => write!(f, "code generation error: {}", err),
        }
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            store: Store::new(),
        }
    }

    pub fn compile<In, Out>(&mut self, input: In, output: Out) -> Result
    where
        In: io::Read,
        Out: io::Write,
    {
        let tokens = lexer::lex(input)?;
        let source = parser::parse(tokens.into_iter())?;
        self.store.define(source);
        name_res::resolve_decls(&mut self.store)?;
        name_res::resolve_defs(&mut self.store)?;
        type_chk::type_check(&mut self.store)?;
        ir_gen::generate(&mut self.store);

        eprint!("{:?}", self.store);

        code_gen::generate(&self.store, output)?;

        Ok(())
    }
}
