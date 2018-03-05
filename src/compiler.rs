use std::fmt;
use std::io;
use std::result;

use store::Store;

use lexer;
use parser;
use resolver;
use type_checker;

pub struct Compiler {
    store: Store,
}

pub enum Error {
    Lexer(lexer::Error),
    Parser(parser::Error),
    Resolver(resolver::Error),
    TypeChecker(type_checker::Error),
    IO(io::Error),
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

impl From<resolver::Error> for Error {
    fn from(err: resolver::Error) -> Error {
        Error::Resolver(err)
    }
}

impl From<type_checker::Error> for Error {
    fn from(err: type_checker::Error) -> Error {
        Error::TypeChecker(err)
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::IO(err)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Lexer(ref err) => write!(f, "lexer error: {}", err),
            Error::Parser(ref err) => write!(f, "parser error: {}", err),
            Error::Resolver(ref err) => write!(f, "resolver error: {}", err),
            Error::TypeChecker(ref err) => write!(f, "type checker error: {}", err),
            Error::IO(ref err) => write!(f, "io error: {}", err),
        }
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            store: Store::new(),
        }
    }

    pub fn compile<In, Out>(&mut self, input: In, mut output: Out) -> Result
    where
        In: io::Read,
        Out: io::Write,
    {
        let tokens = lexer::lex(input)?;
        let source = parser::parse(tokens.into_iter())?;
        self.store.define(source);
        resolver::resolve_decls(&mut self.store)?;
        resolver::resolve_defs(&mut self.store)?;
        type_checker::type_check(&mut self.store)?;

        write!(output, "{:?}", self.store)?;

        Ok(())
    }
}
