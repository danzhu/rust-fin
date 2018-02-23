use std::fmt;
use std::io;
use std::result;

use store;
use lexer;
use parser;

pub struct Compiler {
    store: store::Store,
}

pub enum Error {
    Lexer(lexer::Error),
    Parser(parser::Error),
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
            Error::IO(ref err) => write!(f, "io error: {}", err),
        }
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            store: store::Store::new(),
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

        write!(output, "{:?}", self.store)?;

        Ok(())
    }
}
