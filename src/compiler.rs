use std::io;
use std::fmt;
use std::result;

use lexer::{self, Lexer};
use parser::{self, Parser};

pub struct Compiler {}

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
        Self {}
    }

    pub fn compile<In, Out>(&self, mut input: In, mut output: Out) -> Result
    where
        In: io::Read,
        Out: io::Write,
    {
        // lex
        let tokens = {
            let mut src = String::new();
            input.read_to_string(&mut src)?;
            let lex = Lexer::new(src.chars());
            lex.collect::<result::Result<Vec<_>, _>>()?
        };

        // parse
        let module = {
            let mut par = Parser::new(tokens.into_iter());
            par.parse()?
        };

        write!(output, "{:?}", module)?;

        Ok(())
    }
}
