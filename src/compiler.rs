use std;

use lexer::{Lexer, LexerError};
use parser::{Parser, ParserError};

pub enum CompilerError {
    Lexer(LexerError),
    Parser(ParserError),
    IO(std::io::Error),
}

type CompilerResult<T> = Result<T, CompilerError>;

impl From<LexerError> for CompilerError {
    fn from(err: LexerError) -> CompilerError {
        CompilerError::Lexer(err)
    }
}

impl From<ParserError> for CompilerError {
    fn from(err: ParserError) -> CompilerError {
        CompilerError::Parser(err)
    }
}

impl From<std::io::Error> for CompilerError {
    fn from(err: std::io::Error) -> CompilerError {
        CompilerError::IO(err)
    }
}

impl std::fmt::Debug for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            &CompilerError::Lexer(ref err) => write!(f, "lexer error: {:?}", err),
            &CompilerError::Parser(ref err) => write!(f, "parser error: {:?}", err),
            &CompilerError::IO(ref err) => write!(f, "io error: {}", err),
        }
    }
}

pub fn compile<In, Out>(input: In, mut output: Out) -> CompilerResult<()>
where
    In: Iterator<Item = char>,
    Out: std::io::Write,
{
    // lex
    let tokens = {
        let lex = Lexer::new(input);
        lex.collect::<Result<Vec<_>, _>>()?
    };

    // parse
    let module = {
        let mut par = Parser::new(tokens.into_iter());
        par.parse()?
    };

    writeln!(output, "{:?}", module)?;

    // // generate
    // let out = &mut std::io::stdout();
    // let mut gen = Generator::new(out);
    // gen.generate(&module).expect("code generator error");

    Ok(())
}
