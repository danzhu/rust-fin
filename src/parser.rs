use std;

use lexer::{Paren, Token};

macro_rules! expect {
    ($src:expr, $pat:pat, $res:expr) => (
        match $src.next() {
            Some($pat) => $res,
            token => return Err(ParserError::Expect(stringify!($pat), token)),
        }
    );
    ($src:expr, $pat:pat) => (
        expect!($src, $pat, {})
    );
}

#[derive(Debug)]
pub struct Decl {
    pub name: String,
}

#[derive(Debug)]
pub struct Module {
    pub functions: Vec<Func>,
}

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub params: Vec<Decl>,
    pub body: Expr,
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Block {
        exprs: Vec<Expr>,
    },
    Let {
        value: Box<Expr>,
        var: String,
    },
    Function {
        name: String,
        args: Vec<Expr>,
    },
    Method {
        expr: Box<Expr>,
        name: String,
        args: Vec<Expr>,
    },
    Int(i32),
    Id(String),
}

#[derive(Debug)]
pub enum ParserError {
    Expect(&'static str, Option<Token>),
}

type ParserResult<T> = Result<T, ParserError>;

pub struct Parser<T: Iterator<Item = Token>> {
    source: std::iter::Peekable<T>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(source: T) -> Parser<T> {
        Parser {
            source: source.peekable(),
        }
    }

    pub fn parse(&mut self) -> ParserResult<Module> {
        let mut functions = Vec::new();
        while self.source.peek().is_some() {
            functions.push(self.func()?);
        }
        Ok(Module { functions })
    }

    fn func(&mut self) -> ParserResult<Func> {
        expect!(self.source, Token::Def);
        let name = expect!(self.source, Token::Id(name), name);

        let mut params = Vec::new();
        while let Some(&Token::Id(_)) = self.source.peek() {
            let name = expect!(self.source, Token::Id(name), name);
            params.push(Decl { name });
        }

        expect!(self.source, Token::As);

        let body = self.block()?;

        expect!(self.source, Token::Period);

        Ok(Func { name, params, body })
    }

    fn block(&mut self) -> ParserResult<Expr> {
        let mut exprs = Vec::new();
        loop {
            exprs.push(self.statement()?);
            match self.source.peek() {
                Some(&Token::Comma) => {
                    self.source.next();
                }
                _ => break,
            }
        }

        let kind = ExprKind::Block { exprs };

        Ok(Expr { kind })
    }

    fn statement(&mut self) -> ParserResult<Expr> {
        let expr = self.expr()?;
        if let Some(&Token::Arrow) = self.source.peek() {
            self.source.next();
            // TODO: pattern
            expect!(self.source, Token::Let);
            let var = expect!(self.source, Token::Id(name), name);
            Ok(Expr {
                kind: ExprKind::Let {
                    value: Box::new(expr),
                    var,
                },
            })
        } else {
            Ok(expr)
        }
    }

    fn expr(&mut self) -> ParserResult<Expr> {
        let mut expr = match self.source.peek() {
            Some(&Token::Function(_)) => {
                let name = expect!(self.source, Token::Function(name), name);
                let args = self.args()?;
                Expr {
                    kind: ExprKind::Function { name, args },
                }
            }
            _ => self.term()?,
        };

        while let Some(&Token::Function(_)) = self.source.peek() {
            let name = expect!(self.source, Token::Function(name), name);
            let args = self.args()?;
            let kind = ExprKind::Method {
                expr: Box::new(expr),
                name,
                args,
            };
            expr = Expr { kind };
        }

        Ok(expr)
    }

    fn args(&mut self) -> ParserResult<Vec<Expr>> {
        let mut args = Vec::new();
        loop {
            // TODO: remove ugly duplicate code
            let expr = match self.source.peek() {
                Some(&Token::Id(_)) | Some(&Token::Int(_)) | Some(&Token::Open(Paren::Paren)) => {
                    self.term()?
                }
                _ => break,
            };
            args.push(expr);
        }
        Ok(args)
    }

    fn term(&mut self) -> ParserResult<Expr> {
        let kind = match self.source.next() {
            Some(Token::Id(name)) => ExprKind::Id(name),
            Some(Token::Int(val)) => ExprKind::Int(val),
            Some(Token::Open(Paren::Paren)) => {
                let block = self.block()?;
                expect!(self.source, Token::Close(Paren::Paren));
                return Ok(block);
            }
            token => {
                return Err(ParserError::Expect(
                    "binding, literal, or open parenthesis",
                    token,
                ))
            }
        };

        Ok(Expr { kind })
    }
}
