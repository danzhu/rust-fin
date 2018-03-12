use std::{io, iter, result};
use std::collections::HashMap;

use common::*;
use token::*;
use ast::*;
use def::*;
use ctx::*;

struct Parser<Iter: Iterator<Item = Token>> {
    source: iter::Peekable<Iter>,
    span: Span,
}

pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

pub enum ErrorKind {
    Expect {
        expect: &'static str,
        got: Option<TokenKind>,
    },
}

pub type Result<T> = result::Result<T, Error>;

macro_rules! expect {
    ($self:expr, $pat:ident) => {
        match $self.next() {
            Some(TokenKind::$pat) => {},
            got => return $self.error(ErrorKind::Expect {
                expect: stringify!($pat),
                got
            }),
        }
    }
}

macro_rules! expect_value {
    ($self:expr, $pat:ident) => {
        match $self.next() {
            Some(TokenKind::$pat(val)) => val,
            got => return $self.error(ErrorKind::Expect {
                expect: stringify!($pat),
                got
            }),
        }
    }
}

pub fn parse<Iter>(tokens: Iter) -> Result<Source>
where
    Iter: Iterator<Item = Token>,
{
    let mut par = Parser {
        source: tokens.peekable(),
        span: Span::ZERO,
    };
    par.module()
}

impl<Iter: Iterator<Item = Token>> Parser<Iter> {
    fn module(&mut self) -> Result<Source> {
        let mut src = Source::new();
        while self.peek().is_some() {
            src.defs.push(self.def()?);
        }
        Ok(src)
    }

    fn def(&mut self) -> Result<Def> {
        let kind = match self.next() {
            Some(TokenKind::Struct) => DefKind::Type(self.structure()?),
            Some(TokenKind::Def) => DefKind::Func(self.func()?),
            got => {
                return self.error(ErrorKind::Expect {
                    expect: "top-level definition",
                    got,
                })
            }
        };

        Ok(Def::new(kind))
    }

    fn structure(&mut self) -> Result<TypeDef> {
        let name = expect_value!(self, Type);

        let fields = self.bind_list()?;

        expect!(self, Period);

        Ok(TypeDef::new(
            name,
            TypeDefKind::Struct {
                fields,
                sym_table: HashMap::new(),
            },
        ))
    }

    fn func(&mut self) -> Result<FuncDef> {
        let start = self.start_span();

        let name = expect_value!(self, Id);

        let params = self.bind_list()?;

        let ret = if let Some(&TokenKind::Arrow) = self.peek() {
            self.next();
            self.tp()?
        } else {
            Type::new(TypeKind::Void)
        };

        expect!(self, As);

        let body = self.block()?;

        expect!(self, Period);

        let span = self.end_span(start);
        Ok(FuncDef::new(name, params, ret, body, span))
    }

    fn bind_list(&mut self) -> Result<Vec<BindDef>> {
        let mut binds = Vec::new();
        while let Some(&TokenKind::Id(_)) = self.peek() {
            binds.push(self.bind()?);
        }
        Ok(binds)
    }

    fn bind(&mut self) -> Result<BindDef> {
        let start = self.start_span();
        let name = expect_value!(self, Id);
        let tp = self.tp()?;
        let span = self.end_span(start);
        Ok(BindDef::new(name, tp, span))
    }

    fn block(&mut self) -> Result<Expr> {
        let start = self.start_span();

        let stmt = self.statement()?;
        if let Some(&TokenKind::Comma) = self.peek() {
            let mut stmts = vec![stmt];
            while let Some(&TokenKind::Comma) = self.peek() {
                self.next();
                stmts.push(self.statement()?);
            }

            let span = self.end_span(start);
            Ok(Expr::new(ExprKind::Block { stmts }, span))
        } else {
            Ok(stmt)
        }
    }

    fn statement(&mut self) -> Result<Expr> {
        let start = self.start_span();

        let expr = self.expr()?;
        if let Some(&TokenKind::Arrow) = self.peek() {
            self.next();
            // TODO: pattern
            expect!(self, Let);
            let var = expect_value!(self, Id);

            let span = self.end_span(start);
            Ok(Expr::new(
                ExprKind::Let {
                    value: Box::new(expr),
                    var: Bind::new(Path::new(var)),
                },
                span,
            ))
        } else {
            Ok(expr)
        }
    }

    fn expr(&mut self) -> Result<Expr> {
        let start = self.start_span();

        let mut expr = self.term()?;
        while let Some(&TokenKind::Operator(_)) = self.peek() {
            let op = expect_value!(self, Operator);
            let right = self.term()?;

            let span = self.end_span(start);
            expr = Expr::new(
                ExprKind::Binary {
                    op,
                    left: Box::new(expr),
                    right: Box::new(right),
                },
                span,
            );
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        let mut expr = if let Some(&TokenKind::Quote) = self.peek() {
            let start = self.start_span();

            self.next();
            let kind = match self.next() {
                Some(TokenKind::Id(name)) => {
                    let args = self.args()?;
                    ExprKind::Function {
                        func: Func::new(Path::new(name)),
                        args,
                    }
                }
                Some(TokenKind::Type(name)) => {
                    let args = self.args()?;
                    ExprKind::Construct {
                        tp: Type::new(TypeKind::Named {
                            path: Path::new(name),
                        }),
                        args,
                    }
                }
                got => {
                    return self.error(ErrorKind::Expect {
                        expect: "type or function",
                        got,
                    })
                }
            };

            let span = self.end_span(start);
            Expr::new(kind, span)
        } else {
            self.factor()?
        };

        while let Some(&TokenKind::Quote) = self.peek() {
            let start = self.start_span();

            self.next();
            let name = expect_value!(self, Id);

            let span = self.end_span(start);
            expr = Expr::new(
                ExprKind::Member {
                    value: Box::new(expr),
                    mem: Member::new(Path::new(name)),
                },
                span,
            );
        }

        Ok(expr)
    }

    fn args(&mut self) -> Result<Vec<Expr>> {
        let mut args = Vec::new();
        loop {
            // TODO: remove ugly duplicate code
            match self.peek() {
                Some(&TokenKind::Id(_))
                | Some(&TokenKind::Int(_))
                | Some(&TokenKind::If)
                | Some(&TokenKind::LParen) => {
                    args.push(self.factor()?);
                }
                _ => break,
            }
        }
        Ok(args)
    }

    fn factor(&mut self) -> Result<Expr> {
        let start = self.start_span();

        let kind = match self.next() {
            Some(TokenKind::Id(name)) => ExprKind::Id(Bind::new(Path::new(name))),
            Some(TokenKind::Int(val)) => ExprKind::Int(val),
            Some(TokenKind::If) => {
                let cond = self.cond()?;
                expect!(self, Period);
                return Ok(cond);
            }
            Some(TokenKind::LParen) => {
                let block = self.block()?;
                expect!(self, RParen);
                return Ok(block);
            }
            got => {
                return self.error(ErrorKind::Expect {
                    expect: "value",
                    got,
                })
            }
        };

        let span = self.end_span(start);
        Ok(Expr::new(kind, span))
    }

    fn cond(&mut self) -> Result<Expr> {
        let start = self.start_span();

        let cond = self.block()?;

        expect!(self, Then);
        let succ = self.block()?;

        let fail = match self.peek() {
            Some(&TokenKind::Else) => {
                self.next();
                self.block()?
            }
            Some(&TokenKind::Elif) => {
                self.next();
                self.cond()?
            }
            _ => {
                let end = self.span.end;
                Expr::new(ExprKind::Noop, Span::new(end, end))
            }
        };

        let span = self.end_span(start);
        Ok(Expr::new(
            ExprKind::If {
                cond: Box::new(cond),
                succ: Box::new(succ),
                fail: Box::new(fail),
            },
            span,
        ))
    }

    fn start_span(&mut self) -> Option<Pos> {
        self.source.peek().map(|token| token.span.start)
    }

    fn end_span(&mut self, start: Option<Pos>) -> Span {
        Span {
            start: start.expect("start pos for expr is none"),
            end: self.span.end,
        }
    }

    fn next(&mut self) -> Option<TokenKind> {
        self.source.next().map(|Token { kind, span }| {
            self.span = span;
            kind
        })
    }

    fn peek(&mut self) -> Option<&TokenKind> {
        self.source.peek().map(|token| &token.kind)
    }

    fn error<T>(&self, kind: ErrorKind) -> Result<T> {
        Err(Error {
            kind,
            span: self.span,
        })
    }

    fn tp(&mut self) -> Result<Type> {
        let name = expect_value!(self, Type);
        Ok(Type::new(TypeKind::Named {
            path: Path::new(name),
        }))
    }
}

impl Error {
    pub fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        write!(f, "{}: error: ", self.span.start.format(ctx))?;
        match self.kind {
            ErrorKind::Expect {
                expect,
                got: Some(ref got),
            } => {
                // TODO: make display for token
                write!(f, "expect {}, but got {:?}", expect, got)?;
            }
            ErrorKind::Expect { expect, got: None } => {
                write!(f, "expect {}, but reached EOF", expect)?;
            }
        }
        Ok(())
    }
}
