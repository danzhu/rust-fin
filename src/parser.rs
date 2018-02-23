use std::fmt;
use std::iter;
use std::result;

use lexer::{Token, TokenKind};
use ast::*;

pub enum Error {
    Expect(&'static str, Option<TokenKind>),
}

type Result<T> = result::Result<T, Error>;

pub struct Parser<T: Iterator<Item = Token>> {
    source: iter::Peekable<T>,
}

macro_rules! expect_token {
    ($src:expr, $pat:pat, $res:expr) => {
        match $src.next() {
            Some($pat) => $res,
            got => return Err(Error::Expect(stringify!($pat), got)),
        }
    };
    ($src:expr, $pat:pat) => {
        expect_token!($src, $pat, {})
    };
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Expect(val, ref token) => write!(f, "expect {}, but got {:?}", val, token),
        }
    }
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(source: T) -> Self {
        Self {
            source: source.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Source> {
        self.module()
    }

    fn next(&mut self) -> Option<TokenKind> {
        self.source.next().map(|token| token.kind)
    }

    fn peek(&mut self) -> Option<&TokenKind> {
        self.source.peek().map(|token| &token.kind)
    }

    fn tp(&mut self) -> Result<Type> {
        let name = expect_token!(self, TokenKind::Id(name), name);
        Ok(Type::Named {
            path: Path { name },
        })
    }

    fn module(&mut self) -> Result<Source> {
        let mut defs = Vec::new();
        while self.peek().is_some() {
            defs.push(Def {
                kind: DefKind::Function(self.func()?),
            });
        }
        Ok(Source { defs })
    }

    fn func(&mut self) -> Result<Function> {
        expect_token!(self, TokenKind::Def);
        let name = expect_token!(self, TokenKind::Id(name), name);

        let mut params = Vec::new();
        while let Some(&TokenKind::Id(_)) = self.peek() {
            let name = expect_token!(self, TokenKind::Id(name), name);
            let tp = self.tp()?;
            params.push(Binding { name, tp });
        }

        let ret = if let Some(&TokenKind::Arrow) = self.peek() {
            self.next();
            self.tp()?
        } else {
            Type::Void
        };

        expect_token!(self, TokenKind::As);

        let body = self.seq()?;

        expect_token!(self, TokenKind::Period);

        Ok(Function {
            name,
            params,
            ret,
            body,
        })
    }

    fn seq(&mut self) -> Result<Expr> {
        let expr = self.statement()?;
        if let Some(&TokenKind::Comma) = self.peek() {
            self.next();
            let seq = self.seq()?;
            Ok(Expr {
                kind: ExprKind::Seq {
                    first: Box::new(expr),
                    second: Box::new(seq),
                },
            })
        } else {
            Ok(expr)
        }
    }

    fn statement(&mut self) -> Result<Expr> {
        let expr = self.expr()?;
        if let Some(&TokenKind::Arrow) = self.peek() {
            self.next();
            // TODO: pattern
            expect_token!(self, TokenKind::Let);
            let var = expect_token!(self, TokenKind::Id(name), name);
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

    fn expr(&mut self) -> Result<Expr> {
        let mut expr = self.term()?;
        while let Some(&TokenKind::Operator(_)) = self.peek() {
            let op = expect_token!(self, TokenKind::Operator(op), op);
            let right = self.term()?;
            expr = Expr {
                kind: ExprKind::Binary {
                    op,
                    left: Box::new(expr),
                    right: Box::new(right),
                },
            };
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        match self.peek() {
            Some(&TokenKind::Quote) => {
                self.next();
                let name = expect_token!(self, TokenKind::Id(name), name);
                let args = self.args()?;
                Ok(Expr {
                    kind: ExprKind::Function {
                        path: Path { name },
                        args,
                    },
                })
            }
            _ => self.factor(),
        }

        // TODO: method
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
        match self.next() {
            Some(TokenKind::Id(name)) => Ok(Expr {
                kind: ExprKind::Id(name),
            }),
            Some(TokenKind::Int(val)) => Ok(Expr {
                kind: ExprKind::Int(val),
            }),
            Some(TokenKind::If) => {
                let cond = self.cond()?;
                expect_token!(self, TokenKind::Period);
                Ok(cond)
            }
            Some(TokenKind::LParen) => {
                let seq = self.seq()?;
                expect_token!(self, TokenKind::RParen);
                Ok(seq)
            }
            got => Err(Error::Expect("value", got)),
        }
    }

    fn cond(&mut self) -> Result<Expr> {
        let cond = self.seq()?;

        expect_token!(self, TokenKind::Then);
        let succ = self.seq()?;

        let fail = match self.peek() {
            Some(&TokenKind::Else) => {
                self.next();
                self.seq()?
            }
            Some(&TokenKind::Elif) => {
                self.next();
                self.cond()?
            }
            _ => Expr {
                kind: ExprKind::Noop,
            },
        };

        Ok(Expr {
            kind: ExprKind::If {
                cond: Box::new(cond),
                succ: Box::new(succ),
                fail: Box::new(fail),
            },
        })
    }
}
