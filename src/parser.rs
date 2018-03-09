use std::{fmt, iter, result};
use std::collections::HashMap;

use common::*;
use token::*;
use ast::*;
use def::*;

struct Parser<T: Iterator<Item = Token>> {
    source: iter::Peekable<T>,
}

pub enum Error {
    Expect(&'static str, Option<TokenKind>),
}

pub type Result<T> = result::Result<T, Error>;

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

pub fn parse<Iter>(tokens: Iter) -> Result<Source>
where
    Iter: Iterator<Item = Token>,
{
    let mut par = Parser {
        source: tokens.peekable(),
    };
    par.module()
}

impl<T: Iterator<Item = Token>> Parser<T> {
    fn next(&mut self) -> Option<TokenKind> {
        self.source.next().map(|token| token.kind)
    }

    fn peek(&mut self) -> Option<&TokenKind> {
        self.source.peek().map(|token| &token.kind)
    }

    fn tp(&mut self) -> Result<Type> {
        let name = expect_token!(self, TokenKind::Type(name), name);
        Ok(Type::new(TypeKind::Named {
            path: Path::new(name),
        }))
    }

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
            got => return Err(Error::Expect("top-level definition", got)),
        };

        Ok(Def::new(kind))
    }

    fn structure(&mut self) -> Result<TypeDef> {
        let name = expect_token!(self, TokenKind::Type(name), name);

        let fields = self.bind_list()?;

        expect_token!(self, TokenKind::Period);

        Ok(TypeDef::new(
            name,
            TypeDefKind::Struct {
                fields,
                sym_table: HashMap::new(),
            },
        ))
    }

    fn func(&mut self) -> Result<FuncDef> {
        let name = expect_token!(self, TokenKind::Id(name), name);

        let params = self.bind_list()?;

        let ret = if let Some(&TokenKind::Arrow) = self.peek() {
            self.next();
            self.tp()?
        } else {
            Type::new(TypeKind::Void)
        };

        expect_token!(self, TokenKind::As);

        let body = self.block()?;

        expect_token!(self, TokenKind::Period);

        Ok(FuncDef::new(name, params, ret, body))
    }

    fn bind_list(&mut self) -> Result<Vec<BindDef>> {
        let mut binds = Vec::new();
        while let Some(&TokenKind::Id(_)) = self.peek() {
            binds.push(self.bind()?);
        }
        Ok(binds)
    }

    fn bind(&mut self) -> Result<BindDef> {
        let name = expect_token!(self, TokenKind::Id(name), name);
        let mut bind = BindDef::new(name);
        bind.tp = self.tp()?;
        Ok(bind)
    }

    fn block(&mut self) -> Result<Expr> {
        let stmt = self.statement()?;
        if let Some(&TokenKind::Comma) = self.peek() {
            let mut stmts = vec![stmt];
            while let Some(&TokenKind::Comma) = self.peek() {
                self.next();
                stmts.push(self.statement()?);
            }
            Ok(Expr::new(ExprKind::Block { stmts }))
        } else {
            Ok(stmt)
        }
    }

    fn statement(&mut self) -> Result<Expr> {
        let expr = self.expr()?;
        if let Some(&TokenKind::Arrow) = self.peek() {
            self.next();
            // TODO: pattern
            expect_token!(self, TokenKind::Let);
            let var = expect_token!(self, TokenKind::Id(name), name);
            Ok(Expr::new(ExprKind::Let {
                value: Box::new(expr),
                var: Bind::new(Path::new(var)),
            }))
        } else {
            Ok(expr)
        }
    }

    fn expr(&mut self) -> Result<Expr> {
        let mut expr = self.term()?;
        while let Some(&TokenKind::Operator(_)) = self.peek() {
            let op = expect_token!(self, TokenKind::Operator(op), op);
            let right = self.term()?;
            expr = Expr::new(ExprKind::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        let mut expr = if let Some(&TokenKind::Quote) = self.peek() {
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
                got => return Err(Error::Expect("type or function", got)),
            };
            Expr::new(kind)
        } else {
            self.factor()?
        };

        while let Some(&TokenKind::Quote) = self.peek() {
            self.next();
            let name = expect_token!(self, TokenKind::Id(name), name);
            expr = Expr::new(ExprKind::Member {
                value: Box::new(expr),
                mem: Member::new(Path::new(name)),
            });
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
        match self.next() {
            Some(TokenKind::Id(name)) => Ok(Expr::new(ExprKind::Id(Bind::new(Path::new(name))))),
            Some(TokenKind::Int(val)) => Ok(Expr::new(ExprKind::Int(val))),
            Some(TokenKind::If) => {
                let cond = self.cond()?;
                expect_token!(self, TokenKind::Period);
                Ok(cond)
            }
            Some(TokenKind::LParen) => {
                let block = self.block()?;
                expect_token!(self, TokenKind::RParen);
                Ok(block)
            }
            got => Err(Error::Expect("value", got)),
        }
    }

    fn cond(&mut self) -> Result<Expr> {
        let cond = self.block()?;

        expect_token!(self, TokenKind::Then);
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
            _ => Expr::new(ExprKind::Noop),
        };

        Ok(Expr::new(ExprKind::If {
            cond: Box::new(cond),
            succ: Box::new(succ),
            fail: Box::new(fail),
        }))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Expect(exp, ref token) => write!(f, "expect {}, but got {:?}", exp, token),
        }
    }
}
