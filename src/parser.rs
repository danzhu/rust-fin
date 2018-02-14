use std;

use lexer::Token;

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

pub struct NamedType {
    pub name: String,
}

pub enum Type {
    Named(NamedType),
    Void,
}

pub struct Decl {
    pub name: String,
    pub tp: Type,
}

pub struct Module {
    pub functions: Vec<Func>,
}

pub struct Func {
    pub name: String,
    pub params: Vec<Decl>,
    pub ret: Type,
    pub body: Expr,
}

pub struct Expr {
    pub kind: ExprKind,
}

pub enum ExprKind {
    Seq {
        first: Box<Expr>,
        second: Box<Expr>,
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
    If {
        cond: Box<Expr>,
        succ: Box<Expr>,
        fail: Box<Expr>,
    },
    Int(i32),
    Id(String),
    Noop,
}

pub enum ParserError {
    Expect(&'static str, Option<Token>),
}

type ParserResult<T> = Result<T, ParserError>;

pub struct Parser<T: Iterator<Item = Token>> {
    source: std::iter::Peekable<T>,
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            &Type::Named(NamedType { ref name }) => write!(f, "{}", name)?,
            &Type::Void => write!(f, "Void")?,
        }
        Ok(())
    }
}

impl std::fmt::Debug for Decl {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} {:?}", self.name, self.tp)?;
        Ok(())
    }
}

impl std::fmt::Debug for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for func in &self.functions {
            write!(f, "{:?}", func)?;
        }
        Ok(())
    }
}

impl std::fmt::Debug for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Func {}", self.name)?;
        for param in &self.params {
            write!(f, " {:?}", param)?;
        }
        writeln!(f, " -> {:?}", self.ret)?;
        writeln!(f, "{:?}", self.body)?;
        Ok(())
    }
}

impl Expr {
    fn new(kind: ExprKind) -> Expr {
        Expr { kind }
    }

    fn debug_fmt(&self, f: &mut std::fmt::Formatter, ind: i32) -> std::fmt::Result {
        for _ in 0..ind {
            write!(f, "  ")?;
        }
        match self.kind {
            ExprKind::Seq {
                ref first,
                ref second,
            } => {
                writeln!(f, "Seq")?;
                first.debug_fmt(f, ind + 1)?;
                second.debug_fmt(f, ind + 1)?;
            }
            ExprKind::Let { ref value, ref var } => {
                writeln!(f, "Let {}", var)?;
                value.debug_fmt(f, ind + 1)?;
            }
            ExprKind::Function { ref name, ref args } => {
                writeln!(f, "Function {}", name)?;
                for arg in args {
                    arg.debug_fmt(f, ind + 1)?;
                }
            }
            ExprKind::Method {
                ref expr,
                ref name,
                ref args,
            } => {
                writeln!(f, "Method {}", name)?;
                expr.debug_fmt(f, ind + 1)?;
                for arg in args {
                    arg.debug_fmt(f, ind + 1)?;
                }
            }
            ExprKind::If {
                ref cond,
                ref succ,
                ref fail,
            } => {
                writeln!(f, "If")?;
                cond.debug_fmt(f, ind + 1)?;
                succ.debug_fmt(f, ind + 1)?;
                fail.debug_fmt(f, ind + 1)?;
            }
            ExprKind::Int(i) => {
                writeln!(f, "Int {}", i)?;
            }
            ExprKind::Id(ref id) => {
                writeln!(f, "Id {}", id)?;
            }
            _ => {}
        }
        Ok(())
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.debug_fmt(f, 1)
    }
}

impl std::fmt::Debug for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            &ParserError::Expect(val, ref token) => {
                write!(f, "expect {}, but got {:?}", val, token)?;
            }
        }
        Ok(())
    }
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(source: T) -> Parser<T> {
        Parser {
            source: source.peekable(),
        }
    }

    pub fn parse(&mut self) -> ParserResult<Module> {
        self.module()
    }

    fn tp(&mut self) -> ParserResult<Type> {
        let name = expect!(self.source, Token::Id(name), name);
        Ok(Type::Named(NamedType { name }))
    }

    fn module(&mut self) -> ParserResult<Module> {
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
            let tp = self.tp()?;
            params.push(Decl { name, tp });
        }

        let ret = if let Some(&Token::Arrow) = self.source.peek() {
            self.source.next();
            self.tp()?
        } else {
            Type::Void
        };

        expect!(self.source, Token::As);

        let body = self.seq()?;

        expect!(self.source, Token::Period);

        Ok(Func {
            name,
            params,
            ret,
            body,
        })
    }

    fn seq(&mut self) -> ParserResult<Expr> {
        let expr = self.statement()?;
        if let Some(&Token::Comma) = self.source.peek() {
            self.source.next();
            let seq = self.seq()?;
            Ok(Expr::new(ExprKind::Seq {
                first: Box::new(expr),
                second: Box::new(seq),
            }))
        } else {
            Ok(expr)
        }
    }

    fn statement(&mut self) -> ParserResult<Expr> {
        let expr = self.expr()?;
        if let Some(&Token::Arrow) = self.source.peek() {
            self.source.next();
            // TODO: pattern
            expect!(self.source, Token::Let);
            let var = expect!(self.source, Token::Id(name), name);
            Ok(Expr::new(ExprKind::Let {
                value: Box::new(expr),
                var,
            }))
        } else {
            Ok(expr)
        }
    }

    fn expr(&mut self) -> ParserResult<Expr> {
        let mut expr = match self.source.peek() {
            Some(&Token::Function(_)) => {
                let name = expect!(self.source, Token::Function(name), name);
                let args = self.args()?;
                Expr::new(ExprKind::Function { name, args })
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
            expr = Expr::new(kind);
        }

        Ok(expr)
    }

    fn args(&mut self) -> ParserResult<Vec<Expr>> {
        let mut args = Vec::new();
        loop {
            // TODO: remove ugly duplicate code
            match self.source.peek() {
                Some(&Token::Id(_)) | Some(&Token::Int(_)) | Some(&Token::If)
                | Some(&Token::LParen) => {
                    args.push(self.term()?);
                }
                _ => break,
            }
        }
        Ok(args)
    }

    fn term(&mut self) -> ParserResult<Expr> {
        let kind = match self.source.next() {
            Some(Token::Id(name)) => ExprKind::Id(name),
            Some(Token::Int(val)) => ExprKind::Int(val),
            Some(Token::If) => {
                let cond = self.cond()?;
                expect!(self.source, Token::Period);
                return Ok(cond);
            }
            Some(Token::LParen) => {
                let seq = self.seq()?;
                expect!(self.source, Token::RParen);
                return Ok(seq);
            }
            token => return Err(ParserError::Expect("value", token)),
        };

        Ok(Expr::new(kind))
    }

    fn cond(&mut self) -> ParserResult<Expr> {
        let cond = self.seq()?;

        expect!(self.source, Token::Then);
        let succ = self.seq()?;

        let fail = match self.source.peek() {
            Some(&Token::Else) => {
                self.source.next();
                self.seq()?
            }
            Some(&Token::Elif) => {
                self.source.next();
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
