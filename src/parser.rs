use std::{io, iter, num, result};

use common::*;
use error::*;
use token::*;
use ptree::*;
use ctx::*;

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

pub fn parse(filename: String, content: &str, ctx: &mut Context) -> Result<()> {
    let src = SourceNode {
        filename,
        lines: content.lines().map(|s| s.to_string()).collect(),
        items: Vec::new(),
    };

    let idx = ctx.sources.push(src);

    let lex = Lexer {
        source: content.chars().peekable(),
        pos: Pos {
            file: idx,
            line: 0,
            column: 0,
        },
    };
    let tokens = lex.collect::<Result<Vec<_>>>()?;

    let mut par = Parser {
        source: tokens.into_iter().peekable(),
        span: Span::zero(idx),
    };

    let mut items = Vec::new();

    while par.peek().is_some() {
        let item = match par.next() {
            Some(TokenKind::Struct) => {
                let def = par.structure()?;
                ItemNode::Type(def)
            }
            Some(TokenKind::Def) => {
                let def = par.func()?;
                ItemNode::Func(def)
            }
            got => {
                return par.error(ErrorKind::Expect {
                    expect: "top-level definition",
                    got,
                })
            }
        };
        items.push(item);
    }

    ctx.sources[idx].items = items;
    Ok(())
}

struct Lexer<Iter>
where
    Iter: Iterator<Item = char>,
{
    source: iter::Peekable<Iter>,
    pos: Pos,
}

impl<Iter> Lexer<Iter>
where
    Iter: Iterator<Item = char>,
{
    fn read(&mut self) -> Option<char> {
        let ch = self.source.next();
        match ch {
            Some('\n') => {
                self.pos.line += 1;
                self.pos.column = 0;
            }
            Some(_) => {
                self.pos.column += 1;
            }
            None => {}
        }
        ch
    }

    fn read_while<Pred>(&mut self, pred: Pred) -> String
    where
        Pred: Fn(char) -> bool,
    {
        let mut s = String::new();
        loop {
            match self.source.peek() {
                Some(&c) if pred(c) => {
                    self.read();
                    s.push(c);
                }
                _ => break s,
            }
        }
    }

    fn error<T>(&self, kind: ErrorKind, start: Pos) -> Option<Result<T>> {
        Some(Err(Error {
            kind,
            span: Span::new(start, self.pos),
        }))
    }
}

impl<Iter> Iterator for Lexer<Iter>
where
    Iter: Iterator<Item = char>,
{
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = *self.source.peek()?;

        // skip whitespace
        if ch.is_whitespace() {
            self.read();
            return self.next();
        }

        // skip comments
        if ch == '#' {
            self.read();
            loop {
                match self.read() {
                    Some('\n') | None => break,
                    _ => {}
                }
            }
            return self.next();
        }

        let start = self.pos;

        let kind = if ch.is_alphabetic() {
            // id
            let val = self.read_while(|c| c.is_alphanumeric());

            match val.as_ref() {
                "def" => TokenKind::Def,
                "struct" => TokenKind::Struct,
                "as" => TokenKind::As,
                "if" => TokenKind::If,
                "then" => TokenKind::Then,
                "elif" => TokenKind::Elif,
                "else" => TokenKind::Else,
                "let" => TokenKind::Let,
                _ => if ch.is_lowercase() {
                    TokenKind::Bind(val)
                } else {
                    TokenKind::Type(val)
                },
            }
        } else if ch.is_numeric() {
            // int
            let val = self.read_while(|c| c.is_numeric());
            match val.parse() {
                Ok(val) => TokenKind::Int(val),
                Err(err) => return self.error(ErrorKind::ParseInt(err), start),
            }
        } else {
            self.read();
            match ch {
                '\'' => TokenKind::Quote,
                '-' => if let Some(&'>') = self.source.peek() {
                    self.read();
                    TokenKind::Arrow
                } else {
                    TokenKind::Operator(Op::Arith(ArithOp::Sub))
                },
                '!' => if let Some(&'=') = self.source.peek() {
                    self.read();
                    TokenKind::Operator(Op::Comp(CompOp::Ne))
                } else {
                    TokenKind::Not
                },
                '+' => TokenKind::Operator(Op::Arith(ArithOp::Add)),
                '*' => TokenKind::Operator(Op::Arith(ArithOp::Mul)),
                '/' => TokenKind::Operator(Op::Arith(ArithOp::Div)),
                '%' => TokenKind::Operator(Op::Arith(ArithOp::Mod)),
                '=' => TokenKind::Operator(Op::Comp(CompOp::Eq)),
                '<' => TokenKind::Operator(Op::Comp(CompOp::Lt)),
                '>' => TokenKind::Operator(Op::Comp(CompOp::Gt)),
                ',' => TokenKind::Comma,
                '.' => TokenKind::Period,
                '(' => TokenKind::LParen,
                ')' => TokenKind::RParen,
                _ => return self.error(ErrorKind::UnexpectedChar(ch), start),
            }
        };

        let end = self.pos;
        let span = Span::new(start, end);

        Some(Ok(Token::new(kind, span)))
    }
}

struct Parser<Iter: Iterator<Item = Token>> {
    source: iter::Peekable<Iter>,
    span: Span,
}

impl<Iter: Iterator<Item = Token>> Parser<Iter> {
    fn structure(&mut self) -> Result<TypeNode> {
        let start = self.start_span();
        let name = expect_value!(self, Type);
        let span = self.end_span(start);

        let fields = self.bind_list()?;

        expect!(self, Period);

        Ok(TypeNode {
            name,
            span,
            kind: TypeNodeKind::Struct { fields },
        })
    }

    fn func(&mut self) -> Result<FuncNode> {
        let start = self.start_span();
        let name = expect_value!(self, Bind);
        let span = self.end_span(start);

        let params = self.bind_list()?;

        let ret = if let Some(&TokenKind::Arrow) = self.peek() {
            self.next();
            RetRef::Named(self.tp()?)
        } else {
            RetRef::Void
        };

        expect!(self, As);

        let body = self.block()?;

        expect!(self, Period);

        Ok(FuncNode {
            name,
            params,
            ret,
            body,
            span,
        })
    }

    fn bind_list(&mut self) -> Result<Vec<BindNode>> {
        let mut binds = Vec::new();
        while let Some(&TokenKind::Bind(_)) = self.peek() {
            binds.push(self.bind()?);
        }
        Ok(binds)
    }

    fn bind(&mut self) -> Result<BindNode> {
        let start = self.start_span();
        let name = expect_value!(self, Bind);
        let tp = self.tp()?;
        let span = self.end_span(start);
        Ok(BindNode { name, tp, span })
    }

    fn block(&mut self) -> Result<ExprNode> {
        let start = self.start_span();

        let stmt = self.statement()?;
        if let Some(&TokenKind::Comma) = self.peek() {
            let mut stmts = vec![stmt];
            while let Some(&TokenKind::Comma) = self.peek() {
                self.next();
                stmts.push(self.statement()?);
            }

            let span = self.end_span(start);
            Ok(ExprNode {
                span,
                kind: ExprNodeKind::Block { stmts },
            })
        } else {
            Ok(stmt)
        }
    }

    fn statement(&mut self) -> Result<ExprNode> {
        let start = self.start_span();

        let expr = self.expr()?;
        if let Some(&TokenKind::Arrow) = self.peek() {
            self.next();
            // TODO: pattern
            expect!(self, Let);
            let local_start = self.start_span();
            let name = expect_value!(self, Bind);
            let local_span = self.end_span(local_start);

            let span = self.end_span(start);
            Ok(ExprNode {
                span,
                kind: ExprNodeKind::Let {
                    value: Box::new(expr),
                    bind: LocalNode {
                        name,
                        span: local_span,
                    },
                },
            })
        } else {
            Ok(expr)
        }
    }

    fn expr(&mut self) -> Result<ExprNode> {
        let start = self.start_span();

        let mut expr = self.term()?;
        while let Some(&TokenKind::Operator(_)) = self.peek() {
            let op = expect_value!(self, Operator);
            let right = self.term()?;

            let span = self.end_span(start);
            expr = ExprNode {
                span,
                kind: ExprNodeKind::Binary {
                    op,
                    left: Box::new(expr),
                    right: Box::new(right),
                },
            };
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<ExprNode> {
        let mut expr = if let Some(&TokenKind::Quote) = self.peek() {
            let start = self.start_span();

            // skip quote
            self.next();

            let name_start = self.start_span();
            let name = self.next();
            let name_span = self.end_span(name_start);

            let kind = match name {
                Some(TokenKind::Bind(name)) => {
                    let args = self.args()?;
                    ExprNodeKind::Function {
                        func: FuncRef {
                            path: Path { name },
                            span: name_span,
                        },
                        args,
                    }
                }
                Some(TokenKind::Type(name)) => {
                    let args = self.args()?;
                    ExprNodeKind::Construct {
                        tp: TypeRef {
                            path: Path { name },
                            span: name_span,
                        },
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
            ExprNode { span, kind }
        } else {
            self.factor()?
        };

        while let Some(&TokenKind::Quote) = self.peek() {
            let start = self.start_span();

            self.next();
            let mem = {
                let start = self.start_span();
                let name = expect_value!(self, Bind);
                let span = self.end_span(start);
                MemberRef { name, span }
            };

            let span = self.end_span(start);
            expr = ExprNode {
                span,
                kind: ExprNodeKind::Member {
                    value: Box::new(expr),
                    mem,
                },
            };
        }

        Ok(expr)
    }

    fn args(&mut self) -> Result<Vec<ExprNode>> {
        let mut args = Vec::new();
        loop {
            // TODO: remove ugly duplicate code
            match self.peek() {
                Some(&TokenKind::Bind(_))
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

    fn factor(&mut self) -> Result<ExprNode> {
        let start = self.start_span();

        let kind = match self.next() {
            Some(TokenKind::Bind(name)) => {
                let span = self.end_span(start);
                ExprNodeKind::Bind {
                    bind: BindRef {
                        path: Path { name },
                        span,
                    },
                }
            }
            Some(TokenKind::Int(value)) => ExprNodeKind::Int { value },
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
        Ok(ExprNode { span, kind })
    }

    fn cond(&mut self) -> Result<ExprNode> {
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
                ExprNode {
                    kind: ExprNodeKind::Noop,
                    span: Span::new(end, end),
                }
            }
        };

        let span = self.end_span(start);
        Ok(ExprNode {
            span,
            kind: ExprNodeKind::If {
                cond: Box::new(cond),
                succ: Box::new(succ),
                fail: Box::new(fail),
            },
        })
    }

    fn tp(&mut self) -> Result<TypeRef> {
        let start = self.start_span();
        let name = expect_value!(self, Type);
        let span = self.end_span(start);

        Ok(TypeRef {
            path: Path { name },
            span,
        })
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
            span: self.span,
            kind,
        })
    }
}

pub type Result<T> = result::Result<T, Error>;

pub type Error = ErrorBase<ErrorKind>;

pub enum ErrorKind {
    ParseInt(num::ParseIntError),
    UnexpectedChar(char),
    Expect {
        expect: &'static str,
        got: Option<TokenKind>,
    },
}

impl Print for ErrorKind {
    fn print<Out>(&self, f: &mut Out, _ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        match *self {
            ErrorKind::ParseInt(ref err) => write!(f, "{}", err),
            ErrorKind::UnexpectedChar(ch) => write!(f, "unexpected character '{}'", ch),
            ErrorKind::Expect {
                expect,
                got: Some(ref got),
            } => write!(f, "expect {}, but got {}", expect, got),
            ErrorKind::Expect { expect, got: None } => {
                write!(f, "expect {}, but reached EOF", expect)
            }
        }
    }
}
