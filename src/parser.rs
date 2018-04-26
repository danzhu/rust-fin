use std::{io, iter, num, result};

use common::*;
use ctx::*;
use error::*;
use ptree::*;
use token::*;

macro_rules! expect {
    ($self:expr, $pat:ident) => {
        match $self.next() {
            Some(TokenKind::$pat) => {}
            got => {
                return $self.error(ErrorKind::Expect {
                    expect: stringify!($pat),
                    got,
                })
            }
        }
    };
}

macro_rules! expect_value {
    ($self:expr, $pat:ident) => {
        match $self.next() {
            Some(TokenKind::$pat(val)) => val,
            got => {
                return $self.error(ErrorKind::Expect {
                    expect: stringify!($pat),
                    got,
                })
            }
        }
    };
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

    ctx.sources[idx].items = par.parse()?;
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
                "extern" => TokenKind::Extern,
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
            match (ch, self.source.peek().cloned()) {
                ('-', Some('>')) => {
                    self.read();
                    TokenKind::Arrow
                }
                ('=', Some('=')) => {
                    self.read();
                    TokenKind::Binary(BinaryOp::Comp(CompOp::Eq))
                }
                ('!', Some('=')) => {
                    self.read();
                    TokenKind::Binary(BinaryOp::Comp(CompOp::Ne))
                }
                ('+', Some(' ')) => TokenKind::Binary(BinaryOp::Arith(ArithOp::Add)),
                ('-', Some(' ')) => TokenKind::Binary(BinaryOp::Arith(ArithOp::Sub)),
                ('*', Some(' ')) => TokenKind::Binary(BinaryOp::Arith(ArithOp::Mul)),
                ('/', Some(' ')) => TokenKind::Binary(BinaryOp::Arith(ArithOp::Div)),
                ('%', Some(' ')) => TokenKind::Binary(BinaryOp::Arith(ArithOp::Mod)),
                ('<', Some(' ')) => TokenKind::Binary(BinaryOp::Comp(CompOp::Lt)),
                ('>', Some(' ')) => TokenKind::Binary(BinaryOp::Comp(CompOp::Gt)),
                ('-', _) => TokenKind::Unary(UnaryOp::Neg),
                ('!', _) => TokenKind::Unary(UnaryOp::Not),
                ('=', _) => TokenKind::Equal,
                ('*', _) => TokenKind::Star,
                (',', _) => TokenKind::Comma,
                ('.', _) => TokenKind::Period,
                ('(', _) => TokenKind::LParen,
                (')', _) => TokenKind::RParen,
                ('\'', _) => TokenKind::Quote,
                _ => return self.error(ErrorKind::UnexpectedChar(ch), start),
            }
        };

        let end = self.pos;
        let span = Span::new(start, end);

        Some(Ok(Token::new(kind, span)))
    }
}

struct Parser<Iter>
where
    Iter: Iterator<Item = Token>,
{
    source: iter::Peekable<Iter>,
    span: Span,
}

impl<Iter> Parser<Iter>
where
    Iter: Iterator<Item = Token>,
{
    fn parse(&mut self) -> Result<Vec<ItemNode>> {
        let mut items = Vec::new();

        while self.peek().is_some() {
            let item = match self.next() {
                Some(TokenKind::Struct) => {
                    let def = self.structure()?;
                    ItemNode::Type(def)
                }
                Some(TokenKind::Def) => {
                    let def = self.func()?;
                    ItemNode::Func(def)
                }
                Some(TokenKind::Extern) => {
                    let decl = self.sig()?;
                    ItemNode::Extern(decl)
                }
                got => {
                    return self.error(ErrorKind::Expect {
                        expect: "top-level definition",
                        got,
                    })
                }
            };
            items.push(item);
        }

        Ok(items)
    }

    fn structure(&mut self) -> Result<TypeNode> {
        let start = self.start_span();
        let name = expect_value!(self, Type);
        let span = self.end_span(start);

        expect!(self, As);

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
        let sig = self.sig()?;
        let span = self.end_span(start);

        expect!(self, As);

        let body = self.block()?;

        expect!(self, Period);

        Ok(FuncNode { sig, body, span })
    }

    fn sig(&mut self) -> Result<SigNode> {
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

        Ok(SigNode {
            name,
            params,
            ret,
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
        match self.peek() {
            Some(&TokenKind::Arrow) => {
                self.next();
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
            }
            Some(&TokenKind::Equal) => {
                self.next();
                let var = self.expr()?;

                let span = self.end_span(start);
                Ok(ExprNode {
                    span,
                    kind: ExprNodeKind::Assign {
                        value: Box::new(expr),
                        var: Box::new(var),
                    },
                })
            }
            _ => Ok(expr),
        }
    }

    fn expr(&mut self) -> Result<ExprNode> {
        let start = self.start_span();

        let mut expr = self.term()?;
        while let Some(&TokenKind::Binary(_)) = self.peek() {
            let op = expect_value!(self, Binary);
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
                Some(&TokenKind::Unary(_))
                | Some(&TokenKind::Bind(_))
                | Some(&TokenKind::Int(_))
                | Some(&TokenKind::Let)
                | Some(&TokenKind::Star)
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
            Some(TokenKind::Unary(op)) => {
                let expr = self.factor()?;
                ExprNodeKind::Unary {
                    op,
                    value: Box::new(expr),
                }
            }
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
            Some(TokenKind::Let) => {
                let tp = self.tp()?;
                ExprNodeKind::Var { tp }
            }
            Some(TokenKind::Star) => {
                let expr = self.factor()?;
                ExprNodeKind::Deref {
                    value: Box::new(expr),
                }
            }
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
