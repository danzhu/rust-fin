use std::{io, result};
use std::collections::HashMap;

use common::*;
use error::*;
use ptree::*;
use ast::*;
use ctx::*;

type RefTable = Context;

macro_rules! declare_sym {
    ($ctx: expr, $def: expr, $defs: ident, $kind: ident) => {
        let def = $def;
        if $ctx.sym_table.contains_key(&def.path.name) {
            return Err(Error {
                kind: ErrorKind::DuplicateSymbol {
                    kind: stringify!($kind),
                    path: def.path.clone(),
                },
                span: def.span,
            });
        }
        let name = def.path.name.clone();
        let idx = $ctx.$defs.push(def);
        $ctx.sym_table.insert(name, Symbol::$kind(idx));
    };
}

macro_rules! resolve_path {
    ($ctx: expr, $rf: expr, $kind: ident) => {{
        let ctx: &Context = $ctx;
        let rf = $rf;
        match ctx.get_sym(&rf.path.name) {
            Some(Symbol::$kind(idx)) => idx,
            Some(got) => {
                return Err(Error {
                    kind: ErrorKind::WrongSymbolKind {
                        expect: stringify!($kind),
                        got,
                    },
                    span: rf.span,
                })
            }
            None => {
                return Err(Error {
                    kind: ErrorKind::SymbolNotFound {
                        kind: stringify!($kind),
                        path: rf.path.clone(),
                    },
                    span: rf.span,
                })
            }
        }
    }};
}

pub fn gen(ctx: &mut Context) -> Result<()> {
    declare_types(ctx)?;
    define_types(ctx)?;
    declare_funcs(ctx)?;
    define_funcs(ctx)
}

fn declare_types(ctx: &mut Context) -> Result<()> {
    for item in ctx.sources.iter().flat_map(|src| &src.items) {
        let tp = match *item {
            ItemNode::Type(ref tp) => tp,
            _ => continue,
        };

        let path = Path {
            name: tp.name.clone(),
        };
        let def = TypeDef {
            path,
            span: tp.span,
            kind: TypeDefKind::Opaque,
        };
        declare_sym!(ctx, def, type_defs, Type);
    }
    Ok(())
}

fn define_types(ctx: &mut Context) -> Result<()> {
    for item in &mut ctx.sources.iter().flat_map(|src| &src.items) {
        let tp = match *item {
            ItemNode::Type(ref tp) => tp,
            _ => continue,
        };

        let kind = match tp.kind {
            TypeNodeKind::Struct { ref fields } => {
                let fields = convert_binds(fields, ctx)?;

                let mut sym_table = HashMap::new();
                for (i, field) in fields.iter().enumerate() {
                    sym_table.insert(field.name.clone(), Index::new(i));
                }

                TypeDefKind::Struct { fields, sym_table }
            }
        };

        let mut def = match ctx.sym_table[&tp.name] {
            Symbol::Type(idx) => &mut ctx.type_defs[idx],
            _ => panic!("symbol no longer a type"),
        };

        def.kind = kind;
    }
    Ok(())
}

fn declare_funcs(ctx: &mut Context) -> Result<()> {
    for item in ctx.sources.iter().flat_map(|src| &src.items) {
        let sig = match *item {
            ItemNode::Func(ref func) => &func.sig,
            ItemNode::Extern(ref sig) => sig,
            _ => continue,
        };

        let path = Path {
            name: sig.name.clone(),
        };
        let params = convert_binds(&sig.params, ctx)?;
        let ret = match sig.ret {
            RetRef::Named(ref tp) => resolve_type(tp, ctx)?,
            RetRef::Void => ctx.type_void.clone(),
        };
        let def = FuncDef {
            path,
            params,
            ret,
            span: sig.span,
            body: None,
            ir: None,
        };
        declare_sym!(ctx, def, func_defs, Func);
    }

    Ok(())
}

fn define_funcs(ctx: &mut Context) -> Result<()> {
    for item in ctx.sources.iter().flat_map(|src| &src.items) {
        let func = match *item {
            ItemNode::Func(ref func) => func,
            _ => continue,
        };

        let body = {
            let def = match ctx.sym_table[&func.sig.name] {
                Symbol::Func(idx) => &ctx.func_defs[idx],
                _ => panic!("symbol no longer a func"),
            };

            let mut res = Resolver {
                ctx,
                refs: ctx,
                func: def,
                locals: List::new(),
            };
            res.resolve(func)?
        };

        // TODO: find a way to avoid this second lookup
        let mut def = match ctx.sym_table[&func.sig.name] {
            Symbol::Func(idx) => &mut ctx.func_defs[idx],
            _ => panic!("symbol no longer a func"),
        };

        let idx = ctx.bodies.push(body);
        def.body = Some(idx);
    }

    Ok(())
}

fn convert_binds(binds: &[BindNode], refs: &RefTable) -> Result<List<BindDef>> {
    let mut defs = List::new();
    for bind in binds {
        defs.push(BindDef {
            name: bind.name.clone(),
            tp: resolve_type(&bind.tp, refs)?,
            span: bind.span,
        });
    }
    Ok(defs)
}

fn resolve_type(tp: &TypeRef, refs: &RefTable) -> Result<Type> {
    Ok(Type {
        kind: TypeKind::Named {
            index: resolve_path!(refs, tp, Type),
        },
    })
}

fn resolve_func(func: &FuncRef, refs: &RefTable) -> Result<Func> {
    Ok(Func {
        index: resolve_path!(refs, func, Func),
    })
}

struct Resolver<'a> {
    ctx: &'a Context,
    refs: &'a RefTable,
    func: &'a FuncDef,
    locals: List<BindDef>,
}

impl<'a> Resolver<'a> {
    fn resolve(mut self, func: &FuncNode) -> Result<Body> {
        let mut syms = SymTable::root(self.refs);
        for param in &func.sig.params {
            let def = BindDef {
                name: param.name.clone(),
                tp: resolve_type(&param.tp, self.refs)?,
                span: param.span,
            };
            let index = self.locals.push(def);
            syms.add(
                param.name.clone(),
                Bind {
                    kind: BindKind::Local { index },
                },
            );
        }

        let expr = self.resolve_expr(&func.body, &mut syms)?;

        expect_tp(&self.func.ret, &expr.tp, expr.span)?;

        Ok(Body {
            expr,
            locals: self.locals,
        })
    }

    fn resolve_expr(&mut self, expr: &ExprNode, syms: &mut SymTable) -> Result<Expr> {
        let span = expr.span;
        let expr = match expr.kind {
            ExprNodeKind::Block { ref stmts } => {
                let stmts = stmts
                    .iter()
                    .map(|stmt| self.resolve_expr(stmt, syms))
                    .collect::<Result<Vec<_>>>()?;

                let tp = match stmts.last() {
                    Some(stmt) => stmt.tp.clone(),
                    None => self.ctx.type_void.clone(),
                };
                let kind = ExprKind::Block { stmts };
                Expr { tp, span, kind }
            }
            ExprNodeKind::Let {
                ref value,
                ref bind,
            } => {
                let value = self.resolve_expr(value, syms)?;

                let name = bind.name.clone();
                let tp = value.tp.clone();
                let def = BindDef {
                    name: name.clone(),
                    tp: tp.clone(),
                    span: bind.span,
                };
                let index = self.locals.push(def);
                let bind = Bind {
                    kind: BindKind::Local { index },
                };
                syms.add(name.clone(), bind.clone());

                let tp = self.ctx.type_void.clone();
                let kind = ExprKind::Let {
                    value: Box::new(value),
                    bind,
                };
                Expr { tp, span, kind }
            }
            ExprNodeKind::Var { ref tp } => {
                let tp = resolve_type(tp, self.refs)?;

                let kind = ExprKind::Var { tp: tp.clone() };
                let tp = Type {
                    kind: TypeKind::Ref { tp: Box::new(tp) },
                };
                Expr { tp, span, kind }
            }
            ExprNodeKind::Deref { ref value } => {
                let value = self.resolve_expr(value, syms)?;

                let tp = if let TypeKind::Ref { ref tp } = value.tp.kind {
                    (**tp).clone()
                } else {
                    return Err(Error {
                        kind: ErrorKind::DerefNonRef {
                            tp: value.tp.clone(),
                        },
                        span: expr.span,
                    });
                };
                let kind = ExprKind::Deref {
                    value: Box::new(value),
                };
                Expr { tp, span, kind }
            }
            ExprNodeKind::Assign { ref value, ref var } => {
                let value = self.resolve_expr(value, syms)?;
                let var = self.resolve_expr(var, syms)?;

                let tp = self.ctx.type_void.clone();
                let kind = ExprKind::Assign {
                    value: Box::new(value),
                    var: Box::new(var),
                };
                Expr { tp, span, kind }
            }
            ExprNodeKind::Construct { ref tp, ref args } => {
                let tp = resolve_type(tp, self.refs)?;
                let args = args.iter()
                    .map(|arg| self.resolve_expr(arg, syms))
                    .collect::<Result<Vec<_>>>()?;

                let def = self.ctx.get_type(&tp);
                match def.kind {
                    TypeDefKind::Struct { ref fields, .. } => {
                        if fields.len() != args.len() {
                            return Err(Error {
                                kind: ErrorKind::ArgCount {
                                    expect: fields.len(),
                                    got: args.len(),
                                },
                                span: expr.span,
                            });
                        }

                        for (field, arg) in fields.iter().zip(&args) {
                            expect_tp(&field.tp, &arg.tp, arg.span)?;
                        }

                        let kind = ExprKind::Construct {
                            tp: tp.clone(),
                            args,
                        };
                        Expr { tp, span, kind }
                    }
                    TypeDefKind::Builtin(_) => {
                        return Err(Error {
                            kind: ErrorKind::ConstructPrimitive { tp: tp.clone() },
                            span: expr.span,
                        });
                    }
                    TypeDefKind::Opaque => panic!("constructing opaque type"),
                }
            }
            ExprNodeKind::Function { ref func, ref args } => {
                let func = resolve_func(func, self.refs)?;
                let args = args.iter()
                    .map(|arg| self.resolve_expr(arg, syms))
                    .collect::<Result<Vec<_>>>()?;

                let def = self.ctx.get_func(&func);

                if def.params.len() != args.len() {
                    return Err(Error {
                        kind: ErrorKind::ArgCount {
                            expect: def.params.len(),
                            got: args.len(),
                        },
                        span: expr.span,
                    });
                }
                for (param, arg) in def.params.iter().zip(&args) {
                    expect_tp(&param.tp, &arg.tp, arg.span)?;
                }

                let tp = def.ret.clone();
                let kind = ExprKind::Function { func, args };
                Expr { tp, span, kind }
            }
            ExprNodeKind::Member { ref value, ref mem } => {
                let value = self.resolve_expr(value, syms)?;
                let def = self.ctx.get_type(&value.tp);

                match def.kind {
                    TypeDefKind::Struct {
                        ref fields,
                        ref sym_table,
                    } => {
                        let idx = match sym_table.get(&mem.name) {
                            Some(&idx) => idx,
                            None => {
                                return Err(Error {
                                    kind: ErrorKind::MemberNotFound {
                                        tp: value.tp.clone(),
                                        mem: mem.clone(),
                                    },
                                    span: expr.span,
                                })
                            }
                        };
                        let mem = Member {
                            tp: value.tp.clone(),
                            index: idx,
                        };
                        let tp = fields[idx].tp.clone();
                        let kind = ExprKind::Member {
                            value: Box::new(value),
                            mem,
                        };
                        Expr { tp, span, kind }
                    }
                    TypeDefKind::Builtin(_) => {
                        return Err(Error {
                            kind: ErrorKind::MemberNotFound {
                                tp: value.tp.clone(),
                                mem: mem.clone(),
                            },
                            span: expr.span,
                        });
                    }
                    TypeDefKind::Opaque => panic!("member of opaque type"),
                }
            }
            ExprNodeKind::Unary { op, ref value } => {
                let value = self.resolve_expr(value, syms)?;

                let tp = match op {
                    UnaryOp::Neg => {
                        // TODO: float
                        expect_tp(&self.ctx.type_int, &value.tp, expr.span)?;
                        self.ctx.type_int.clone()
                    }
                    UnaryOp::Not => {
                        expect_tp(&self.ctx.type_bool, &value.tp, expr.span)?;
                        self.ctx.type_bool.clone()
                    }
                };
                let kind = ExprKind::Unary {
                    op,
                    value: Box::new(value),
                };
                Expr { tp, span, kind }
            }
            ExprNodeKind::Binary {
                op,
                ref left,
                ref right,
            } => {
                let left = self.resolve_expr(left, syms)?;
                let right = self.resolve_expr(right, syms)?;

                expect_tp(&left.tp, &right.tp, expr.span)?;

                let tp = match op {
                    BinaryOp::Arith(_) => left.tp.clone(),
                    BinaryOp::Comp(_) => self.ctx.type_bool.clone(),
                };
                let kind = ExprKind::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                };
                Expr { tp, span, kind }
            }
            ExprNodeKind::If {
                ref cond,
                ref succ,
                ref fail,
            } => {
                let cond = self.resolve_expr(cond, syms)?;

                let mut succ_syms = SymTable::new(syms);
                let succ = self.resolve_expr(succ, &mut succ_syms)?;

                let mut fail_syms = SymTable::new(syms);
                let fail = self.resolve_expr(fail, &mut fail_syms)?;

                expect_tp(&self.ctx.type_bool, &cond.tp, expr.span)?;
                expect_tp(&succ.tp, &fail.tp, expr.span)?;

                let tp = succ.tp.clone();
                let kind = ExprKind::If {
                    cond: Box::new(cond),
                    succ: Box::new(succ),
                    fail: Box::new(fail),
                };
                Expr { tp, span, kind }
            }
            ExprNodeKind::Bind { ref bind } => {
                let bind = match syms.get(&bind.path.name) {
                    Some(bind) => bind,
                    None => {
                        return Err(Error {
                            kind: ErrorKind::SymbolNotFound {
                                kind: "Bind",
                                path: bind.path.clone(),
                            },
                            span: expr.span,
                        })
                    }
                };
                let tp = match bind.kind {
                    BindKind::Local { index } => self.locals[index].tp.clone(),
                };
                let kind = ExprKind::Bind { bind };
                Expr { tp, span, kind }
            }
            ExprNodeKind::Int { value } => {
                let tp = self.ctx.type_int.clone();
                let kind = ExprKind::Int { value };
                Expr { tp, span, kind }
            }
            ExprNodeKind::Noop => {
                let tp = self.ctx.type_void.clone();
                let kind = ExprKind::Noop;
                Expr { tp, span, kind }
            }
        };
        Ok(expr)
    }
}

struct SymTable<'a> {
    symbols: HashMap<String, Bind>,
    parent: SymTableParent<'a>,
}

impl<'a> SymTable<'a> {
    fn root(ctx: &'a Context) -> Self {
        SymTable {
            symbols: HashMap::new(),
            parent: SymTableParent::Context(ctx),
        }
    }

    fn new(parent: &'a SymTable) -> Self {
        SymTable {
            symbols: HashMap::new(),
            parent: SymTableParent::Table(parent),
        }
    }

    fn add(&mut self, name: String, bind: Bind) {
        self.symbols.insert(name, bind);
    }

    fn get(&self, name: &str) -> Option<Bind> {
        match (self.symbols.get(name), &self.parent) {
            (Some(bind), _) => Some(bind.clone()),
            (None, &SymTableParent::Table(parent)) => parent.get(name),
            (None, &SymTableParent::Context(_)) => None,
        }
    }
}

enum SymTableParent<'a> {
    Table(&'a SymTable<'a>),
    Context(&'a Context),
}

fn expect_tp(expect: &Type, got: &Type, span: Span) -> Result<()> {
    if expect == got {
        Ok(())
    } else {
        Err(Error {
            kind: ErrorKind::TypeMismatch {
                expect: expect.clone(),
                got: got.clone(),
            },
            span,
        })
    }
}

pub type Result<T> = result::Result<T, Error>;

pub type Error = ErrorBase<ErrorKind>;

pub enum ErrorKind {
    DuplicateSymbol { kind: &'static str, path: Path },
    SymbolNotFound { kind: &'static str, path: Path },
    WrongSymbolKind { expect: &'static str, got: Symbol },
    ArgCount { expect: usize, got: usize },
    TypeMismatch { expect: Type, got: Type },
    ConstructPrimitive { tp: Type },
    MemberNotFound { tp: Type, mem: MemberRef },
    DerefNonRef { tp: Type },
}

impl Print for ErrorKind {
    fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        match *self {
            ErrorKind::DuplicateSymbol { kind, ref path } => {
                write!(f, "duplicate {} symbol {}", kind, path)
            }
            ErrorKind::SymbolNotFound { kind, ref path } => {
                write!(f, "{} symbol {} not found", kind, path)
            }
            ErrorKind::WrongSymbolKind { expect, got } => {
                write!(f, "expect {} symbol, got {}", expect, got.format(ctx))
            }
            ErrorKind::ArgCount { expect, got } => {
                write!(f, "expect {} arguments, got {}", expect, got)
            }
            ErrorKind::TypeMismatch {
                ref expect,
                ref got,
            } => write!(
                f,
                "expect type {}, got {}",
                expect.format(ctx),
                got.format(ctx)
            ),
            ErrorKind::ConstructPrimitive { ref tp } => {
                write!(f, "attempt to construct primitive type {}", tp.format(ctx))
            }
            ErrorKind::MemberNotFound { ref tp, ref mem } => write!(
                f,
                "type {} doesn't have member {}",
                tp.format(ctx),
                mem.name,
            ),
            ErrorKind::DerefNonRef { ref tp } => {
                write!(f, "dereferencing a non-reference type {}", tp.format(ctx))
            }
        }
    }
}
