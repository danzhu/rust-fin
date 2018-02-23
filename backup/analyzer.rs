use std::collections::HashMap;

use parser::{Binding, Expression, Function, Module, Type};

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct ModId(usize);

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct LocalId(usize);

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct TypeId {
    module: ModId,
    local: LocalId,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct FuncId {
    module: ModId,
    local: LocalId,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct BindId(usize);

pub struct TypeDef {
    pub name: String,
    pub kind: TypeKind,
}

pub enum TypeKind {
    Primitive,
}

pub struct FuncDef {
    pub name: String,
    pub params: Vec<Bind>,
    pub ret: TypeRef,
    pub locals: Vec<Bind>,
    pub body: Option<Expr>,
}

#[derive(PartialEq, Eq)]
pub enum TypeRef {
    Named { id: TypeId },
    Void,
}

#[derive(PartialEq, Eq)]
pub struct FuncRef {
    pub id: FuncId,
}

pub struct Bind {
    pub name: String,
    pub tp: TypeRef,
}

pub struct Expr {
    pub tp: TypeRef,
    pub kind: ExprKind,
}

pub enum ExprKind {
    Call {
        func: FuncRef,
        args: Vec<Expr>,
    },
    If {
        cond: Box<Expr>,
        succ: Box<Expr>,
        fail: Box<Expr>,
    },
    Id(BindId),
    Int(i32),
    Noop,
}

#[derive(Copy, Clone)]
enum Symbol {
    Mod(ModId),
    Type(TypeId),
    Func(FuncId),
    Bind(BindId),
}

struct SymbolTable<'a> {
    parent: Option<&'a SymbolTable<'a>>,
    symbols: HashMap<String, Symbol>,
}

pub struct ModDef {
    types: Vec<TypeDef>,
    funcs: Vec<FuncDef>,
    syms: SymbolTable<'static>,
}

pub struct DefTable {
    mods: Vec<ModDef>,
}

pub enum AnalyzerError {
    ExpectType(&'static str, Symbol),
    IdNotFound(String),
    TypeMismatch,
}

type AnalyzerResult<T> = Result<T, AnalyzerError>;

macro_rules! expect_sym {
    ($table:expr, $name:expr, $pat:pat, $res:expr) => (
        match $table.get($name) {
            Some(&$pat) => $res,
            Some(&sym) => return Err(AnalyzerError::ExpectType(stringify!($pat), sym)),
            None => return Err(AnalyzerError::IdNotFound($name.clone())),
        }
    )
}

impl<'a> SymbolTable<'a> {
    fn new<'b>() -> SymbolTable<'b> {
        SymbolTable {
            parent: None,
            symbols: HashMap::new(),
        }
    }

    fn get(&self, name: &String) -> Option<&Symbol> {
        if let Some(it) = self.symbols.get(name) {
            Some(it)
        } else if let Some(par) = self.parent {
            par.get(name)
        } else {
            None
        }
    }

    fn get_type(&self, name: &String) -> AnalyzerResult<TypeId> {
        Ok(expect_sym!(self, name, Symbol::Type(id), id))
    }

    fn get_func(&self, name: &String) -> AnalyzerResult<FuncId> {
        Ok(expect_sym!(self, name, Symbol::Func(id), id))
    }
}

impl ModDef {
    fn new() -> ModDef {
        ModDef {
            types: Vec::new(),
            funcs: Vec::new(),
            syms: SymbolTable::new(),
        }
    }

    fn get_func(&self, id: LocalId) -> &FuncDef {
        &self.funcs[id.0]
    }

    fn get_func_mut(&mut self, id: LocalId) -> &mut FuncDef {
        &mut self.funcs[id.0]
    }
}

impl DefTable {
    pub fn new() -> DefTable {
        let builtin = ModDef::new();
        DefTable {
            mods: vec![builtin],
        }
    }

    pub fn get_mod(&self, id: ModId) -> &ModDef {
        &self.mods[id.0]
    }

    pub fn get_mod_mut(&self, id: ModId) -> &mut ModDef {
        &mut self.mods[id.0]
    }

    pub fn get_func(&self, id: FuncId) -> &FuncDef {
        self.get_mod(id.module).get_func(id.local)
    }

    pub fn get_func_mut(&self, id: FuncId) -> &mut FuncDef {
        self.get_mod_mut(id.module).get_func_mut(id.local)
    }

    pub fn decl_mod(&mut self, module: &Module) -> AnalyzerResult<ModId> {
        let id = ModId(self.mods.len());
        let md = ModDef::new();
        for func in module.functions {
            self.decl_func(&func)?;
        }
        self.mods.push(md);
        Ok(id)
    }

    pub fn decl_func(&mut self, mod_id: ModId, func: &Function) -> AnalyzerResult<FuncId> {
        let md = self.get_mod_mut(mod_id)?;
        let id = FuncId {
            module: mod_id,
            local: LocalId(md.funcs.len()),
        };
        md.funcs.push(FuncDef {
            name: func.name.clone(),
            params: func.params
                .iter()
                .map(|param| lower_bind(param, syms))
                .collect::<Result<Vec<_>, _>>()?,
            ret: lower_type(&func.ret, syms)?,
            locals: Vec::new(),
            body: None,
        });
        Ok(id)
    }

    pub fn def_func(&mut self, id: FuncId, func: &Function) -> AnalyzerResult<()> {
        let syms = SymbolTable::new();
        let binds = Vec::new();
        let body = lower_expr(func.body, syms, self, binds)?;

        self.get_func_mut(id)?.body = Some(body);

        Ok(())
    }
}

fn lower_type<'a>(tp: &Type, syms: &SymbolTable<'a>) -> AnalyzerResult<TypeRef> {
    Ok(match tp {
        &Type::Named { ref name } => TypeRef::Named {
            id: syms.get_type(name)?,
        },
        &Type::Void => TypeRef::Void,
    })
}

fn lower_bind<'a>(bind: &Binding, syms: &SymbolTable<'a>) -> AnalyzerResult<Bind> {
    Ok(Bind {
        name: bind.name.clone(),
        tp: lower_type(&bind.tp, syms)?,
    })
}

fn lower_expr<'a>(
    expr: Expression,
    syms: &SymbolTable<'a>,
    defs: &DefTable,
) -> AnalyzerResult<Expr> {
    Ok(match expr {
        Expression::Function { name, args } => {
            let id = syms.get_func(&name)?;
            Expr {
                tp: match defs.get_func(id) {
                    Some(func) => func.ret,
                    None => return Err(AnalyzerError::IdNotFound(name)),
                },
                kind: ExprKind::Call {
                    func: FuncRef { id },
                    args: args.into_iter()
                        .map(|expr| lower_expr(expr, syms, defs))
                        .collect::<Result<Vec<_>, _>>()?,
                },
            }
        }
        Expression::If { cond, succ, fail } => {
            let cond = lower_expr(*cond, syms, defs)?;
            let succ = lower_expr(*succ, syms, defs)?;
            let fail = lower_expr(*fail, syms, defs)?;

            if succ.tp != fail.tp {
                return Err(AnalyzerError::TypeMismatch);
            }

            Expr {
                tp: succ.tp,
                kind: ExprKind::If {
                    cond: Box::new(cond),
                    succ: Box::new(succ),
                    fail: Box::new(fail),
                },
            }
        }
        Expression::Int(val) => Expr {
            tp: TypeRef::Named {
                id: syms.get_type(&"Int".to_string())?,
            },
            kind: ExprKind::Int(val),
        },
        Expression::Noop => Expr {
            tp: TypeRef::Named {
                id: syms.get_type(&"Void".to_string())?,
            },
            kind: ExprKind::Noop,
        },
    })
}
