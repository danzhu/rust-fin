use std::collections::HashMap;
use std::fmt;

use common::*;
use ast::*;
use ir::*;

#[derive(Clone)]
pub struct Source {
    pub defs: Vec<Def>,
}

#[derive(Clone)]
pub struct Def {
    pub kind: DefKind,
}

#[derive(Clone)]
pub enum DefKind {
    Func(FuncDef),
}

pub struct Store {
    pub type_defs: List<TypeDef>,
    pub func_defs: List<FuncDef>,
    pub sym_table: HashMap<String, Symbol>,

    pub def_int: Index,
    pub type_int: Type,
    pub def_bool: Index,
    pub type_bool: Type,
}

#[derive(Clone)]
pub struct TypeDef {
    pub name: String,
    pub kind: TypeDefKind,
}

#[derive(Clone)]
pub enum TypeDefKind {
    Int,
    Bool,
}

#[derive(Clone)]
pub struct FuncDef {
    pub name: String,
    pub params: Vec<BindDef>,
    pub ret: Type,
    pub body: Expr,
    pub locals: List<BindDef>,
    pub ir: Ir,
}

#[derive(Clone)]
pub struct BindDef {
    pub name: String,
    pub tp: Type,
    pub reg: Reg,
}

#[derive(Copy, Clone)]
pub enum Symbol {
    Type(Index),
    Func(Index),
}

macro_rules! define_tp {
    ($store:expr, $def:ident, $type:ident, $name:ident) => {{
        let idx = $store.define_type(TypeDef::new(stringify!($name), TypeDefKind::$name));
        let mut path = Path::new(stringify!($name));
        path.index = idx;
        $store.$def = idx;
        $store.$type = Type::new(TypeKind::Named { path });
    }}
}

impl Source {
    pub fn new() -> Self {
        Self { defs: Vec::new() }
    }
}

impl fmt::Debug for Source {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for def in &self.defs {
            write!(f, "{:?}", def)?;
        }
        Ok(())
    }
}

impl Def {
    pub fn new(kind: DefKind) -> Self {
        Self { kind }
    }
}

impl fmt::Debug for Def {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            DefKind::Func(ref func) => writeln!(f, "{:?}", func),
        }
    }
}

impl Store {
    pub fn new() -> Self {
        let mut store = Store {
            type_defs: List::new(),
            func_defs: List::new(),
            sym_table: HashMap::new(),

            def_int: Index::UNKNOWN,
            type_int: Type::new(TypeKind::Unknown),
            def_bool: Index::UNKNOWN,
            type_bool: Type::new(TypeKind::Unknown),
        };

        define_tp!(store, def_int, type_int, Int);
        define_tp!(store, def_bool, type_bool, Bool);

        store
    }

    pub fn get_sym(&self, path: &Path) -> Option<Symbol> {
        self.sym_table.get(&path.name).cloned()
    }

    pub fn define(&mut self, src: Source) {
        for def in src.defs {
            match def.kind {
                DefKind::Func(func) => {
                    self.define_func(func);
                }
            }
        }
    }

    fn define_type(&mut self, tp: TypeDef) -> Index {
        let name = tp.name.clone();
        let idx = self.type_defs.push(tp);
        self.sym_table.insert(name, Symbol::Type(idx));
        idx
    }

    fn define_func(&mut self, func: FuncDef) -> Index {
        let name = func.name.clone();
        let idx = self.func_defs.push(func);
        self.sym_table.insert(name, Symbol::Func(idx));
        idx
    }
}

impl fmt::Debug for Store {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for tp in &self.type_defs {
            writeln!(f, "{:?}", tp)?;
        }
        for func in &self.func_defs {
            writeln!(f, "{:?}", func)?;
        }
        Ok(())
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Symbol::Func(idx) => write!(f, "function {:?}", idx),
            Symbol::Type(idx) => write!(f, "type {:?}", idx),
        }
    }
}

impl TypeDef {
    pub fn new<Str>(name: Str, kind: TypeDefKind) -> Self
    where
        Str: Into<String>,
    {
        Self {
            name: name.into(),
            kind,
        }
    }
}

impl fmt::Debug for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Type ")?;
        match self.kind {
            TypeDefKind::Int => writeln!(f, "Int"),
            TypeDefKind::Bool => writeln!(f, "Bool"),
        }
    }
}

impl FuncDef {
    pub fn new<Str>(name: Str, params: Vec<BindDef>, ret: Type, body: Expr) -> Self
    where
        Str: Into<String>,
    {
        Self {
            name: name.into(),
            params,
            ret,
            body,
            locals: List::new(),
            ir: Ir::new(),
        }
    }
}

impl fmt::Debug for FuncDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Function {}", self.name)?;
        writeln!(f, "Params")?;
        for param in &self.params {
            writeln!(f, "{}{:?}", INDENT, param)?;
        }
        writeln!(f, "Ret {:?}", self.ret)?;
        writeln!(f, "Locals")?;
        for local in &self.locals {
            writeln!(f, "{}{:?}", INDENT, local)?;
        }
        write!(f, "{:?}", self.body)?;
        write!(f, "{:?}", self.ir)
    }
}

impl BindDef {
    pub fn new(name: String) -> Self {
        Self {
            name,
            tp: Type::new(TypeKind::Unknown),
            reg: Reg::NONE,
        }
    }
}

impl fmt::Debug for BindDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {:?}", self.name, self.tp)
    }
}
