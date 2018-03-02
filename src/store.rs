use std::collections::HashMap;
use std::fmt;

use ast::*;

pub struct Store {
    pub type_defs: Vec<TypeDef>,
    pub func_defs: Vec<FuncDef>,
    pub sym_table: HashMap<String, Symbol>,

    pub type_int: Index,
}

#[derive(Copy, Clone)]
pub enum Symbol {
    Type(Index),
    Func(Index),
}

macro_rules! define_tp {
    ($store:expr, $name:ident) => {
        $store.define_type(TypeDef {
            name: stringify!($name).to_string(),
            kind: TypeDefKind::$name,
        })
    }
}

impl Store {
    pub fn new() -> Self {
        let mut store = Store {
            type_defs: Vec::new(),
            func_defs: Vec::new(),
            sym_table: HashMap::new(),

            type_int: Index::INVALID,
        };

        store.type_int = define_tp!(store, Int);

        store
    }

    pub fn get_sym(&self, path: &Path) -> Option<Symbol> {
        self.sym_table.get(&path.name).cloned()
    }

    pub fn define(&mut self, src: Source) {
        for def in src.defs {
            match def.kind {
                DefKind::Type(tp) => { self.define_type(tp); },
                DefKind::Func(func) => { self.define_func(func); },
            }
        }
    }

    fn define_type(&mut self, tp: TypeDef) -> Index {
        let idx = Index::new(self.type_defs.len());
        self.sym_table.insert(tp.name.clone(), Symbol::Type(idx));
        self.type_defs.push(tp);
        idx
    }

    fn define_func(&mut self, func: FuncDef) -> Index {
        let idx = Index::new(self.func_defs.len());
        self.sym_table.insert(func.name.clone(), Symbol::Func(idx));
        self.func_defs.push(func);
        idx
    }
}

impl fmt::Debug for Store {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for tp in &self.type_defs {
            writeln!(f, "Type {:?}", tp)?;
        }
        for func in &self.func_defs {
            writeln!(f, "Function {:?}", func)?;
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
