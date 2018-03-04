use std::collections::HashMap;
use std::fmt;

use ast::*;

pub struct Store {
    pub type_defs: List<TypeDef>,
    pub func_defs: List<FuncDef>,
    pub sym_table: HashMap<String, Symbol>,

    pub def_int: Index,
    pub type_int: Type,
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

impl Store {
    pub fn new() -> Self {
        let mut store = Store {
            type_defs: List::new(),
            func_defs: List::new(),
            sym_table: HashMap::new(),

            def_int: Index::UNKNOWN,
            type_int: Type::new(TypeKind::Unknown),
        };

        define_tp!(store, def_int, type_int, Int);

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
