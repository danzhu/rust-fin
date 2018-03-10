use std::fmt;
use std::collections::HashMap;

use common::*;
use def::*;

pub struct Context {
    pub type_defs: List<TypeDef>,
    pub func_defs: List<FuncDef>,
    pub sym_table: HashMap<String, Symbol>,

    pub type_int: Type,
    pub type_bool: Type,
}

macro_rules! define_tp {
    ($store:expr, $type:ident, $name:ident) => {{
        let kind = TypeDefKind::Builtin(BuiltinType::$name);
        let tp = TypeDef::new(stringify!($name), kind);
        let idx = $store.define_type(tp);
        let path = Path::Resolved(idx);
        $store.$type = Type::new(TypeKind::Named { path });
    }}
}

impl Context {
    pub fn new() -> Self {
        let mut store = Context {
            type_defs: List::new(),
            func_defs: List::new(),
            sym_table: HashMap::new(),

            type_int: Type::new(TypeKind::Unknown),
            type_bool: Type::new(TypeKind::Unknown),
        };

        define_tp!(store, type_int, Int);
        define_tp!(store, type_bool, Bool);

        store
    }

    pub fn get_sym(&self, segs: &Segs) -> Option<Symbol> {
        self.sym_table.get(&segs.name).cloned()
    }

    pub fn define(&mut self, src: Source) {
        for def in src.defs {
            match def.kind {
                DefKind::Type(tp) => {
                    self.define_type(tp);
                }
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

impl fmt::Debug for Context {
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
