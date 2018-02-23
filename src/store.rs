use std::collections::HashMap;
use std::fmt;

use ast::*;

pub struct Store {
    pub type_defs: Vec<TypeDef>,
    pub type_map: HashMap<String, Index>,
    pub func_defs: Vec<FuncDef>,
    pub func_map: HashMap<String, Index>,
}

impl Store {
    pub fn new() -> Self {
        let mut store = Store {
            type_defs: Vec::new(),
            type_map: HashMap::new(),
            func_defs: Vec::new(),
            func_map: HashMap::new(),
        };

        store.define_type(TypeDef {
            name: "Int".to_string(),
            kind: TypeDefKind::Int,
        });

        store
    }

    pub fn type_index(&self, path: &Path) -> Option<Index> {
        self.type_map.get(&path.name).cloned()
    }

    pub fn func_index(&self, path: &Path) -> Option<Index> {
        self.func_map.get(&path.name).cloned()
    }

    pub fn define(&mut self, src: Source) {
        for def in src.defs {
            match def.kind {
                DefKind::Type(tp) => self.define_type(tp),
                DefKind::Func(func) => self.define_func(func),
            }
        }
    }

    fn define_type(&mut self, tp: TypeDef) {
        let index = Index::new(self.type_defs.len());
        self.type_map.insert(tp.name.clone(), index);
        self.type_defs.push(tp);
    }

    fn define_func(&mut self, func: FuncDef) {
        let index = Index::new(self.func_defs.len());
        self.func_map.insert(func.name.clone(), index);
        self.func_defs.push(func);
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
