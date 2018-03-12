use std::io;
use std::collections::HashMap;

use common::*;
use def::*;

#[derive(Default)]
pub struct Context {
    pub type_defs: List<TypeDef>,
    pub func_defs: List<FuncDef>,

    pub sym_table: HashMap<String, Symbol>,

    pub type_int: Type,
    pub type_bool: Type,
}

impl Context {
    pub fn new() -> Self {
        macro_rules! define_tp {
            ($ctx:expr, $name:ident) => {{
                let kind = TypeDefKind::Builtin(BuiltinType::$name);
                let tp = TypeDef::new(stringify!($name), kind);
                let idx = $ctx.define_type(tp);
                let path = Path::Resolved(idx);
                Type::new(TypeKind::Named { path })
            }}
        }

        let mut ctx: Context = Default::default();

        ctx.type_int = define_tp!(ctx, Int);
        ctx.type_bool = define_tp!(ctx, Bool);

        ctx
    }

    pub fn get_sym(&self, segs: &Segs) -> Option<Symbol> {
        self.sym_table.get(&segs.name).cloned()
    }

    pub fn get_type(&self, tp: &Type) -> &TypeDef {
        &self.type_defs[tp.path().index()]
    }

    pub fn get_func(&self, func: &Func) -> &FuncDef {
        &self.func_defs[func.path.index()]
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

    pub fn print<Out>(&self, f: &mut Out) -> io::Result<()>
    where
        Out: io::Write,
    {
        for tp in &self.type_defs {
            tp.print(f, self)?;
            writeln!(f)?;
        }
        for func in &self.func_defs {
            func.print(f, self)?;
            writeln!(f)?;
        }
        Ok(())
    }
}
