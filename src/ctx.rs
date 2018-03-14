use std::io;
use std::collections::HashMap;

use common::*;
use def::*;
use ir::*;

#[derive(Default)]
pub struct Context {
    pub sources: List<Source>,

    pub type_defs: List<TypeDef>,
    pub func_defs: List<FuncDef>,

    pub irs: List<Ir>,

    pub sym_table: HashMap<String, Symbol>,

    pub type_int: Type,
    pub type_bool: Type,
}

impl Context {
    pub fn new() -> Self {
        macro_rules! define_tp {
            ($ctx:expr, $src:expr, $name:ident) => {{
                let span = Span::zero($src);
                let kind = TypeDefKind::Builtin(BuiltinType::$name);
                let tp = TypeDef::new(stringify!($name), span, kind);
                let idx = $ctx.type_defs.push(tp);
                let path = Path::Resolved(idx);
                Type::new(TypeKind::Named { path })
            }}
        }

        let mut ctx: Context = Default::default();

        let src = ctx.sources.push(Source::new("<builtin>", ""));

        ctx.type_int = define_tp!(ctx, src, Int);
        ctx.type_bool = define_tp!(ctx, src, Bool);

        ctx
    }

    pub fn get_sym(&self, name: &Name) -> Option<Symbol> {
        self.sym_table.get(&name.name).cloned()
    }

    pub fn get_type(&self, tp: &Type) -> &TypeDef {
        &self.type_defs[tp.path().index()]
    }

    pub fn get_func(&self, func: &Func) -> &FuncDef {
        &self.func_defs[func.path.index()]
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
