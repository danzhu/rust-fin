use std::io;
use std::collections::HashMap;

use common::*;
use error::*;
use ptree::*;
use ast::*;
use ir::*;

pub struct Context {
    pub sources: List<SourceNode>,

    pub type_defs: List<TypeDef>,
    pub func_defs: List<FuncDef>,

    pub bodies: List<Body>,

    pub irs: List<Ir>,

    pub sym_table: HashMap<String, Symbol>,

    pub type_void: Type,
    pub type_int: Type,
    pub type_bool: Type,
}

impl Context {
    pub fn new() -> Self {
        macro_rules! define_tp {
            ($ctx:expr, $src:expr, $name:ident) => {{
                let span = Span::zero($src);
                let kind = TypeDefKind::Builtin(BuiltinType::$name);
                let name = stringify!($name).to_string();
                let path = Path { name: name.clone() };
                let tp = TypeDef { path, span, kind };
                let index = $ctx.type_defs.push(tp);
                $ctx.sym_table.insert(name, Symbol::Type(index));
                Type { kind: TypeKind::Named { index } }
            }}
        }

        let type_void = Type {
            kind: TypeKind::Void,
        };

        let mut ctx: Context = Context {
            sources: List::new(),
            type_defs: List::new(),
            func_defs: List::new(),
            bodies: List::new(),
            irs: List::new(),
            sym_table: HashMap::new(),
            type_void: type_void.clone(),
            // HACK: we need to create the context before we call methods
            type_int: type_void.clone(),
            type_bool: type_void.clone(),
        };

        let src = ctx.sources.push(SourceNode {
            filename: "<builtin>".to_string(),
            lines: Vec::new(),
            items: Vec::new(),
        });

        ctx.type_int = define_tp!(ctx, src, Int);
        ctx.type_bool = define_tp!(ctx, src, Bool);

        ctx
    }

    pub fn get_sym(&self, name: &str) -> Option<Symbol> {
        self.sym_table.get(name).cloned()
    }

    pub fn get_type(&self, tp: &Type) -> &TypeDef {
        match tp.kind {
            TypeKind::Named { index } => &self.type_defs[index],
            TypeKind::Void => panic!("void has no type def"),
        }
    }

    pub fn get_func(&self, func: &Func) -> &FuncDef {
        &self.func_defs[func.index]
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

#[derive(Copy, Clone)]
pub enum Symbol {
    Type(Index),
    Func(Index),
}

impl Symbol {
    pub fn format(&self, ctx: &Context) -> String {
        match *self {
            Symbol::Type(idx) => format!("type {}", ctx.type_defs[idx].path),
            Symbol::Func(idx) => format!("function {}", ctx.func_defs[idx].path),
        }
    }
}
