use std::fmt;

use common;

pub struct Module {
    pub items: Vec<Item>,
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        for item in &self.items {
            if first {
                first = false;
            } else {
                writeln!(f)?;
            }

            item.fmt(f)?;
        }
        Ok(())
    }
}

pub struct Item {
    pub kind: ItemKind,
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ItemKind::Type(ref def) => def.fmt(f),
            ItemKind::Define(ref def) => {
                write!(f, "define ")?;
                def.fmt(f)
            }
            ItemKind::Declare(ref def) => {
                write!(f, "declare ")?;
                def.fmt(f)?;
                writeln!(f)
            }
        }
    }
}

pub enum ItemKind {
    Type(TypeDef),
    Define(FuncDef),
    Declare(FuncDecl),
}

pub struct TypeDef {
    pub name: Type,
    pub kind: TypeDefKind,
}

impl fmt::Display for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            TypeDefKind::Struct { ref fields } => {
                write!(f, "{} = type {{ ", self.name)?;
                let mut first = true;
                for field in fields {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }

                    field.fmt(f)?;
                }
                write!(f, " }}")?;
            }
        }

        writeln!(f)
    }
}

pub enum TypeDefKind {
    Struct { fields: Vec<Type> },
}

pub struct FuncDef {
    decl: FuncDecl,
    blocks: Vec<BlockDef>,
}

impl FuncDef {
    pub fn new(decl: FuncDecl) -> Self {
        FuncDef {
            decl,
            blocks: Vec::new(),
        }
    }

    pub fn add_block(&mut self) -> Block {
        let def = BlockDef {
            instrs: Vec::new(),
            term: None,
        };
        let block = Block {
            index: self.blocks.len(),
        };
        self.blocks.push(def);
        block
    }

    pub fn comment<Str>(&mut self, block: Block, comment: Str)
    where
        Str: Into<String>,
    {
        let instr = InstrKind::Comment {
            comment: comment.into(),
        };
        self.write(block, instr);
    }

    pub fn write(&mut self, block: Block, kind: InstrKind) -> Value {
        let def = &mut self.blocks[block.index];
        let value = Value::Instr(block.index, def.instrs.len());
        def.instrs.push(Instr { kind });
        value
    }

    pub fn end(&mut self, block: Block, kind: TermKind) {
        let def = &mut self.blocks[block.index];
        def.term = Some(Term { kind })
    }
}

impl fmt::Display for FuncDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn fmt_instr(reg: &Value, instr: &Instr, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", common::INDENT)?;

            match instr.kind {
                InstrKind::Comment { ref comment } => {
                    write!(f, "; {}", comment)?;
                }
                InstrKind::Alloca { ref tp } => {
                    write!(f, "{} = alloca {}", reg, tp)?;
                }
                InstrKind::Arith {
                    op,
                    ref tp,
                    ref left,
                    ref right,
                } => {
                    write!(f, "{} = {} {} {}, {}", reg, op, tp, left, right)?;
                }
                InstrKind::Icmp {
                    op,
                    ref tp,
                    ref left,
                    ref right,
                } => {
                    write!(f, "{} = icmp {} {} {}, {}", reg, op, tp, left, right)?;
                }
                InstrKind::Load { ref tp, ref addr } => {
                    write!(f, "{} = load {}, {}* {}", reg, tp, tp, addr)?;
                }
                InstrKind::Store {
                    ref tp,
                    ref value,
                    ref addr,
                } => {
                    write!(f, "store {} {}, {}* {}", tp, value, tp, addr)?;
                }
                InstrKind::GetElementPtr {
                    ref tp,
                    ref ptr,
                    ref offsets,
                } => {
                    write!(f, "{} = getelementptr {}, {}* {}", reg, tp, tp, ptr)?;
                    for &(ref tp, ref offset) in offsets {
                        write!(f, ", {} {}", tp, offset)?;
                    }
                }
                InstrKind::Call {
                    ref tp,
                    ref name,
                    ref args,
                } => {
                    if let Type::Void = *tp {
                        // void cannot be stored to a register
                    } else {
                        write!(f, "{} = ", reg)?;
                    }

                    write!(f, "call {} {}(", tp, name)?;
                    let mut first = true;
                    for &(ref tp, ref arg) in args {
                        if first {
                            first = false;
                        } else {
                            write!(f, ", ")?;
                        }

                        write!(f, "{} {}", tp, arg)?;
                    }
                    write!(f, ")")?;
                }
            }

            writeln!(f)
        }

        fn fmt_term(term: &Term, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", common::INDENT)?;

            match term.kind {
                TermKind::BrCond {
                    ref cond,
                    ref succ,
                    ref fail,
                } => {
                    write!(f, "br i1 {}, label {}, label {}", cond, succ, fail)?;
                }
                TermKind::Br { ref block } => {
                    write!(f, "br label {}", block)?;
                }
                TermKind::RetVal { ref tp, ref value } => {
                    write!(f, "ret {} {}", tp, value)?;
                }
                TermKind::Ret => {
                    write!(f, "ret void")?;
                }
                TermKind::Unreachable => {
                    write!(f, "unreachable")?;
                }
            }

            writeln!(f)
        }

        self.decl.fmt(f)?;

        writeln!(f, " {{")?;
        let mut first = true;
        for (block_idx, block) in self.blocks.iter().enumerate() {
            if first {
                first = false;
            } else {
                writeln!(f)?;
            }

            fmt_block(block_idx, f)?;
            writeln!(f, ":")?;

            for (instr_idx, instr) in block.instrs.iter().enumerate() {
                let reg = Value::Instr(block_idx, instr_idx);
                fmt_instr(&reg, instr, f)?;
            }

            let term = block.term.as_ref().expect("unterminated block");
            fmt_term(term, f)?;
        }
        writeln!(f, "}}")
    }
}

pub struct FuncDecl {
    pub name: Value,
    pub ret: Type,
    pub params: Vec<ParamDef>,
}

impl fmt::Display for FuncDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}(", self.ret, self.name)?;
        let mut first = true;
        for param in &self.params {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }

            param.fmt(f)?;
        }
        write!(f, ")")
    }
}

pub struct ParamDef {
    pub name: Value,
    pub tp: Type,
}

impl fmt::Display for ParamDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.tp, self.name)
    }
}

pub struct BlockDef {
    pub instrs: Vec<Instr>,
    pub term: Option<Term>,
}

pub struct Instr {
    pub kind: InstrKind,
}

pub enum InstrKind {
    Comment {
        comment: String,
    },
    Alloca {
        tp: Type,
    },
    Arith {
        op: ArithOp,
        tp: Type,
        left: Value,
        right: Value,
    },
    Icmp {
        op: IcmpOp,
        tp: Type,
        left: Value,
        right: Value,
    },
    Load {
        tp: Type,
        addr: Value,
    },
    Store {
        tp: Type,
        value: Value,
        addr: Value,
    },
    GetElementPtr {
        tp: Type,
        ptr: Value,
        offsets: Vec<(Type, Value)>,
    },
    Call {
        tp: Type,
        name: Value,
        args: Vec<(Type, Value)>,
    },
}

pub struct Term {
    pub kind: TermKind,
}

pub enum TermKind {
    BrCond {
        cond: Value,
        succ: Block,
        fail: Block,
    },
    Br {
        block: Block,
    },
    RetVal {
        tp: Type,
        value: Value,
    },
    Ret,
    Unreachable,
}

#[derive(Clone, Copy)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Sdiv,
    Srem,
    Xor,
}

impl fmt::Display for ArithOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match *self {
            ArithOp::Add => "add",
            ArithOp::Sub => "sub",
            ArithOp::Mul => "mul",
            ArithOp::Sdiv => "sdiv",
            ArithOp::Srem => "srem",
            ArithOp::Xor => "xor",
        };
        write!(f, "{}", op)
    }
}

#[derive(Clone, Copy)]
pub enum IcmpOp {
    Eq,
    Ne,
    Slt,
    Sgt,
}

impl fmt::Display for IcmpOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match *self {
            IcmpOp::Eq => "eq",
            IcmpOp::Ne => "ne",
            IcmpOp::Slt => "slt",
            IcmpOp::Sgt => "sgt",
        };
        write!(f, "{}", op)
    }
}

#[derive(Clone)]
pub enum Type {
    Identified(String),
    Builtin(&'static str),
    Pointer(Box<Type>),
    Void,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Identified(ref id) => write!(f, "%{}", id),
            Type::Builtin(name) => write!(f, "{}", name),
            Type::Pointer(ref tp) => write!(f, "{}*", tp),
            Type::Void => write!(f, "void"),
        }
    }
}

#[derive(Clone, Copy)]
pub struct Block {
    pub index: usize,
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "%")?;
        fmt_block(self.index, f)
    }
}

#[derive(Clone)]
pub enum Value {
    Func(String),
    Param(usize),
    Instr(usize, usize),
    Int(i32),
    Bool(bool),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Func(ref name) => write!(f, "@{}", name),
            Value::Param(idx) => write!(f, "%_p{}", idx),
            Value::Instr(block, idx) => write!(f, "%_{}_{}", block, idx),
            Value::Int(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
        }
    }
}

fn fmt_block(idx: usize, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "b{}", idx)
}
