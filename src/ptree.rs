use common::*;

#[derive(Clone)]
pub struct SourceNode {
    pub filename: String,
    pub lines: Vec<String>,
    pub items: Vec<ItemNode>,
}

#[derive(Clone)]
pub enum ItemNode {
    Type(TypeNode),
    Func(FuncNode),
    Extern(SigNode),
}

#[derive(Clone)]
pub struct TypeNode {
    pub name: Name,
    pub span: Span,
    pub kind: TypeNodeKind,
}

#[derive(Clone)]
pub enum TypeNodeKind {
    Struct { fields: Vec<BindNode> },
}

#[derive(Clone)]
pub struct FuncNode {
    pub sig: SigNode,
    pub body: ExprNode,
    pub span: Span,
}

#[derive(Clone)]
pub struct SigNode {
    pub name: Name,
    pub params: Vec<BindNode>,
    pub ret: RetRef,
    pub span: Span,
}

#[derive(Clone)]
pub struct BindNode {
    pub name: String,
    pub tp: TypeRef,
    pub span: Span,
}

#[derive(Clone)]
pub struct LocalNode {
    pub name: String,
    pub span: Span,
}

#[derive(Clone)]
pub struct ExprNode {
    pub span: Span,
    pub kind: ExprNodeKind,
}

#[derive(Clone)]
pub enum ExprNodeKind {
    Block {
        stmts: Vec<ExprNode>,
    },
    Let {
        value: Box<ExprNode>,
        bind: LocalNode,
    },
    Construct {
        tp: TypeRef,
        args: Vec<ExprNode>,
    },
    Function {
        func: FuncRef,
        args: Vec<ExprNode>,
    },
    Member {
        value: Box<ExprNode>,
        mem: MemberRef,
    },
    Unary {
        op: UnaryOp,
        value: Box<ExprNode>,
    },
    Binary {
        op: BinaryOp,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    },
    If {
        cond: Box<ExprNode>,
        succ: Box<ExprNode>,
        fail: Box<ExprNode>,
    },
    Int {
        value: i32,
    },
    Bind {
        bind: BindRef,
    },
    Noop,
}

#[derive(Clone)]
pub struct ItemRef {
    pub source: usize,
    pub index: Index,
}

#[derive(Clone)]
pub enum RetRef {
    Named(TypeRef),
    Void,
}

#[derive(Clone)]
pub struct TypeRef {
    pub path: Path,
    pub span: Span,
}

#[derive(Clone)]
pub struct FuncRef {
    pub path: Path,
    pub span: Span,
}

#[derive(Clone)]
pub struct MemberRef {
    pub name: Name,
    pub span: Span,
}

#[derive(Clone)]
pub struct BindRef {
    pub path: Path,
    pub span: Span,
}
