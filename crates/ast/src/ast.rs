use parser::syntax::MySyntaxNodePtr;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Lident(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Uident(pub String);

impl Uident {
    pub fn new(name: &str) -> Self {
        Self(name.to_string())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
}

impl BinaryOp {
    pub fn symbol(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::And => "&&",
            Self::Or => "||",
        }
    }

    pub fn method_name(self) -> &'static str {
        match self {
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mul => "mul",
            Self::Div => "div",
            Self::And => "and",
            Self::Or => "or",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl UnaryOp {
    pub fn symbol(self) -> &'static str {
        match self {
            Self::Neg => "-",
            Self::Not => "!",
        }
    }

    pub fn method_name(self) -> &'static str {
        match self {
            Self::Neg => "neg",
            Self::Not => "not",
        }
    }
}

#[derive(Debug, Clone)]
pub enum Ty {
    TUnit,
    TBool,
    TInt,
    TString,
    TTuple { typs: Vec<Ty> },
    TCon { name: String },
    TApp { ty: Box<Ty>, args: Vec<Ty> },
    TArray { len: usize, elem: Box<Ty> },
    TFunc { params: Vec<Ty>, ret_ty: Box<Ty> },
}

#[derive(Debug, Clone)]
pub struct File {
    pub toplevels: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum Item {
    EnumDef(EnumDef),
    StructDef(StructDef),
    TraitDef(TraitDef),
    ImplBlock(ImplBlock),
    Fn(Fn),
    ExternGo(ExternGo),
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub name: Lident,
    pub generics: Vec<Uident>,
    pub params: Vec<(Lident, Ty)>,
    pub ret_ty: Option<Ty>,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct ExternGo {
    pub package_path: String,
    pub go_symbol: String,
    pub goml_name: Lident,
    pub params: Vec<(Lident, Ty)>,
    pub ret_ty: Option<Ty>,
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub name: Uident,
    pub generics: Vec<Uident>,
    pub variants: Vec<(Uident, Vec<Ty>)>,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: Uident,
    pub generics: Vec<Uident>,
    pub fields: Vec<(Lident, Ty)>,
}

#[derive(Debug, Clone)]
pub struct TraitDef {
    pub name: Uident,
    pub method_sigs: Vec<TraitMethodSignature>,
}

#[derive(Debug, Clone)]
pub struct TraitMethodSignature {
    pub name: Lident,
    pub params: Vec<Ty>,
    pub ret_ty: Ty,
}

#[derive(Debug, Clone)]
pub struct ImplBlock {
    pub trait_name: Uident,
    pub for_type: Ty,
    pub methods: Vec<Fn>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    EVar {
        name: Lident,
        astptr: MySyntaxNodePtr,
    },
    EUnit,
    EBool {
        value: bool,
    },
    EInt {
        value: i32,
    },
    EString {
        value: String,
    },
    EConstr {
        vcon: Uident,
        args: Vec<Expr>,
    },
    EStructLiteral {
        name: Uident,
        fields: Vec<(Lident, Expr)>,
    },
    ETuple {
        items: Vec<Expr>,
    },
    EArray {
        items: Vec<Expr>,
    },
    ELet {
        pat: Pat,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    EMatch {
        expr: Box<Expr>,
        arms: Vec<Arm>,
    },
    EIf {
        cond: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    EWhile {
        cond: Box<Expr>,
        body: Box<Expr>,
    },
    EFor {
        pat: Pat,
        iter: Box<Expr>,
        body: Box<Expr>,
    },
    ECall {
        func: Lident,
        args: Vec<Expr>,
    },
    EUnary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    EBinary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    EProj {
        tuple: Box<Expr>,
        index: usize,
    },
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub pat: Pat,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub enum Pat {
    PVar {
        name: Lident,
        astptr: MySyntaxNodePtr,
    },
    PUnit,
    PBool {
        value: bool,
    },
    PInt {
        value: i32,
    },
    PString {
        value: String,
    },
    PConstr {
        vcon: Uident,
        args: Vec<Pat>,
    },
    PStruct {
        name: Uident,
        fields: Vec<(Lident, Pat)>,
    },
    PTuple {
        pats: Vec<Pat>,
    },
    PWild,
}
