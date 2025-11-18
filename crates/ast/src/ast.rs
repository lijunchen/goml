use parser::syntax::MySyntaxNodePtr;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub String);

impl Ident {
    pub fn new(name: &str) -> Self {
        Self(name.to_string())
    }

    pub fn starts_with_uppercase(&self) -> bool {
        self.0
            .chars()
            .next()
            .map(|c| c.is_uppercase())
            .unwrap_or(false)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstructorIdent {
    pub enum_name: Option<Ident>,
    pub variant: Ident,
}

impl ConstructorIdent {
    pub fn new(enum_name: Option<Ident>, variant: Ident) -> Self {
        Self { enum_name, variant }
    }

    pub fn from_variant(variant: Ident) -> Self {
        Self {
            enum_name: None,
            variant,
        }
    }

    pub fn enum_name(&self) -> Option<&Ident> {
        self.enum_name.as_ref()
    }

    pub fn variant(&self) -> &Ident {
        &self.variant
    }

    pub fn display(&self) -> String {
        if let Some(enum_name) = &self.enum_name {
            format!("{}::{}", enum_name.0, self.variant.0)
        } else {
            self.variant.0.clone()
        }
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
    TInt8,
    TInt16,
    TInt32,
    TInt64,
    TUint8,
    TUint16,
    TUint32,
    TUint64,
    TFloat32,
    TFloat64,
    TString,
    TTuple { typs: Vec<Ty> },
    TCon { name: String },
    TApp { ty: Box<Ty>, args: Vec<Ty> },
    TArray { len: usize, elem: Box<Ty> },
    TRef { elem: Box<Ty> },
    TFunc { params: Vec<Ty>, ret_ty: Box<Ty> },
}

#[derive(Debug, Clone)]
pub struct ClosureParam {
    pub name: Ident,
    pub ty: Option<Ty>,
    pub astptr: MySyntaxNodePtr,
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
    ExternType(ExternType),
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub name: Ident,
    pub generics: Vec<Ident>,
    pub params: Vec<(Ident, Ty)>,
    pub ret_ty: Option<Ty>,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct ExternGo {
    pub package_path: String,
    pub go_symbol: String,
    pub goml_name: Ident,
    pub explicit_go_symbol: bool,
    pub params: Vec<(Ident, Ty)>,
    pub ret_ty: Option<Ty>,
}

#[derive(Debug, Clone)]
pub struct ExternType {
    pub goml_name: Ident,
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub name: Ident,
    pub generics: Vec<Ident>,
    pub variants: Vec<(Ident, Vec<Ty>)>,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: Ident,
    pub generics: Vec<Ident>,
    pub fields: Vec<(Ident, Ty)>,
}

#[derive(Debug, Clone)]
pub struct TraitDef {
    pub name: Ident,
    pub method_sigs: Vec<TraitMethodSignature>,
}

#[derive(Debug, Clone)]
pub struct TraitMethodSignature {
    pub name: Ident,
    pub params: Vec<Ty>,
    pub ret_ty: Ty,
}

#[derive(Debug, Clone)]
pub struct ImplBlock {
    pub trait_name: Option<Ident>,
    pub for_type: Ty,
    pub methods: Vec<Fn>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    EVar {
        name: Ident,
        astptr: MySyntaxNodePtr,
    },
    EUnit,
    EBool {
        value: bool,
    },
    EInt {
        value: String,
    },
    EFloat {
        value: f64,
    },
    EString {
        value: String,
    },
    EConstr {
        constructor: ConstructorIdent,
        args: Vec<Expr>,
    },
    EStructLiteral {
        name: Ident,
        fields: Vec<(Ident, Expr)>,
    },
    ETuple {
        items: Vec<Expr>,
    },
    EArray {
        items: Vec<Expr>,
    },
    ELet {
        pat: Pat,
        annotation: Option<Ty>,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    EClosure {
        params: Vec<ClosureParam>,
        body: Box<Expr>,
    },
    EMatch {
        expr: Box<Expr>,
        arms: Vec<Arm>,
        astptr: MySyntaxNodePtr,
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
    ECall {
        func: Box<Expr>,
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
    EField {
        expr: Box<Expr>,
        field: Ident,
        astptr: MySyntaxNodePtr,
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
        name: Ident,
        astptr: MySyntaxNodePtr,
    },
    PUnit,
    PBool {
        value: bool,
    },
    PInt {
        value: String,
    },
    PString {
        value: String,
    },
    PConstr {
        constructor: ConstructorIdent,
        args: Vec<Pat>,
    },
    PStruct {
        name: Ident,
        fields: Vec<(Ident, Pat)>,
    },
    PTuple {
        pats: Vec<Pat>,
    },
    PWild,
}
