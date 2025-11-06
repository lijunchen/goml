use ast::ast::{BinaryOp, Uident, UnaryOp};
use ena::unify::{EqUnifyValue, UnifyKey};
use parser::syntax::MySyntaxNodePtr;

pub const ARRAY_WILDCARD_LEN: usize = usize::MAX;

#[derive(Debug)]
pub struct File {
    pub toplevels: Vec<Item>,
}

#[derive(Debug)]
pub enum Item {
    ImplBlock(ImplBlock),
    Fn(Fn),
    ExternGo(ExternGo),
    ExternType(ExternType),
}

#[derive(Debug)]
pub struct ImplBlock {
    pub trait_name: Uident,
    pub for_type: Ty,
    pub methods: Vec<Fn>,
}

#[derive(Debug)]
pub struct Fn {
    pub name: String,
    pub params: Vec<(String, Ty)>,
    pub ret_ty: Ty,
    pub body: Expr,
}

#[derive(Debug)]
pub struct ExternGo {
    pub goml_name: String,
    pub go_name: String,
    pub package_path: String,
    pub params: Vec<(String, Ty)>,
    pub ret_ty: Ty,
}

#[derive(Debug)]
pub struct ExternType {
    pub goml_name: String,
}

#[derive(Debug, Clone)]
pub struct ClosureParam {
    pub name: String,
    pub ty: Ty,
    pub astptr: Option<MySyntaxNodePtr>,
}

impl ClosureParam {
    pub fn get_ty(&self) -> Ty {
        self.ty.clone()
    }
}

#[derive(Debug, Clone)]
pub struct EnumConstructor {
    pub type_name: Uident,
    pub variant: Uident,
    pub index: usize,
}

impl EnumConstructor {
    pub fn enum_index(&self) -> usize {
        self.index
    }
}

#[derive(Debug, Clone)]
pub struct StructConstructor {
    pub type_name: Uident,
}

#[derive(Debug, Clone)]
pub enum Constructor {
    Enum(EnumConstructor),
    Struct(StructConstructor),
}

impl Constructor {
    pub fn name(&self) -> &Uident {
        match self {
            Constructor::Enum(constructor) => &constructor.variant,
            Constructor::Struct(constructor) => &constructor.type_name,
        }
    }

    pub fn type_name(&self) -> &Uident {
        match self {
            Constructor::Enum(constructor) => &constructor.type_name,
            Constructor::Struct(constructor) => &constructor.type_name,
        }
    }

    pub fn is_struct(&self) -> bool {
        matches!(self, Constructor::Struct(_))
    }

    pub fn as_enum(&self) -> Option<&EnumConstructor> {
        if let Constructor::Enum(constructor) = self {
            Some(constructor)
        } else {
            None
        }
    }

    pub fn as_struct(&self) -> Option<&StructConstructor> {
        if let Constructor::Struct(constructor) = self {
            Some(constructor)
        } else {
            None
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    TVar(TypeVar),
    TUnit,
    TBool,
    TInt,
    TString,
    TTuple { typs: Vec<Ty> },
    TCon { name: String },
    TApp { ty: Box<Ty>, args: Vec<Ty> },
    TArray { len: usize, elem: Box<Ty> },
    TRef { elem: Box<Ty> },
    TParam { name: String },
    TFunc { params: Vec<Ty>, ret_ty: Box<Ty> },
}

impl std::fmt::Debug for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TVar(var) => write!(f, "TVar({})", var.0),
            Self::TUnit => write!(f, "TUnit"),
            Self::TBool => write!(f, "TBool"),
            Self::TInt => write!(f, "TInt"),
            Self::TString => write!(f, "TString"),
            Self::TTuple { typs } => write!(f, "TTuple({:?})", typs),
            Self::TCon { name } => write!(f, "TCon({})", name),
            Self::TApp { ty, args } => write!(f, "TApp({:?}, {:?})", ty, args),
            Self::TArray { len, elem } => write!(f, "TArray({}, {:?})", len, elem),
            Self::TRef { elem } => write!(f, "TRef({:?})", elem),
            Self::TParam { name } => write!(f, "TParam({})", name),
            Self::TFunc { params, ret_ty } => write!(f, "TFunc({:?}, {:?})", params, ret_ty),
        }
    }
}

impl Ty {
    pub fn get_constr_name_unsafe(&self) -> String {
        match self {
            Self::TCon { name } => name.clone(),
            Self::TApp { ty, .. } => ty.get_constr_name_unsafe(),
            Self::TRef { .. } => "Ref".to_string(),
            _ => {
                panic!("Expected a constructor type, got: {:?}", self)
            }
        }
    }
}

impl EqUnifyValue for Ty {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(u32);

#[derive(Debug, Clone)]
pub enum BinaryResolution {
    Builtin,
    Overloaded { trait_name: Uident },
}

#[derive(Debug, Clone)]
pub enum UnaryResolution {
    Builtin,
    Overloaded { trait_name: Uident },
}

impl UnifyKey for TypeVar {
    type Value = Option<Ty>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> TypeVar {
        TypeVar(u)
    }

    fn tag() -> &'static str {
        "TypeVar"
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    EVar {
        name: String,
        ty: Ty,
        astptr: Option<MySyntaxNodePtr>,
    },
    EUnit {
        ty: Ty,
    },
    EBool {
        value: bool,
        ty: Ty,
    },
    EInt {
        value: i32,
        ty: Ty,
    },
    EString {
        value: String,
        ty: Ty,
    },
    EConstr {
        constructor: Constructor,
        args: Vec<Expr>,
        ty: Ty,
    },
    ETuple {
        items: Vec<Expr>,
        ty: Ty,
    },
    EArray {
        items: Vec<Expr>,
        ty: Ty,
    },
    EClosure {
        params: Vec<ClosureParam>,
        body: Box<Expr>,
        ty: Ty,
    },
    ELet {
        pat: Pat,
        value: Box<Expr>,
        body: Box<Expr>,
        ty: Ty,
    },
    EMatch {
        expr: Box<Expr>,
        arms: Vec<Arm>,
        ty: Ty,
    },
    EIf {
        cond: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
        ty: Ty,
    },
    EWhile {
        cond: Box<Expr>,
        body: Box<Expr>,
        ty: Ty,
    },
    ECall {
        func: Box<Expr>,
        args: Vec<Expr>,
        ty: Ty,
    },
    EUnary {
        op: UnaryOp,
        expr: Box<Expr>,
        ty: Ty,
        resolution: UnaryResolution,
    },
    EProj {
        tuple: Box<Expr>,
        index: usize,
        ty: Ty,
    },
    EBinary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        ty: Ty,
        resolution: BinaryResolution,
    },
}

impl Expr {
    pub fn get_ty(&self) -> Ty {
        match self {
            Self::EVar { ty, .. } => ty.clone(),
            Self::EUnit { ty, .. } => ty.clone(),
            Self::EBool { ty, .. } => ty.clone(),
            Self::EInt { ty, .. } => ty.clone(),
            Self::EString { ty, .. } => ty.clone(),
            Self::EConstr { ty, .. } => ty.clone(),
            Self::ETuple { ty, .. } => ty.clone(),
            Self::EArray { ty, .. } => ty.clone(),
            Self::EClosure { ty, .. } => ty.clone(),
            Self::ELet { ty, .. } => ty.clone(),
            Self::EMatch { ty, .. } => ty.clone(),
            Self::EIf { ty, .. } => ty.clone(),
            Self::EWhile { ty, .. } => ty.clone(),
            Self::ECall { ty, .. } => ty.clone(),
            Self::EUnary { ty, .. } => ty.clone(),
            Self::EProj { ty, .. } => ty.clone(),
            Self::EBinary { ty, .. } => ty.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub pat: Pat,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub enum Pat {
    PVar {
        name: String,
        ty: Ty,
        astptr: Option<MySyntaxNodePtr>,
    },
    PUnit {
        ty: Ty,
    },
    PBool {
        value: bool,
        ty: Ty,
    },
    PInt {
        value: i32,
        ty: Ty,
    },
    PString {
        value: String,
        ty: Ty,
    },
    PConstr {
        constructor: Constructor,
        args: Vec<Pat>,
        ty: Ty,
    },
    PTuple {
        items: Vec<Pat>,
        ty: Ty,
    },
    PWild {
        ty: Ty,
    },
}

impl Pat {
    pub fn get_ty(&self) -> Ty {
        match self {
            Self::PVar { ty, .. } => ty.clone(),
            Self::PUnit { ty, .. } => ty.clone(),
            Self::PBool { ty, .. } => ty.clone(),
            Self::PInt { ty, .. } => ty.clone(),
            Self::PString { ty, .. } => ty.clone(),
            Self::PConstr { ty, .. } => ty.clone(),
            Self::PTuple { ty, .. } => ty.clone(),
            Self::PWild { ty, .. } => ty.clone(),
        }
    }
}
