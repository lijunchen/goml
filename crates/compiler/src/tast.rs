use ena::unify::{EqUnifyValue, UnifyKey};
use parser::syntax::MySyntaxNodePtr;

use crate::common::{Constructor, Prim};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TastIdent(pub String);

impl TastIdent {
    pub fn new(name: &str) -> Self {
        TastIdent(name.to_string())
    }
}

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
    pub generics: Vec<String>,
    pub trait_name: Option<TastIdent>,
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    TVar(TypeVar),
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
    TEnum { name: String },
    TStruct { name: String },
    TApp { ty: Box<Ty>, args: Vec<Ty> },
    TArray { len: usize, elem: Box<Ty> },
    TVec { elem: Box<Ty> },
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
            Self::TInt8 => write!(f, "TInt8"),
            Self::TInt16 => write!(f, "TInt16"),
            Self::TInt32 => write!(f, "TInt32"),
            Self::TInt64 => write!(f, "TInt64"),
            Self::TUint8 => write!(f, "TUint8"),
            Self::TUint16 => write!(f, "TUint16"),
            Self::TUint32 => write!(f, "TUint32"),
            Self::TUint64 => write!(f, "TUint64"),
            Self::TFloat32 => write!(f, "TFloat32"),
            Self::TFloat64 => write!(f, "TFloat64"),
            Self::TString => write!(f, "TString"),
            Self::TTuple { typs } => write!(f, "TTuple({:?})", typs),
            Self::TEnum { name } => write!(f, "TEnum({})", name),
            Self::TStruct { name } => write!(f, "TStruct({})", name),
            Self::TApp { ty, args } => write!(f, "TApp({:?}, {:?})", ty, args),
            Self::TArray { len, elem } => write!(f, "TArray({}, {:?})", len, elem),
            Self::TVec { elem } => write!(f, "TVec({:?})", elem),
            Self::TRef { elem } => write!(f, "TRef({:?})", elem),
            Self::TParam { name } => write!(f, "TParam({})", name),
            Self::TFunc { params, ret_ty } => write!(f, "TFunc({:?}, {:?})", params, ret_ty),
        }
    }
}

impl Ty {
    pub fn get_constr_name_unsafe(&self) -> String {
        match self {
            Self::TEnum { name } | Self::TStruct { name } => name.clone(),
            Self::TApp { ty, .. } => ty.get_constr_name_unsafe(),
            Self::TVec { .. } => "Vec".to_string(),
            Self::TRef { .. } => "Ref".to_string(),
            _ => {
                panic!("Expected a constructor type, got: {:?}", self)
            }
        }
    }
}

impl EqUnifyValue for Ty {}

impl Prim {
    pub fn unit() -> Self {
        Prim::Unit { value: () }
    }

    pub fn boolean(value: bool) -> Self {
        Prim::Bool { value }
    }

    pub fn string(value: String) -> Self {
        Prim::String { value }
    }

    pub fn zero_for_int_ty(ty: &Ty) -> Self {
        match ty {
            Ty::TInt8 => Prim::Int8 { value: 0 },
            Ty::TInt16 => Prim::Int16 { value: 0 },
            Ty::TInt32 => Prim::Int32 { value: 0 },
            Ty::TInt64 => Prim::Int64 { value: 0 },
            Ty::TUint8 => Prim::UInt8 { value: 0 },
            Ty::TUint16 => Prim::UInt16 { value: 0 },
            Ty::TUint32 => Prim::UInt32 { value: 0 },
            Ty::TUint64 => Prim::UInt64 { value: 0 },
            _ => panic!("Unsupported integer literal type {:?}", ty),
        }
    }

    pub fn from_float_literal(value: f64, ty: &Ty) -> Self {
        match ty {
            Ty::TFloat32 => Prim::Float32 {
                value: value as f32,
            },
            Ty::TFloat64 => Prim::Float64 { value },
            _ => panic!("Unsupported float literal type {:?}", ty),
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        if let Prim::Bool { value } = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_int8(&self) -> Option<i8> {
        if let Prim::Int8 { value } = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_int16(&self) -> Option<i16> {
        if let Prim::Int16 { value } = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_int32(&self) -> Option<i32> {
        if let Prim::Int32 { value } = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_int64(&self) -> Option<i64> {
        if let Prim::Int64 { value } = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_uint8(&self) -> Option<u8> {
        if let Prim::UInt8 { value } = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_uint16(&self) -> Option<u16> {
        if let Prim::UInt16 { value } = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_uint32(&self) -> Option<u32> {
        if let Prim::UInt32 { value } = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_uint64(&self) -> Option<u64> {
        if let Prim::UInt64 { value } = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_float32(&self) -> Option<f32> {
        if let Prim::Float32 { value } = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_float64(&self) -> Option<f64> {
        if let Prim::Float64 { value } = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Prim::String { value } => Some(value.as_str()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(u32);

#[derive(Debug, Clone)]
pub enum BinaryResolution {
    Builtin,
    Overloaded { trait_name: TastIdent },
}

#[derive(Debug, Clone)]
pub enum UnaryResolution {
    Builtin,
    Overloaded { trait_name: TastIdent },
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
    EPrim {
        value: Prim,
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
        captures: Vec<(String, Ty)>,
    },
    ELet {
        pat: Pat,
        value: Box<Expr>,
        ty: Ty,
    },
    EBlock {
        exprs: Vec<Expr>,
        ty: Ty,
    },
    EMatch {
        expr: Box<Expr>,
        arms: Vec<Arm>,
        ty: Ty,
        astptr: Option<MySyntaxNodePtr>,
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
    EGo {
        expr: Box<Expr>,
        ty: Ty,
    },
    ECall {
        func: Box<Expr>,
        args: Vec<Expr>,
        ty: Ty,
    },
    EUnary {
        op: common_defs::UnaryOp,
        expr: Box<Expr>,
        ty: Ty,
        resolution: UnaryResolution,
    },
    EProj {
        tuple: Box<Expr>,
        index: usize,
        ty: Ty,
    },
    EField {
        expr: Box<Expr>,
        field_name: String,
        ty: Ty,
        astptr: Option<MySyntaxNodePtr>,
    },
    EBinary {
        op: common_defs::BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        ty: Ty,
        resolution: BinaryResolution,
    },
    ETraitMethod {
        trait_name: TastIdent,
        method_name: TastIdent,
        ty: Ty,
        astptr: Option<MySyntaxNodePtr>,
    },
    EInherentMethod {
        receiver_ty: Ty,
        method_name: TastIdent,
        ty: Ty,
        astptr: Option<MySyntaxNodePtr>,
    },
}

impl Expr {
    pub fn get_ty(&self) -> Ty {
        match self {
            Self::EVar { ty, .. } => ty.clone(),
            Self::EPrim { ty, .. } => ty.clone(),
            Self::EConstr { ty, .. } => ty.clone(),
            Self::ETuple { ty, .. } => ty.clone(),
            Self::EArray { ty, .. } => ty.clone(),
            Self::EClosure { ty, .. } => ty.clone(),
            Self::ELet { ty, .. } => ty.clone(),
            Self::EBlock { ty, .. } => ty.clone(),
            Self::EMatch { ty, .. } => ty.clone(),
            Self::EIf { ty, .. } => ty.clone(),
            Self::EWhile { ty, .. } => ty.clone(),
            Self::EGo { ty, .. } => ty.clone(),
            Self::ECall { ty, .. } => ty.clone(),
            Self::EUnary { ty, .. } => ty.clone(),
            Self::EProj { ty, .. } => ty.clone(),
            Self::EField { ty, .. } => ty.clone(),
            Self::EBinary { ty, .. } => ty.clone(),
            Self::ETraitMethod { ty, .. } => ty.clone(),
            Self::EInherentMethod { ty, .. } => ty.clone(),
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
    PPrim {
        value: Prim,
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
            Self::PPrim { ty, .. } => ty.clone(),
            Self::PConstr { ty, .. } => ty.clone(),
            Self::PTuple { ty, .. } => ty.clone(),
            Self::PWild { ty, .. } => ty.clone(),
        }
    }
}
