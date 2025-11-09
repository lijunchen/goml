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

#[derive(Debug, Clone)]
pub enum Primitive {
    Unit { value: () },
    Bool { value: bool },
    Int8 { value: i8 },
    Int16 { value: i16 },
    Int32 { value: i32 },
    Int64 { value: i64 },
    UInt8 { value: u8 },
    UInt16 { value: u16 },
    UInt32 { value: u32 },
    UInt64 { value: u64 },
    Float32 { value: f32 },
    Float64 { value: f64 },
    String { value: String },
}

impl Primitive {
    pub fn unit() -> Self {
        Primitive::Unit { value: () }
    }

    pub fn boolean(value: bool) -> Self {
        Primitive::Bool { value }
    }

    pub fn string(value: String) -> Self {
        Primitive::String { value }
    }

    pub fn from_int_literal(value: i128, ty: &Ty) -> Self {
        match ty {
            Ty::TInt8 => Primitive::Int8 { value: value as i8 },
            Ty::TInt16 => Primitive::Int16 {
                value: value as i16,
            },
            Ty::TInt32 | Ty::TInt => Primitive::Int32 {
                value: value as i32,
            },
            Ty::TInt64 => Primitive::Int64 {
                value: value as i64,
            },
            Ty::TUint8 => Primitive::UInt8 { value: value as u8 },
            Ty::TUint16 => Primitive::UInt16 {
                value: value as u16,
            },
            Ty::TUint32 => Primitive::UInt32 {
                value: value as u32,
            },
            Ty::TUint64 => Primitive::UInt64 {
                value: value as u64,
            },
            _ => panic!("Unsupported integer literal type {:?}", ty),
        }
    }

    pub fn from_float_literal(value: f64, ty: &Ty) -> Self {
        match ty {
            Ty::TFloat32 => Primitive::Float32 {
                value: value as f32,
            },
            Ty::TFloat64 => Primitive::Float64 { value },
            _ => panic!("Unsupported float literal type {:?}", ty),
        }
    }

    pub fn coerce(self, ty: &Ty) -> Self {
        match ty {
            Ty::TUnit => self.expect_unit(),
            Ty::TBool => self.expect_bool(),
            Ty::TInt8 => Primitive::Int8 {
                value: self.into_signed_value() as i8,
            },
            Ty::TInt16 => Primitive::Int16 {
                value: self.into_signed_value() as i16,
            },
            Ty::TInt32 | Ty::TInt => Primitive::Int32 {
                value: self.into_signed_value() as i32,
            },
            Ty::TInt64 => Primitive::Int64 {
                value: self.into_signed_value() as i64,
            },
            Ty::TUint8 => Primitive::UInt8 {
                value: self.into_unsigned_value() as u8,
            },
            Ty::TUint16 => Primitive::UInt16 {
                value: self.into_unsigned_value() as u16,
            },
            Ty::TUint32 => Primitive::UInt32 {
                value: self.into_unsigned_value() as u32,
            },
            Ty::TUint64 => Primitive::UInt64 {
                value: self.into_unsigned_value() as u64,
            },
            Ty::TFloat32 => Primitive::Float32 {
                value: self.into_float_value() as f32,
            },
            Ty::TFloat64 => Primitive::Float64 {
                value: self.into_float_value(),
            },
            Ty::TString => self.expect_string(),
            Ty::TVar(_) => self,
            Ty::TTuple { .. }
            | Ty::TCon { .. }
            | Ty::TApp { .. }
            | Ty::TArray { .. }
            | Ty::TRef { .. }
            | Ty::TParam { .. }
            | Ty::TFunc { .. } => panic!(
                "Cannot coerce primitive literal {:?} to non-primitive type {:?}",
                self, ty
            ),
        }
    }

    fn expect_unit(self) -> Self {
        match self {
            Primitive::Unit { .. } => Primitive::Unit { value: () },
            other => panic!("Expected unit primitive, got {:?}", other),
        }
    }

    fn expect_bool(self) -> Self {
        match self {
            Primitive::Bool { value } => Primitive::Bool { value },
            other => panic!("Expected bool primitive, got {:?}", other),
        }
    }

    fn expect_string(self) -> Self {
        match self {
            Primitive::String { value } => Primitive::String { value },
            other => panic!("Expected string primitive, got {:?}", other),
        }
    }

    fn into_signed_value(self) -> i128 {
        match self {
            Primitive::Int8 { value } => value as i128,
            Primitive::Int16 { value } => value as i128,
            Primitive::Int32 { value } => value as i128,
            Primitive::Int64 { value } => value as i128,
            other => panic!("Expected signed integer primitive, got {:?}", other),
        }
    }

    fn into_unsigned_value(self) -> u128 {
        match self {
            Primitive::UInt8 { value } => value as u128,
            Primitive::UInt16 { value } => value as u128,
            Primitive::UInt32 { value } => value as u128,
            Primitive::UInt64 { value } => value as u128,
            other => panic!("Expected unsigned integer primitive, got {:?}", other),
        }
    }

    fn into_float_value(self) -> f64 {
        match self {
            Primitive::Float32 { value } => value as f64,
            Primitive::Float64 { value } => value,
            other => panic!("Expected float primitive, got {:?}", other),
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        if let Primitive::Bool { value } = self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_signed(&self) -> Option<i128> {
        match self {
            Primitive::Int8 { value } => Some(*value as i128),
            Primitive::Int16 { value } => Some(*value as i128),
            Primitive::Int32 { value } => Some(*value as i128),
            Primitive::Int64 { value } => Some(*value as i128),
            _ => None,
        }
    }

    pub fn as_unsigned(&self) -> Option<u128> {
        match self {
            Primitive::UInt8 { value } => Some(*value as u128),
            Primitive::UInt16 { value } => Some(*value as u128),
            Primitive::UInt32 { value } => Some(*value as u128),
            Primitive::UInt64 { value } => Some(*value as u128),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            Primitive::Float32 { value } => Some(*value as f64),
            Primitive::Float64 { value } => Some(*value),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Primitive::String { value } => Some(value.as_str()),
            _ => None,
        }
    }
}

impl std::fmt::Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::Unit { .. } => write!(f, "()"),
            Primitive::Bool { value } => write!(f, "{}", value),
            Primitive::Int8 { value } => write!(f, "{}", value),
            Primitive::Int16 { value } => write!(f, "{}", value),
            Primitive::Int32 { value } => write!(f, "{}", value),
            Primitive::Int64 { value } => write!(f, "{}", value),
            Primitive::UInt8 { value } => write!(f, "{}", value),
            Primitive::UInt16 { value } => write!(f, "{}", value),
            Primitive::UInt32 { value } => write!(f, "{}", value),
            Primitive::UInt64 { value } => write!(f, "{}", value),
            Primitive::Float32 { value } => write!(f, "{}", value),
            Primitive::Float64 { value } => write!(f, "{}", value),
            Primitive::String { value } => write!(f, "{:?}", value),
        }
    }
}

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
    EPrimitive {
        value: Primitive,
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
    EField {
        expr: Box<Expr>,
        field_name: String,
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
            Self::EPrimitive { ty, .. } => ty.clone(),
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
            Self::EField { ty, .. } => ty.clone(),
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
    PPrimitive {
        value: Primitive,
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
            Self::PPrimitive { ty, .. } => ty.clone(),
            Self::PConstr { ty, .. } => ty.clone(),
            Self::PTuple { ty, .. } => ty.clone(),
            Self::PWild { ty, .. } => ty.clone(),
        }
    }
}
