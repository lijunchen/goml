use parser::syntax::MySyntaxNodePtr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TraitId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ImplId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CtorId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GenericParamId(pub u32);

#[derive(Debug, Clone)]
pub enum HirType {
    Builtin(BuiltinType),
    Tuple(Vec<HirTypeId>),
    Func {
        params: Vec<HirTypeId>,
        ret: HirTypeId,
    },
    Array {
        elem: HirTypeId,
        len: usize,
    },
    App {
        base: HirTypeId,
        args: Vec<HirTypeId>,
    },
    Named(TypeId, Vec<HirTypeId>),
    Generic(GenericParamId),
}

pub type HirTypeId = la_arena::Idx<HirType>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    Unit,
    Bool,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float32,
    Float64,
    String,
}

#[derive(Debug, Clone)]
pub struct HirFile {
    pub items: Vec<HirItemId>,
}

pub type HirItemId = la_arena::Idx<HirItem>;

#[derive(Debug, Clone)]
pub enum HirItem {
    Fn(HirFn),
    Struct(HirStruct),
    Enum(HirEnum),
    Trait(HirTrait),
    ImplTrait(HirImplTrait),
    ImplInherent(HirImplInherent),
    ExternGo(HirExternGo),
    ExternType(HirExternType),
    ExternBuiltin(HirExternBuiltin),
}

#[derive(Debug, Clone)]
pub struct HirFn {
    pub fn_id: ItemId,
    pub generics: Vec<GenericParamId>,
    pub params: Vec<(LocalId, HirTypeId)>,
    pub ret_ty: HirTypeId,
    pub body: HirExprId,
    pub astptr: Option<MySyntaxNodePtr>,
}

#[derive(Debug, Clone)]
pub struct HirStruct {
    pub type_id: TypeId,
    pub generics: Vec<GenericParamId>,
    pub fields: Vec<(FieldId, HirTypeId)>,
    pub astptr: Option<MySyntaxNodePtr>,
}

#[derive(Debug, Clone)]
pub struct HirEnum {
    pub type_id: TypeId,
    pub generics: Vec<GenericParamId>,
    pub variants: Vec<HirEnumVariant>,
    pub astptr: Option<MySyntaxNodePtr>,
}

#[derive(Debug, Clone)]
pub struct HirEnumVariant {
    pub ctor_id: CtorId,
    pub payload: Vec<HirTypeId>,
    pub astptr: Option<MySyntaxNodePtr>,
}

#[derive(Debug, Clone)]
pub struct HirTrait {
    pub trait_id: TraitId,
    pub methods: Vec<HirTraitMethodSig>,
    pub astptr: Option<MySyntaxNodePtr>,
}

#[derive(Debug, Clone)]
pub struct HirTraitMethodSig {
    pub name: ItemId,
    pub params: Vec<HirTypeId>,
    pub ret_ty: HirTypeId,
    pub astptr: Option<MySyntaxNodePtr>,
}

#[derive(Debug, Clone)]
pub struct HirImplTrait {
    pub impl_id: ImplId,
    pub trait_id: TraitId,
    pub for_type: HirTypeId,
    pub generics: Vec<GenericParamId>,
    pub methods: Vec<HirItemId>,
    pub astptr: Option<MySyntaxNodePtr>,
}

#[derive(Debug, Clone)]
pub struct HirImplInherent {
    pub impl_id: ImplId,
    pub for_type: HirTypeId,
    pub methods: Vec<HirItemId>,
    pub astptr: Option<MySyntaxNodePtr>,
}

#[derive(Debug, Clone)]
pub struct HirExternGo {
    pub item_id: ItemId,
    pub package_path: String,
    pub go_symbol: String,
    pub explicit_go_symbol: bool,
    pub params: Vec<(LocalId, HirTypeId)>,
    pub ret_ty: HirTypeId,
    pub astptr: Option<MySyntaxNodePtr>,
}

#[derive(Debug, Clone)]
pub struct HirExternType {
    pub type_id: TypeId,
    pub astptr: Option<MySyntaxNodePtr>,
}

#[derive(Debug, Clone)]
pub struct HirExternBuiltin {
    pub item_id: ItemId,
    pub params: Vec<(LocalId, HirTypeId)>,
    pub ret_ty: HirTypeId,
    pub astptr: Option<MySyntaxNodePtr>,
}

pub type HirExprId = la_arena::Idx<HirExpr>;

#[derive(Debug, Clone)]
pub struct HirExpr {
    pub kind: HirExprKind,
    pub astptr: Option<MySyntaxNodePtr>,
}

#[derive(Debug, Clone)]
pub enum HirExprKind {
    LocalVar(LocalId),
    GlobalItem(ItemId),

    Unit,
    Bool(bool),
    Int(String),
    Float(f64),
    String(String),

    EnumCtor {
        ctor_id: CtorId,
        args: Vec<HirExprId>,
    },

    UnresolvedEnumCtor {
        name: String,
        args: Vec<HirExprId>,
    },

    StructLiteral {
        type_id: TypeId,
        fields: Vec<(FieldId, HirExprId)>,
    },

    Tuple(Vec<HirExprId>),
    Array(Vec<HirExprId>),

    Let {
        pat: HirPatId,
        value: HirExprId,
        annotation: Option<HirTypeId>,
    },

    Block {
        exprs: Vec<HirExprId>,
    },

    Closure {
        params: Vec<LocalId>,
        body: HirExprId,
    },

    Match {
        scrutinee: HirExprId,
        arms: Vec<HirArm>,
    },

    If {
        cond: HirExprId,
        then_branch: HirExprId,
        else_branch: HirExprId,
    },

    While {
        cond: HirExprId,
        body: HirExprId,
    },

    Go {
        expr: HirExprId,
    },

    Call {
        callee: HirExprId,
        args: Vec<HirExprId>,
    },

    MethodCall {
        recv: HirExprId,
        method_name: String,
        args: Vec<HirExprId>,
        explicit_trait: Option<TraitId>,
        explicit_type: Option<TypeId>,
    },

    Unary {
        op: ast::ast::UnaryOp,
        expr: HirExprId,
    },

    Binary {
        op: ast::ast::BinaryOp,
        lhs: HirExprId,
        rhs: HirExprId,
    },

    Projection {
        expr: HirExprId,
        index: usize,
    },

    Field {
        expr: HirExprId,
        field_name: String,
    },
}

pub type HirPatId = la_arena::Idx<HirPat>;

#[derive(Debug, Clone)]
pub struct HirPat {
    pub kind: HirPatKind,
    pub astptr: Option<MySyntaxNodePtr>,
}

#[derive(Debug, Clone)]
pub enum HirPatKind {
    Wild,
    Unit,
    Bool(bool),
    Int(String),
    String(String),

    Var(LocalId),

    Tuple(Vec<HirPatId>),

    Struct {
        type_id: TypeId,
        fields: Vec<(FieldId, HirPatId)>,
    },

    Enum {
        ctor_id: CtorId,
        args: Vec<HirPatId>,
    },

    UnresolvedVariant(String),
}

#[derive(Debug, Clone)]
pub struct HirArm {
    pub pat: HirPatId,
    pub body: HirExprId,
}
