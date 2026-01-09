use ast::ast;
use la_arena::{Arena, Idx};
use parser::syntax::MySyntaxNodePtr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(pub u32);

impl DefId {
    pub fn index(self) -> u32 {
        self.0
    }

    pub fn to_debug_string(self) -> String {
        format!("def/{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(pub u32);

impl LocalId {
    pub fn index(self) -> u32 {
        self.0
    }

    pub fn to_debug_string(self) -> String {
        format!("local/{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(pub u32);

impl ExprId {
    pub fn index(self) -> u32 {
        self.0
    }

    pub fn to_debug_string(self) -> String {
        format!("expr/{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PatId(pub u32);

impl PatId {
    pub fn index(self) -> u32 {
        self.0
    }

    pub fn to_debug_string(self) -> String {
        format!("pat/{}", self.0)
    }
}

#[derive(Debug, Clone, Default)]
pub struct FirTable {
    local_hints: Vec<String>,
    defs: Vec<Def>,
    exprs: Arena<Expr>,
    pats: Arena<Pat>,
}

impl FirTable {
    pub fn new() -> Self {
        Self {
            local_hints: Vec::new(),
            defs: Vec::new(),
            exprs: Arena::new(),
            pats: Arena::new(),
        }
    }

    pub fn fresh_local(&mut self, hint: &str) -> LocalId {
        let id = self.local_hints.len() as u32;
        self.local_hints.push(hint.to_string());
        LocalId(id)
    }

    pub fn local_hint(&self, id: LocalId) -> &str {
        &self.local_hints[id.0 as usize]
    }

    pub fn local_ident_name(&self, id: LocalId) -> String {
        format!("{}/{}", self.local_hint(id), id.0)
    }

    pub fn alloc_def(&mut self, def: Def) -> DefId {
        let id = self.defs.len() as u32;
        self.defs.push(def);
        DefId(id)
    }

    pub fn def(&self, id: DefId) -> &Def {
        &self.defs[id.0 as usize]
    }

    pub fn def_mut(&mut self, id: DefId) -> &mut Def {
        &mut self.defs[id.0 as usize]
    }

    pub fn alloc_expr(&mut self, expr: Expr) -> ExprId {
        let idx = self.exprs.alloc(expr);
        ExprId(idx.into_raw().into_u32())
    }

    pub fn expr(&self, id: ExprId) -> &Expr {
        let idx = Idx::from_raw(la_arena::RawIdx::from_u32(id.0));
        &self.exprs[idx]
    }

    pub fn alloc_pat(&mut self, pat: Pat) -> PatId {
        let idx = self.pats.alloc(pat);
        PatId(idx.into_raw().into_u32())
    }

    pub fn pat(&self, id: PatId) -> &Pat {
        let idx = Idx::from_raw(la_arena::RawIdx::from_u32(id.0));
        &self.pats[idx]
    }
}

#[derive(Debug, Clone)]
pub enum FirIdent {
    Name(String),
    Fresh { id: u32, hint: String },
}

impl FirIdent {
    pub fn name(s: impl Into<String>) -> Self {
        FirIdent::Name(s.into())
    }

    pub fn fresh(id: u32, hint: impl Into<String>) -> Self {
        FirIdent::Fresh {
            id,
            hint: hint.into(),
        }
    }

    pub fn to_ident_name(&self) -> String {
        match self {
            FirIdent::Name(s) => s.clone(),
            FirIdent::Fresh { id, hint } => format!("{}/{}", hint, id),
        }
    }

    pub fn hint(&self) -> &str {
        match self {
            FirIdent::Name(s) => s,
            FirIdent::Fresh { hint, .. } => hint,
        }
    }
}

impl PartialEq for FirIdent {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (FirIdent::Name(a), FirIdent::Name(b)) => a == b,
            (FirIdent::Fresh { id: a, .. }, FirIdent::Fresh { id: b, .. }) => a == b,
            _ => false,
        }
    }
}

impl Eq for FirIdent {}

impl std::hash::Hash for FirIdent {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            FirIdent::Name(s) => {
                0u8.hash(state);
                s.hash(state);
            }
            FirIdent::Fresh { id, .. } => {
                1u8.hash(state);
                id.hash(state);
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinId {
    ArrayGet,
    ArraySet,
    Ref,
    RefGet,
    RefSet,
    VecNew,
    VecPush,
    VecGet,
    VecLen,
    Named(u32),
}

impl BuiltinId {
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "array_get" => Some(BuiltinId::ArrayGet),
            "array_set" => Some(BuiltinId::ArraySet),
            "ref" => Some(BuiltinId::Ref),
            "ref_get" => Some(BuiltinId::RefGet),
            "ref_set" => Some(BuiltinId::RefSet),
            "vec_new" => Some(BuiltinId::VecNew),
            "vec_push" => Some(BuiltinId::VecPush),
            "vec_get" => Some(BuiltinId::VecGet),
            "vec_len" => Some(BuiltinId::VecLen),
            _ => None,
        }
    }

    pub fn to_name(&self) -> String {
        match self {
            BuiltinId::ArrayGet => "array_get".to_string(),
            BuiltinId::ArraySet => "array_set".to_string(),
            BuiltinId::Ref => "ref".to_string(),
            BuiltinId::RefGet => "ref_get".to_string(),
            BuiltinId::RefSet => "ref_set".to_string(),
            BuiltinId::VecNew => "vec_new".to_string(),
            BuiltinId::VecPush => "vec_push".to_string(),
            BuiltinId::VecGet => "vec_get".to_string(),
            BuiltinId::VecLen => "vec_len".to_string(),
            BuiltinId::Named(id) => format!("builtin/{}", id),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NameRef {
    Local(LocalId),
    Def(DefId),
    Builtin(BuiltinId),
    Unresolved(Path),
}

impl NameRef {
    pub fn display(&self, fir_table: &FirTable) -> String {
        match self {
            NameRef::Local(id) => fir_table.local_ident_name(*id),
            NameRef::Def(id) => id.to_debug_string(),
            NameRef::Builtin(id) => id.to_name(),
            NameRef::Unresolved(path) => path.display(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathSegment {
    pub seg: String,
}

impl PathSegment {
    pub fn new(seg: String) -> Self {
        Self { seg }
    }

    pub fn seg(&self) -> &String {
        &self.seg
    }
}

impl From<&ast::PathSegment> for PathSegment {
    fn from(seg: &ast::PathSegment) -> Self {
        PathSegment {
            seg: seg.ident.0.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub segments: Vec<PathSegment>,
}

impl Path {
    pub fn new(segments: Vec<PathSegment>) -> Self {
        Self { segments }
    }

    pub fn from_idents(idents: Vec<String>) -> Self {
        let segments = idents.into_iter().map(PathSegment::new).collect();
        Self { segments }
    }

    pub fn from_ident(ident: String) -> Self {
        Self {
            segments: vec![PathSegment::new(ident)],
        }
    }

    pub fn segments(&self) -> &[PathSegment] {
        &self.segments
    }

    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn len(&self) -> usize {
        self.segments.len()
    }

    pub fn last(&self) -> Option<&PathSegment> {
        self.segments.last()
    }

    pub fn last_ident(&self) -> Option<&String> {
        self.last().map(|segment| segment.seg())
    }

    pub fn namespace_segments(&self) -> &[PathSegment] {
        if self.segments.len() > 1 {
            &self.segments[..self.segments.len() - 1]
        } else {
            &[]
        }
    }

    pub fn parent_ident(&self) -> Option<&String> {
        if self.segments.len() > 1 {
            Some(self.segments[self.segments.len() - 2].seg())
        } else {
            None
        }
    }

    pub fn display(&self) -> String {
        self.segments
            .iter()
            .map(|segment| segment.seg.clone())
            .collect::<Vec<_>>()
            .join("::")
    }
}

impl From<&ast::Path> for Path {
    fn from(path: &ast::Path) -> Self {
        Path {
            segments: path.segments.iter().map(|s| s.into()).collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
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
    TTuple {
        typs: Vec<TypeExpr>,
    },
    TCon {
        name: String,
    },
    TApp {
        ty: Box<TypeExpr>,
        args: Vec<TypeExpr>,
    },
    TArray {
        len: usize,
        elem: Box<TypeExpr>,
    },
    TFunc {
        params: Vec<TypeExpr>,
        ret_ty: Box<TypeExpr>,
    },
}

impl From<&ast::TypeExpr> for TypeExpr {
    fn from(ty: &ast::TypeExpr) -> Self {
        match ty {
            ast::TypeExpr::TUnit => TypeExpr::TUnit,
            ast::TypeExpr::TBool => TypeExpr::TBool,
            ast::TypeExpr::TInt8 => TypeExpr::TInt8,
            ast::TypeExpr::TInt16 => TypeExpr::TInt16,
            ast::TypeExpr::TInt32 => TypeExpr::TInt32,
            ast::TypeExpr::TInt64 => TypeExpr::TInt64,
            ast::TypeExpr::TUint8 => TypeExpr::TUint8,
            ast::TypeExpr::TUint16 => TypeExpr::TUint16,
            ast::TypeExpr::TUint32 => TypeExpr::TUint32,
            ast::TypeExpr::TUint64 => TypeExpr::TUint64,
            ast::TypeExpr::TFloat32 => TypeExpr::TFloat32,
            ast::TypeExpr::TFloat64 => TypeExpr::TFloat64,
            ast::TypeExpr::TString => TypeExpr::TString,
            ast::TypeExpr::TTuple { typs } => TypeExpr::TTuple {
                typs: typs.iter().map(|t| t.into()).collect(),
            },
            ast::TypeExpr::TCon { name } => TypeExpr::TCon { name: name.clone() },
            ast::TypeExpr::TApp { ty, args } => TypeExpr::TApp {
                ty: Box::new(ty.as_ref().into()),
                args: args.iter().map(|a| a.into()).collect(),
            },
            ast::TypeExpr::TArray { len, elem } => TypeExpr::TArray {
                len: *len,
                elem: Box::new(elem.as_ref().into()),
            },
            ast::TypeExpr::TFunc { params, ret_ty } => TypeExpr::TFunc {
                params: params.iter().map(|p| p.into()).collect(),
                ret_ty: Box::new(ret_ty.as_ref().into()),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct ClosureParam {
    pub name: LocalId,
    pub ty: Option<TypeExpr>,
    pub astptr: MySyntaxNodePtr,
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub ast: MySyntaxNodePtr,
    pub text: String,
}

impl From<&ast::Attribute> for Attribute {
    fn from(attr: &ast::Attribute) -> Self {
        Attribute {
            ast: attr.ast,
            text: attr.text.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct File {
    pub toplevels: Vec<DefId>,
}

#[derive(Debug, Clone)]
pub enum Def {
    EnumDef(EnumDef),
    StructDef(StructDef),
    TraitDef(TraitDef),
    ImplBlock(ImplBlock),
    Fn(Fn),
    ExternGo(ExternGo),
    ExternType(ExternType),
    ExternBuiltin(ExternBuiltin),
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub attrs: Vec<Attribute>,
    pub name: String,
    pub generics: Vec<FirIdent>,
    pub params: Vec<(LocalId, TypeExpr)>,
    pub ret_ty: Option<TypeExpr>,
    pub body: ExprId,
}

#[derive(Debug, Clone)]
pub struct ExternGo {
    pub attrs: Vec<Attribute>,
    pub package_path: String,
    pub go_symbol: String,
    pub goml_name: FirIdent,
    pub explicit_go_symbol: bool,
    pub params: Vec<(FirIdent, TypeExpr)>,
    pub ret_ty: Option<TypeExpr>,
}

impl From<&ast::ExternGo> for ExternGo {
    fn from(ext: &ast::ExternGo) -> Self {
        ExternGo {
            attrs: ext.attrs.iter().map(|a| a.into()).collect(),
            package_path: ext.package_path.clone(),
            go_symbol: ext.go_symbol.clone(),
            goml_name: FirIdent::name(&ext.goml_name.0),
            explicit_go_symbol: ext.explicit_go_symbol,
            params: ext
                .params
                .iter()
                .map(|(i, t)| (FirIdent::name(&i.0), t.into()))
                .collect(),
            ret_ty: ext.ret_ty.as_ref().map(|t| t.into()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternType {
    pub attrs: Vec<Attribute>,
    pub goml_name: FirIdent,
}

impl From<&ast::ExternType> for ExternType {
    fn from(ext: &ast::ExternType) -> Self {
        ExternType {
            attrs: ext.attrs.iter().map(|a| a.into()).collect(),
            goml_name: FirIdent::name(&ext.goml_name.0),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternBuiltin {
    pub attrs: Vec<Attribute>,
    pub name: FirIdent,
    pub params: Vec<(FirIdent, TypeExpr)>,
    pub ret_ty: Option<TypeExpr>,
}

impl From<&ast::ExternBuiltin> for ExternBuiltin {
    fn from(ext: &ast::ExternBuiltin) -> Self {
        ExternBuiltin {
            attrs: ext.attrs.iter().map(|a| a.into()).collect(),
            name: FirIdent::name(&ext.name.0),
            params: ext
                .params
                .iter()
                .map(|(i, t)| (FirIdent::name(&i.0), t.into()))
                .collect(),
            ret_ty: ext.ret_ty.as_ref().map(|t| t.into()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub attrs: Vec<Attribute>,
    pub name: FirIdent,
    pub generics: Vec<FirIdent>,
    pub variants: Vec<(FirIdent, Vec<TypeExpr>)>,
}

impl From<&ast::EnumDef> for EnumDef {
    fn from(e: &ast::EnumDef) -> Self {
        EnumDef {
            attrs: e.attrs.iter().map(|a| a.into()).collect(),
            name: FirIdent::name(&e.name.0),
            generics: e.generics.iter().map(|g| FirIdent::name(&g.0)).collect(),
            variants: e
                .variants
                .iter()
                .map(|(i, tys)| (FirIdent::name(&i.0), tys.iter().map(|t| t.into()).collect()))
                .collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub attrs: Vec<Attribute>,
    pub name: FirIdent,
    pub generics: Vec<FirIdent>,
    pub fields: Vec<(FirIdent, TypeExpr)>,
}

impl From<&ast::StructDef> for StructDef {
    fn from(s: &ast::StructDef) -> Self {
        StructDef {
            attrs: s.attrs.iter().map(|a| a.into()).collect(),
            name: FirIdent::name(&s.name.0),
            generics: s.generics.iter().map(|g| FirIdent::name(&g.0)).collect(),
            fields: s
                .fields
                .iter()
                .map(|(i, t)| (FirIdent::name(&i.0), t.into()))
                .collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TraitDef {
    pub attrs: Vec<Attribute>,
    pub name: FirIdent,
    pub method_sigs: Vec<TraitMethodSignature>,
}

impl From<&ast::TraitDef> for TraitDef {
    fn from(t: &ast::TraitDef) -> Self {
        TraitDef {
            attrs: t.attrs.iter().map(|a| a.into()).collect(),
            name: FirIdent::name(&t.name.0),
            method_sigs: t.method_sigs.iter().map(|m| m.into()).collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TraitMethodSignature {
    pub name: FirIdent,
    pub params: Vec<TypeExpr>,
    pub ret_ty: TypeExpr,
}

impl From<&ast::TraitMethodSignature> for TraitMethodSignature {
    fn from(m: &ast::TraitMethodSignature) -> Self {
        TraitMethodSignature {
            name: FirIdent::name(&m.name.0),
            params: m.params.iter().map(|p| p.into()).collect(),
            ret_ty: (&m.ret_ty).into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImplBlock {
    pub attrs: Vec<Attribute>,
    pub generics: Vec<FirIdent>,
    pub trait_name: Option<FirIdent>,
    pub for_type: TypeExpr,
    pub methods: Vec<DefId>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    ENameRef {
        res: NameRef,
        hint: String,
        astptr: Option<MySyntaxNodePtr>,
    },
    EUnit,
    EBool {
        value: bool,
    },
    EInt {
        value: String,
    },
    EInt8 {
        value: String,
    },
    EInt16 {
        value: String,
    },
    EInt32 {
        value: String,
    },
    EInt64 {
        value: String,
    },
    EUInt8 {
        value: String,
    },
    EUInt16 {
        value: String,
    },
    EUInt32 {
        value: String,
    },
    EUInt64 {
        value: String,
    },
    EFloat {
        value: f64,
    },
    EFloat32 {
        value: String,
    },
    EFloat64 {
        value: String,
    },
    EString {
        value: String,
    },
    EConstr {
        constructor: Path,
        args: Vec<ExprId>,
    },
    EStructLiteral {
        name: FirIdent,
        fields: Vec<(FirIdent, ExprId)>,
    },
    ETuple {
        items: Vec<ExprId>,
    },
    EArray {
        items: Vec<ExprId>,
    },
    ELet {
        pat: PatId,
        annotation: Option<TypeExpr>,
        value: ExprId,
    },
    EClosure {
        params: Vec<ClosureParam>,
        body: ExprId,
    },
    EMatch {
        expr: ExprId,
        arms: Vec<Arm>,
    },
    EIf {
        cond: ExprId,
        then_branch: ExprId,
        else_branch: ExprId,
    },
    EWhile {
        cond: ExprId,
        body: ExprId,
    },
    EGo {
        expr: ExprId,
    },
    ECall {
        func: ExprId,
        args: Vec<ExprId>,
    },
    EUnary {
        op: common_defs::UnaryOp,
        expr: ExprId,
    },
    EBinary {
        op: common_defs::BinaryOp,
        lhs: ExprId,
        rhs: ExprId,
    },
    EProj {
        tuple: ExprId,
        index: usize,
    },
    EField {
        expr: ExprId,
        field: FirIdent,
    },
    EBlock {
        exprs: Vec<ExprId>,
    },
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub pat: PatId,
    pub body: ExprId,
}

#[derive(Debug, Clone)]
pub enum Pat {
    PVar {
        name: LocalId,
        astptr: MySyntaxNodePtr,
    },
    PUnit,
    PBool {
        value: bool,
    },
    PInt {
        value: String,
    },
    PInt8 {
        value: String,
    },
    PInt16 {
        value: String,
    },
    PInt32 {
        value: String,
    },
    PInt64 {
        value: String,
    },
    PUInt8 {
        value: String,
    },
    PUInt16 {
        value: String,
    },
    PUInt32 {
        value: String,
    },
    PUInt64 {
        value: String,
    },
    PString {
        value: String,
    },
    PConstr {
        constructor: Path,
        args: Vec<PatId>,
    },
    PStruct {
        name: FirIdent,
        fields: Vec<(FirIdent, PatId)>,
    },
    PTuple {
        pats: Vec<PatId>,
    },
    PWild,
}
