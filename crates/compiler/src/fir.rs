use ast::ast;
use parser::syntax::MySyntaxNodePtr;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub String);

impl From<&ast::Ident> for Ident {
    fn from(ident: &ast::Ident) -> Self {
        Ident(ident.0.clone())
    }
}

impl From<ast::Ident> for Ident {
    fn from(ident: ast::Ident) -> Self {
        Ident(ident.0)
    }
}

impl Ident {
    pub fn new(name: &str) -> Self {
        Self(name.to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathSegment {
    pub ident: Ident,
}

impl PathSegment {
    pub fn new(ident: Ident) -> Self {
        Self { ident }
    }

    pub fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl From<&ast::PathSegment> for PathSegment {
    fn from(seg: &ast::PathSegment) -> Self {
        PathSegment {
            ident: (&seg.ident).into(),
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

    pub fn from_idents(idents: Vec<Ident>) -> Self {
        let segments = idents.into_iter().map(PathSegment::new).collect();
        Self { segments }
    }

    pub fn from_ident(ident: Ident) -> Self {
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

    pub fn last_ident(&self) -> Option<&Ident> {
        self.last().map(|segment| segment.ident())
    }

    pub fn namespace_segments(&self) -> &[PathSegment] {
        if self.segments.len() > 1 {
            &self.segments[..self.segments.len() - 1]
        } else {
            &[]
        }
    }

    pub fn parent_ident(&self) -> Option<&Ident> {
        if self.segments.len() > 1 {
            Some(self.segments[self.segments.len() - 2].ident())
        } else {
            None
        }
    }

    pub fn display(&self) -> String {
        self.segments
            .iter()
            .map(|segment| segment.ident.0.clone())
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    Eq,
    NotEq,
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
            Self::Less => "<",
            Self::Greater => ">",
            Self::LessEq => "<=",
            Self::GreaterEq => ">=",
            Self::Eq => "==",
            Self::NotEq => "!=",
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
            Self::Less => "less",
            Self::Greater => "greater",
            Self::LessEq => "less_eq",
            Self::GreaterEq => "greater_eq",
            Self::Eq => "eq",
            Self::NotEq => "not_eq",
        }
    }
}

impl From<ast::BinaryOp> for BinaryOp {
    fn from(op: ast::BinaryOp) -> Self {
        match op {
            ast::BinaryOp::Add => BinaryOp::Add,
            ast::BinaryOp::Sub => BinaryOp::Sub,
            ast::BinaryOp::Mul => BinaryOp::Mul,
            ast::BinaryOp::Div => BinaryOp::Div,
            ast::BinaryOp::And => BinaryOp::And,
            ast::BinaryOp::Or => BinaryOp::Or,
            ast::BinaryOp::Less => BinaryOp::Less,
            ast::BinaryOp::Greater => BinaryOp::Greater,
            ast::BinaryOp::LessEq => BinaryOp::LessEq,
            ast::BinaryOp::GreaterEq => BinaryOp::GreaterEq,
            ast::BinaryOp::Eq => BinaryOp::Eq,
            ast::BinaryOp::NotEq => BinaryOp::NotEq,
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

impl From<ast::UnaryOp> for UnaryOp {
    fn from(op: ast::UnaryOp) -> Self {
        match op {
            ast::UnaryOp::Neg => UnaryOp::Neg,
            ast::UnaryOp::Not => UnaryOp::Not,
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
    pub name: Ident,
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
    ExternBuiltin(ExternBuiltin),
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub generics: Vec<Ident>,
    pub params: Vec<(Ident, TypeExpr)>,
    pub ret_ty: Option<TypeExpr>,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct ExternGo {
    pub attrs: Vec<Attribute>,
    pub package_path: String,
    pub go_symbol: String,
    pub goml_name: Ident,
    pub explicit_go_symbol: bool,
    pub params: Vec<(Ident, TypeExpr)>,
    pub ret_ty: Option<TypeExpr>,
}

impl From<&ast::ExternGo> for ExternGo {
    fn from(ext: &ast::ExternGo) -> Self {
        ExternGo {
            attrs: ext.attrs.iter().map(|a| a.into()).collect(),
            package_path: ext.package_path.clone(),
            go_symbol: ext.go_symbol.clone(),
            goml_name: (&ext.goml_name).into(),
            explicit_go_symbol: ext.explicit_go_symbol,
            params: ext
                .params
                .iter()
                .map(|(i, t)| (i.into(), t.into()))
                .collect(),
            ret_ty: ext.ret_ty.as_ref().map(|t| t.into()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternType {
    pub attrs: Vec<Attribute>,
    pub goml_name: Ident,
}

impl From<&ast::ExternType> for ExternType {
    fn from(ext: &ast::ExternType) -> Self {
        ExternType {
            attrs: ext.attrs.iter().map(|a| a.into()).collect(),
            goml_name: (&ext.goml_name).into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternBuiltin {
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub params: Vec<(Ident, TypeExpr)>,
    pub ret_ty: Option<TypeExpr>,
}

impl From<&ast::ExternBuiltin> for ExternBuiltin {
    fn from(ext: &ast::ExternBuiltin) -> Self {
        ExternBuiltin {
            attrs: ext.attrs.iter().map(|a| a.into()).collect(),
            name: (&ext.name).into(),
            params: ext
                .params
                .iter()
                .map(|(i, t)| (i.into(), t.into()))
                .collect(),
            ret_ty: ext.ret_ty.as_ref().map(|t| t.into()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub generics: Vec<Ident>,
    pub variants: Vec<(Ident, Vec<TypeExpr>)>,
}

impl From<&ast::EnumDef> for EnumDef {
    fn from(e: &ast::EnumDef) -> Self {
        EnumDef {
            attrs: e.attrs.iter().map(|a| a.into()).collect(),
            name: (&e.name).into(),
            generics: e.generics.iter().map(|g| g.into()).collect(),
            variants: e
                .variants
                .iter()
                .map(|(i, tys)| (i.into(), tys.iter().map(|t| t.into()).collect()))
                .collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub generics: Vec<Ident>,
    pub fields: Vec<(Ident, TypeExpr)>,
}

impl From<&ast::StructDef> for StructDef {
    fn from(s: &ast::StructDef) -> Self {
        StructDef {
            attrs: s.attrs.iter().map(|a| a.into()).collect(),
            name: (&s.name).into(),
            generics: s.generics.iter().map(|g| g.into()).collect(),
            fields: s.fields.iter().map(|(i, t)| (i.into(), t.into())).collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TraitDef {
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub method_sigs: Vec<TraitMethodSignature>,
}

impl From<&ast::TraitDef> for TraitDef {
    fn from(t: &ast::TraitDef) -> Self {
        TraitDef {
            attrs: t.attrs.iter().map(|a| a.into()).collect(),
            name: (&t.name).into(),
            method_sigs: t.method_sigs.iter().map(|m| m.into()).collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TraitMethodSignature {
    pub name: Ident,
    pub params: Vec<TypeExpr>,
    pub ret_ty: TypeExpr,
}

impl From<&ast::TraitMethodSignature> for TraitMethodSignature {
    fn from(m: &ast::TraitMethodSignature) -> Self {
        TraitMethodSignature {
            name: (&m.name).into(),
            params: m.params.iter().map(|p| p.into()).collect(),
            ret_ty: (&m.ret_ty).into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImplBlock {
    pub attrs: Vec<Attribute>,
    pub generics: Vec<Ident>,
    pub trait_name: Option<Ident>,
    pub for_type: TypeExpr,
    pub methods: Vec<Fn>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    EPath {
        path: Path,
        astptr: MySyntaxNodePtr,
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
        annotation: Option<TypeExpr>,
        value: Box<Expr>,
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
    EGo {
        expr: Box<Expr>,
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
    EBlock {
        exprs: Vec<Expr>,
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
