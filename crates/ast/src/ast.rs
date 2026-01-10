use parser::syntax::MySyntaxNodePtr;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstIdent(pub String);

impl AstIdent {
    pub fn new(name: &str) -> Self {
        Self(name.to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathSegment {
    pub ident: AstIdent,
}

impl PathSegment {
    pub fn new(ident: AstIdent) -> Self {
        Self { ident }
    }

    pub fn ident(&self) -> &AstIdent {
        &self.ident
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

    pub fn from_idents(idents: Vec<AstIdent>) -> Self {
        let segments = idents.into_iter().map(PathSegment::new).collect();
        Self { segments }
    }

    pub fn from_ident(ident: AstIdent) -> Self {
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

    pub fn last_ident(&self) -> Option<&AstIdent> {
        self.last().map(|segment| segment.ident())
    }

    pub fn namespace_segments(&self) -> &[PathSegment] {
        if self.segments.len() > 1 {
            &self.segments[..self.segments.len() - 1]
        } else {
            &[]
        }
    }

    pub fn parent_ident(&self) -> Option<&AstIdent> {
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
        path: Path,
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

#[derive(Debug, Clone)]
pub struct ClosureParam {
    pub name: AstIdent,
    pub ty: Option<TypeExpr>,
    pub astptr: MySyntaxNodePtr,
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub ast: MySyntaxNodePtr,
    pub text: String,
}

#[derive(Debug, Clone)]
pub struct File {
    pub package: AstIdent,
    pub imports: Vec<AstIdent>,
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
    pub name: AstIdent,
    pub generics: Vec<AstIdent>,
    pub params: Vec<(AstIdent, TypeExpr)>,
    pub ret_ty: Option<TypeExpr>,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct ExternGo {
    pub attrs: Vec<Attribute>,
    pub package_path: String,
    pub go_symbol: String,
    pub goml_name: AstIdent,
    pub explicit_go_symbol: bool,
    pub params: Vec<(AstIdent, TypeExpr)>,
    pub ret_ty: Option<TypeExpr>,
}

#[derive(Debug, Clone)]
pub struct ExternType {
    pub attrs: Vec<Attribute>,
    pub goml_name: AstIdent,
}

#[derive(Debug, Clone)]
pub struct ExternBuiltin {
    pub attrs: Vec<Attribute>,
    pub name: AstIdent,
    pub params: Vec<(AstIdent, TypeExpr)>,
    pub ret_ty: Option<TypeExpr>,
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub attrs: Vec<Attribute>,
    pub name: AstIdent,
    pub generics: Vec<AstIdent>,
    pub variants: Vec<(AstIdent, Vec<TypeExpr>)>,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub attrs: Vec<Attribute>,
    pub name: AstIdent,
    pub generics: Vec<AstIdent>,
    pub fields: Vec<(AstIdent, TypeExpr)>,
}

#[derive(Debug, Clone)]
pub struct TraitDef {
    pub attrs: Vec<Attribute>,
    pub name: AstIdent,
    pub method_sigs: Vec<TraitMethodSignature>,
}

#[derive(Debug, Clone)]
pub struct TraitMethodSignature {
    pub name: AstIdent,
    pub params: Vec<TypeExpr>,
    pub ret_ty: TypeExpr,
}

#[derive(Debug, Clone)]
pub struct ImplBlock {
    pub attrs: Vec<Attribute>,
    pub generics: Vec<AstIdent>,
    pub trait_name: Option<AstIdent>,
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
        name: AstIdent,
        fields: Vec<(AstIdent, Expr)>,
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
        op: common_defs::UnaryOp,
        expr: Box<Expr>,
    },
    EBinary {
        op: common_defs::BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    EProj {
        tuple: Box<Expr>,
        index: usize,
    },
    EField {
        expr: Box<Expr>,
        field: AstIdent,
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
        name: AstIdent,
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
        name: AstIdent,
        fields: Vec<(AstIdent, Pat)>,
    },
    PTuple {
        pats: Vec<Pat>,
    },
    PWild,
}
