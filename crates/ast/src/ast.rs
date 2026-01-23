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
    TDyn {
        trait_path: Path,
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
    pub generic_bounds: Vec<(AstIdent, Vec<Path>)>,
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
    pub trait_name: Option<Path>,
    pub for_type: TypeExpr,
    pub methods: Vec<Fn>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    EPath {
        path: Path,
        astptr: MySyntaxNodePtr,
    },
    EUnit {
        astptr: MySyntaxNodePtr,
    },
    EBool {
        value: bool,
        astptr: MySyntaxNodePtr,
    },
    EInt {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    EInt8 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    EInt16 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    EInt32 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    EInt64 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    EUInt8 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    EUInt16 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    EUInt32 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    EUInt64 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    EFloat {
        value: f64,
        astptr: MySyntaxNodePtr,
    },
    EFloat32 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    EFloat64 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    EString {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    EConstr {
        constructor: Path,
        args: Vec<Expr>,
        astptr: MySyntaxNodePtr,
    },
    EStructLiteral {
        name: Path,
        fields: Vec<(AstIdent, Expr)>,
        astptr: MySyntaxNodePtr,
    },
    ETuple {
        items: Vec<Expr>,
        astptr: MySyntaxNodePtr,
    },
    EArray {
        items: Vec<Expr>,
        astptr: MySyntaxNodePtr,
    },
    ELet {
        pat: Pat,
        annotation: Option<TypeExpr>,
        value: Box<Expr>,
        astptr: MySyntaxNodePtr,
    },
    EClosure {
        params: Vec<ClosureParam>,
        body: Box<Expr>,
        astptr: MySyntaxNodePtr,
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
        astptr: MySyntaxNodePtr,
    },
    EWhile {
        cond: Box<Expr>,
        body: Box<Expr>,
        astptr: MySyntaxNodePtr,
    },
    EGo {
        expr: Box<Expr>,
        astptr: MySyntaxNodePtr,
    },
    ECall {
        func: Box<Expr>,
        args: Vec<Expr>,
        astptr: MySyntaxNodePtr,
    },
    EUnary {
        op: common_defs::UnaryOp,
        expr: Box<Expr>,
        astptr: MySyntaxNodePtr,
    },
    EBinary {
        op: common_defs::BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        astptr: MySyntaxNodePtr,
    },
    EProj {
        tuple: Box<Expr>,
        index: usize,
        astptr: MySyntaxNodePtr,
    },
    EField {
        expr: Box<Expr>,
        field: AstIdent,
        astptr: MySyntaxNodePtr,
    },
    EBlock {
        exprs: Vec<Expr>,
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
        name: AstIdent,
        astptr: MySyntaxNodePtr,
    },
    PUnit {
        astptr: MySyntaxNodePtr,
    },
    PBool {
        value: bool,
        astptr: MySyntaxNodePtr,
    },
    PInt {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    PInt8 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    PInt16 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    PInt32 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    PInt64 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    PUInt8 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    PUInt16 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    PUInt32 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    PUInt64 {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    PString {
        value: String,
        astptr: MySyntaxNodePtr,
    },
    PConstr {
        constructor: Path,
        args: Vec<Pat>,
        astptr: MySyntaxNodePtr,
    },
    PStruct {
        name: Path,
        fields: Vec<(AstIdent, Pat)>,
        astptr: MySyntaxNodePtr,
    },
    PTuple {
        pats: Vec<Pat>,
        astptr: MySyntaxNodePtr,
    },
    PWild {
        astptr: MySyntaxNodePtr,
    },
}
