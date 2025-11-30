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
    TRef {
        elem: Box<TypeExpr>,
    },
    TFunc {
        params: Vec<TypeExpr>,
        ret_ty: Box<TypeExpr>,
    },
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

#[derive(Debug, Clone)]
pub struct ExternType {
    pub attrs: Vec<Attribute>,
    pub goml_name: Ident,
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub generics: Vec<Ident>,
    pub variants: Vec<(Ident, Vec<TypeExpr>)>,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub generics: Vec<Ident>,
    pub fields: Vec<(Ident, TypeExpr)>,
}

#[derive(Debug, Clone)]
pub struct TraitDef {
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub method_sigs: Vec<TraitMethodSignature>,
}

#[derive(Debug, Clone)]
pub struct TraitMethodSignature {
    pub name: Ident,
    pub params: Vec<TypeExpr>,
    pub ret_ty: TypeExpr,
}

#[derive(Debug, Clone)]
pub struct ImplBlock {
    pub attrs: Vec<Attribute>,
    pub trait_name: Option<Ident>,
    pub for_type: TypeExpr,
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
    ETypeMember {
        type_name: Ident,
        member: Ident,
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
