use lexer::TokenKind;

#[allow(bad_style)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum MySyntaxKind {
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Eq,
    Semi,
    Comma,
    Colon,
    Arrow,
    FatArrow,
    Plus,
    Minus,
    Star,
    Slash,
    Dot,
    AndAnd,
    OrOr,
    Pipe,
    Bang,
    ExternKeyword,
    FnKeyword,
    TraitKeyword,
    ImplKeyword,
    ForKeyword,
    EnumKeyword,
    StructKeyword,
    TypeKeyword,
    MatchKeyword,
    IfKeyword,
    ElseKeyword,
    LetKeyword,
    InKeyword,
    ReturnKeyword,
    GoKeyword,
    WhileKeyword,
    TrueKeyword,
    FalseKeyword,
    WildcardKeyword,
    UnitKeyword,
    BoolKeyword,
    IntKeyword,
    Int8Keyword,
    Int16Keyword,
    Int32Keyword,
    Int64Keyword,
    StringKeyword,
    ArrayKeyword,
    Lident,
    Uident,
    Int,
    Str,
    Whitespace,
    Comment,

    Error,

    TombStone,
    ErrorTree,
    EXTERN,
    FN,
    ENUM,
    TRAIT,
    TRAIT_METHOD_SIG_LIST,
    TRAIT_METHOD_SIG,
    IMPL,
    STRUCT,
    STRUCT_FIELD_LIST,
    STRUCT_FIELD,
    TYPE_LIST,
    TYPE_UNIT,
    TYPE_BOOL,
    TYPE_INT,
    TYPE_INT8,
    TYPE_INT16,
    TYPE_INT32,
    TYPE_INT64,
    TYPE_STRING,
    TYPE_TUPLE,
    TYPE_TAPP,
    TYPE_ARRAY,
    TYPE_FUNC,
    EXPR_LITERAL,
    EXPR_UIDENT,
    EXPR_LIDENT,
    EXPR_PAREN,
    EXPR_BINARY,
    EXPR_PREFIX,
    EXPR_IF,
    EXPR_IF_COND,
    EXPR_IF_THEN,
    EXPR_IF_ELSE,
    EXPR_WHILE,
    EXPR_WHILE_COND,
    EXPR_WHILE_BODY,
    EXPR_MATCH,
    EXPR_GO,
    EXPR_UNIT,
    EXPR_BOOL,
    EXPR_INT,
    EXPR_STR,
    EXPR_ARRAY_LITERAL,
    EXPR_TUPLE,
    EXPR_CALL,
    EXPR_STRUCT_LITERAL,
    ARG_LIST,
    ARG,
    STRUCT_LITERAL_FIELD_LIST,
    STRUCT_LITERAL_FIELD,
    EXPR_CLOSURE,
    EXPR_CLOSURE_BODY,
    CLOSURE_PARAM_LIST,
    CLOSURE_PARAM,
    STMT_LET,
    STMT_EXPR,
    STRUCT_PATTERN_FIELD_LIST,
    STRUCT_PATTERN_FIELD,
    VARIANT_LIST,
    VARIANT,
    PATTERN,
    PATTERN_UNIT,
    PATTERN_BOOL,
    PATTERN_STRING,
    PATTERN_INT,
    PATTERN_TUPLE,
    PATTERN_CONSTR,
    PATTERN_VARIABLE,
    PATTERN_WILDCARD,
    MATCH_ARM_LIST,
    MATCH_ARM,
    PARAM,
    PARAM_LIST,
    GENERIC,
    GENERIC_LIST,
    TYPE_PARAM_LIST,
    BLOCK,
    FILE,
}

impl From<MySyntaxKind> for rowan::SyntaxKind {
    fn from(kind: MySyntaxKind) -> Self {
        Self(kind as u16)
    }
}

pub trait ToSyntaxKind {
    fn to_syntax_kind(self) -> rowan::SyntaxKind;
}

impl ToSyntaxKind for TokenKind {
    fn to_syntax_kind(self) -> rowan::SyntaxKind {
        rowan::SyntaxKind(self as u16)
    }
}

pub type MySyntaxNode = rowan::SyntaxNode<MyLang>;
pub type MySyntaxElement = rowan::SyntaxElement<MyLang>;
pub type MySyntaxToken = rowan::SyntaxToken<MyLang>;
pub type MySyntaxNodeChildren = rowan::SyntaxNodeChildren<MyLang>;
pub type MySyntaxElementChildren = rowan::SyntaxElementChildren<MyLang>;
pub type MySyntaxNodePtr = rowan::ast::SyntaxNodePtr<MyLang>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MyLang {}

impl rowan::Language for MyLang {
    type Kind = MySyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= MySyntaxKind::FILE as u16);
        unsafe { std::mem::transmute::<u16, MySyntaxKind>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}
