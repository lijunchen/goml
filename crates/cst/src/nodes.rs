use parser::syntax::{MySyntaxKind, MySyntaxNode, MySyntaxToken};

use crate::{
    cst::{CstChildren, CstNode},
    support,
};
use MySyntaxKind::*;

macro_rules! impl_cst_node_simple {
    ($node:ident, $syntax_kind:expr) => {
        impl CstNode for $node {
            fn can_cast(kind: MySyntaxKind) -> bool {
                kind == $syntax_kind
            }
            fn cast(syntax: MySyntaxNode) -> Option<Self> {
                if Self::can_cast(syntax.kind()) {
                    Some(Self { syntax })
                } else {
                    None
                }
            }
            fn syntax(&self) -> &MySyntaxNode {
                &self.syntax
            }
        }
    };
}

macro_rules! impl_display_via_syntax {
    ($node:ident) => {
        impl std::fmt::Display for $node {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Display::fmt(self.syntax(), f)
            }
        }
    };
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct File {
    pub(crate) syntax: MySyntaxNode,
}

impl File {
    pub fn items(&self) -> CstChildren<Item> {
        support::children(&self.syntax)
    }

    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(File, MySyntaxKind::FILE);
impl_display_via_syntax!(File);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Item {
    Enum(Enum),
    Struct(Struct),
    Trait(Trait),
    Impl(Impl),
    Fn(Fn),
    Extern(Extern),
}

impl CstNode for Item {
    fn can_cast(kind: MySyntaxKind) -> bool {
        matches!(
            kind,
            MySyntaxKind::ENUM
                | MySyntaxKind::STRUCT
                | MySyntaxKind::TRAIT
                | MySyntaxKind::IMPL
                | MySyntaxKind::FN
                | MySyntaxKind::EXTERN
        )
    }
    fn cast(syntax: MySyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            ENUM => Item::Enum(Enum { syntax }),
            STRUCT => Item::Struct(Struct { syntax }),
            TRAIT => Item::Trait(Trait { syntax }),
            IMPL => Item::Impl(Impl { syntax }),
            FN => Item::Fn(Fn { syntax }),
            EXTERN => Item::Extern(Extern { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &MySyntaxNode {
        match self {
            Item::Enum(it) => &it.syntax,
            Item::Struct(it) => &it.syntax,
            Item::Trait(it) => &it.syntax,
            Item::Impl(it) => &it.syntax,
            Item::Fn(it) => &it.syntax,
            Item::Extern(it) => &it.syntax,
        }
    }
}

impl_display_via_syntax!(Item);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Enum {
    pub(crate) syntax: MySyntaxNode,
}

impl Enum {
    pub fn uident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }

    pub fn generic_list(&self) -> Option<GenericList> {
        support::child(&self.syntax)
    }

    pub fn variant_list(&self) -> Option<VariantList> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(Enum, MySyntaxKind::ENUM);
impl_display_via_syntax!(Enum);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub(crate) syntax: MySyntaxNode,
}

impl Struct {
    pub fn uident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }

    pub fn generic_list(&self) -> Option<GenericList> {
        support::child(&self.syntax)
    }

    pub fn field_list(&self) -> Option<StructFieldList> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(Struct, MySyntaxKind::STRUCT);
impl_display_via_syntax!(Struct);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Trait {
    pub(crate) syntax: MySyntaxNode,
}

impl Trait {
    pub fn uident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }

    pub fn trait_method_list(&self) -> Option<TraitMethodList> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(Trait, MySyntaxKind::TRAIT);
impl_display_via_syntax!(Trait);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitMethodList {
    pub(crate) syntax: MySyntaxNode,
}

impl TraitMethodList {
    pub fn methods(&self) -> CstChildren<TraitMethod> {
        support::children(&self.syntax)
    }
}

impl_cst_node_simple!(TraitMethodList, MySyntaxKind::TRAIT_METHOD_SIG_LIST);
impl_display_via_syntax!(TraitMethodList);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitMethod {
    pub(crate) syntax: MySyntaxNode,
}

impl TraitMethod {
    pub fn lident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }

    pub fn type_list(&self) -> Option<TypeList> {
        support::child(&self.syntax)
    }

    pub fn return_type(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(TraitMethod, MySyntaxKind::TRAIT_METHOD_SIG);
impl_display_via_syntax!(TraitMethod);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Impl {
    pub(crate) syntax: MySyntaxNode,
}

impl Impl {
    pub fn uident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }

    pub fn for_type(&self) -> Option<Type> {
        support::child(&self.syntax)
    }

    pub fn functions(&self) -> CstChildren<Fn> {
        support::children(&self.syntax)
    }
}

impl_cst_node_simple!(Impl, MySyntaxKind::IMPL);
impl_display_via_syntax!(Impl);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Generic {
    pub(crate) syntax: MySyntaxNode,
}

impl Generic {
    pub fn uident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }
}
impl_cst_node_simple!(Generic, MySyntaxKind::GENERIC);
impl_display_via_syntax!(Generic);

////////////////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericList {
    pub(crate) syntax: MySyntaxNode,
}

impl GenericList {
    pub fn generics(&self) -> CstChildren<Generic> {
        support::children(&self.syntax)
    }
}

impl_cst_node_simple!(GenericList, MySyntaxKind::GENERIC_LIST);
impl_display_via_syntax!(GenericList);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeParamList {
    pub(crate) syntax: MySyntaxNode,
}

impl TypeParamList {
    pub fn types(&self) -> CstChildren<Type> {
        support::children(&self.syntax)
    }
}

impl_cst_node_simple!(TypeParamList, MySyntaxKind::TYPE_PARAM_LIST);
impl_display_via_syntax!(TypeParamList);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variant {
    pub(crate) syntax: MySyntaxNode,
}

impl Variant {
    pub fn uident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }

    pub fn type_list(&self) -> Option<TypeList> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(Variant, MySyntaxKind::VARIANT);
impl_display_via_syntax!(Variant);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariantList {
    pub(crate) syntax: MySyntaxNode,
}

impl VariantList {
    pub fn variants(&self) -> CstChildren<Variant> {
        support::children(&self.syntax)
    }
}

impl_cst_node_simple!(VariantList, MySyntaxKind::VARIANT_LIST);
impl_display_via_syntax!(VariantList);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructFieldList {
    pub(crate) syntax: MySyntaxNode,
}

impl StructFieldList {
    pub fn fields(&self) -> CstChildren<StructField> {
        support::children(&self.syntax)
    }
}

impl_cst_node_simple!(StructFieldList, MySyntaxKind::STRUCT_FIELD_LIST);
impl_display_via_syntax!(StructFieldList);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub(crate) syntax: MySyntaxNode,
}

impl StructField {
    pub fn lident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }

    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(StructField, MySyntaxKind::STRUCT_FIELD);
impl_display_via_syntax!(StructField);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Fn {
    pub(crate) syntax: MySyntaxNode,
}

impl Fn {
    pub fn lident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }

    pub fn generic_list(&self) -> Option<GenericList> {
        support::child(&self.syntax)
    }

    pub fn param_list(&self) -> Option<ParamList> {
        support::child(&self.syntax)
    }

    pub fn return_type(&self) -> Option<Type> {
        support::child(&self.syntax)
    }

    pub fn block(&self) -> Option<Block> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(Fn, MySyntaxKind::FN);
impl_display_via_syntax!(Fn);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Extern {
    pub(crate) syntax: MySyntaxNode,
}

impl Extern {
    pub fn lang(&self) -> Option<MySyntaxToken> {
        let mut strings = self
            .syntax
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .filter(|tok| tok.kind() == MySyntaxKind::Str);
        strings.next()
    }

    pub fn package(&self) -> Option<MySyntaxToken> {
        let mut strings = self
            .syntax
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .filter(|tok| tok.kind() == MySyntaxKind::Str);
        strings.next()?;
        strings.next()
    }

    pub fn symbol(&self) -> Option<MySyntaxToken> {
        let mut strings = self
            .syntax
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .filter(|tok| tok.kind() == MySyntaxKind::Str);
        strings.next()?;
        strings.next()?;
        strings.next()
    }

    pub fn lident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }

    pub fn type_keyword(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::TypeKeyword)
    }

    pub fn uident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }

    pub fn param_list(&self) -> Option<ParamList> {
        support::child(&self.syntax)
    }

    pub fn return_type(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(Extern, MySyntaxKind::EXTERN);
impl_display_via_syntax!(Extern);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParamList {
    pub(crate) syntax: MySyntaxNode,
}

impl ParamList {
    pub fn params(&self) -> CstChildren<Param> {
        support::children(&self.syntax)
    }
}

impl_cst_node_simple!(ParamList, MySyntaxKind::PARAM_LIST);
impl_display_via_syntax!(ParamList);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub(crate) syntax: MySyntaxNode,
}

impl Param {
    pub fn lident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }

    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(Param, MySyntaxKind::PARAM);
impl_display_via_syntax!(Param);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub(crate) syntax: MySyntaxNode,
}

impl Block {
    pub fn stmts(&self) -> CstChildren<Stmt> {
        support::children(&self.syntax)
    }

    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(Block, MySyntaxKind::BLOCK);
impl_display_via_syntax!(Block);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    LetStmt(LetStmt),
    ExprStmt(ExprStmt),
}

impl CstNode for Stmt {
    fn can_cast(kind: MySyntaxKind) -> bool {
        matches!(kind, MySyntaxKind::STMT_LET | MySyntaxKind::STMT_EXPR)
    }

    fn cast(syntax: MySyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            MySyntaxKind::STMT_LET => Stmt::LetStmt(LetStmt { syntax }),
            MySyntaxKind::STMT_EXPR => Stmt::ExprStmt(ExprStmt { syntax }),
            _ => return None,
        };
        Some(res)
    }

    fn syntax(&self) -> &MySyntaxNode {
        match self {
            Stmt::LetStmt(it) => &it.syntax,
            Stmt::ExprStmt(it) => &it.syntax,
        }
    }
}

impl_display_via_syntax!(Stmt);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LetStmt {
    pub(crate) syntax: MySyntaxNode,
}

impl LetStmt {
    pub fn pattern(&self) -> Option<Pattern> {
        support::child(&self.syntax)
    }

    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }

    pub fn value(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(LetStmt, MySyntaxKind::STMT_LET);
impl_display_via_syntax!(LetStmt);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprStmt {
    pub(crate) syntax: MySyntaxNode,
}

impl ExprStmt {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(ExprStmt, MySyntaxKind::STMT_EXPR);
impl_display_via_syntax!(ExprStmt);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    UnitExpr(UnitExpr),
    BoolExpr(BoolExpr),
    IntExpr(IntExpr),
    FloatExpr(FloatExpr),
    StrExpr(StrExpr),
    CallExpr(CallExpr),
    StructLiteralExpr(StructLiteralExpr),
    ArrayLiteralExpr(ArrayLiteralExpr),
    MatchExpr(MatchExpr),
    IfExpr(IfExpr),
    WhileExpr(WhileExpr),
    IdentExpr(IdentExpr),
    TupleExpr(TupleExpr),
    BinaryExpr(BinaryExpr),
    PrefixExpr(PrefixExpr),
    ClosureExpr(ClosureExpr),
    GoExpr(GoExpr),
}

impl CstNode for Expr {
    fn can_cast(kind: MySyntaxKind) -> bool {
        matches!(
            kind,
            EXPR_UNIT
                | EXPR_BOOL
                | EXPR_INT
                | EXPR_FLOAT
                | EXPR_STR
                | EXPR_CALL
                | EXPR_STRUCT_LITERAL
                | EXPR_ARRAY_LITERAL
                | EXPR_MATCH
                | EXPR_IDENT
                | EXPR_TUPLE
                | EXPR_BINARY
                | EXPR_PREFIX
                | EXPR_CLOSURE
                | EXPR_WHILE
                | EXPR_GO
        )
    }
    fn cast(syntax: MySyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            EXPR_UNIT => Expr::UnitExpr(UnitExpr { syntax }),
            EXPR_BOOL => Expr::BoolExpr(BoolExpr { syntax }),
            EXPR_INT => Expr::IntExpr(IntExpr { syntax }),
            EXPR_FLOAT => Expr::FloatExpr(FloatExpr { syntax }),
            EXPR_STR => Expr::StrExpr(StrExpr { syntax }),
            EXPR_CALL => Expr::CallExpr(CallExpr { syntax }),
            EXPR_STRUCT_LITERAL => Expr::StructLiteralExpr(StructLiteralExpr { syntax }),
            EXPR_MATCH => Expr::MatchExpr(MatchExpr { syntax }),
            EXPR_IF => Expr::IfExpr(IfExpr { syntax }),
            EXPR_WHILE => Expr::WhileExpr(WhileExpr { syntax }),
            EXPR_IDENT => Expr::IdentExpr(IdentExpr { syntax }),
            EXPR_TUPLE => Expr::TupleExpr(TupleExpr { syntax }),
            EXPR_BINARY => Expr::BinaryExpr(BinaryExpr { syntax }),
            EXPR_PREFIX => Expr::PrefixExpr(PrefixExpr { syntax }),
            EXPR_ARRAY_LITERAL => Expr::ArrayLiteralExpr(ArrayLiteralExpr { syntax }),
            EXPR_CLOSURE => Expr::ClosureExpr(ClosureExpr { syntax }),
            EXPR_GO => Expr::GoExpr(GoExpr { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &MySyntaxNode {
        match self {
            Self::UnitExpr(it) => &it.syntax,
            Self::BoolExpr(it) => &it.syntax,
            Self::IntExpr(it) => &it.syntax,
            Self::FloatExpr(it) => &it.syntax,
            Self::StrExpr(it) => &it.syntax,
            Self::CallExpr(it) => &it.syntax,
            Self::StructLiteralExpr(it) => &it.syntax,
            Self::ArrayLiteralExpr(it) => &it.syntax,
            Self::MatchExpr(it) => &it.syntax,
            Self::IfExpr(it) => &it.syntax,
            Self::WhileExpr(it) => &it.syntax,
            Self::IdentExpr(it) => &it.syntax,
            Self::TupleExpr(it) => &it.syntax,
            Self::BinaryExpr(it) => &it.syntax,
            Self::PrefixExpr(it) => &it.syntax,
            Self::ClosureExpr(it) => &it.syntax,
            Self::GoExpr(it) => &it.syntax,
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnitExpr {
    pub(crate) syntax: MySyntaxNode,
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BoolExpr {
    pub(crate) syntax: MySyntaxNode,
}

impl BoolExpr {
    pub fn value(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::TrueKeyword)
            .or_else(|| support::token(&self.syntax, MySyntaxKind::FalseKeyword))
    }
}

impl_cst_node_simple!(BoolExpr, MySyntaxKind::EXPR_BOOL);
impl_display_via_syntax!(BoolExpr);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntExpr {
    pub(crate) syntax: MySyntaxNode,
}

impl IntExpr {
    pub fn value(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Int)
    }
}

impl_cst_node_simple!(IntExpr, MySyntaxKind::EXPR_INT);
impl_display_via_syntax!(IntExpr);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FloatExpr {
    pub(crate) syntax: MySyntaxNode,
}

impl FloatExpr {
    pub fn value(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Float)
    }
}

impl_cst_node_simple!(FloatExpr, MySyntaxKind::EXPR_FLOAT);
impl_display_via_syntax!(FloatExpr);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StrExpr {
    pub(crate) syntax: MySyntaxNode,
}

impl StrExpr {
    pub fn value(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Str)
    }
}

impl_cst_node_simple!(StrExpr, MySyntaxKind::EXPR_STR);
impl_display_via_syntax!(StrExpr);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructLiteralExpr {
    pub(crate) syntax: MySyntaxNode,
}

impl StructLiteralExpr {
    pub fn uident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }

    pub fn field_list(&self) -> Option<StructLiteralFieldList> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(StructLiteralExpr, MySyntaxKind::EXPR_STRUCT_LITERAL);
impl_display_via_syntax!(StructLiteralExpr);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayLiteralExpr {
    pub(crate) syntax: MySyntaxNode,
}

impl ArrayLiteralExpr {
    pub fn exprs(&self) -> CstChildren<Expr> {
        support::children(&self.syntax)
    }
}

impl_cst_node_simple!(ArrayLiteralExpr, MySyntaxKind::EXPR_ARRAY_LITERAL);
impl_display_via_syntax!(ArrayLiteralExpr);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructLiteralFieldList {
    pub(crate) syntax: MySyntaxNode,
}

impl StructLiteralFieldList {
    pub fn fields(&self) -> CstChildren<StructLiteralField> {
        support::children(&self.syntax)
    }
}

impl_cst_node_simple!(
    StructLiteralFieldList,
    MySyntaxKind::STRUCT_LITERAL_FIELD_LIST
);
impl_display_via_syntax!(StructLiteralFieldList);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructLiteralField {
    pub(crate) syntax: MySyntaxNode,
}

impl StructLiteralField {
    pub fn lident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }

    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(StructLiteralField, MySyntaxKind::STRUCT_LITERAL_FIELD);
impl_display_via_syntax!(StructLiteralField);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfExpr {
    pub(crate) syntax: MySyntaxNode,
}

impl IfExpr {
    pub fn cond(&self) -> Option<IfExprCond> {
        support::child(&self.syntax)
    }

    pub fn then_branch(&self) -> Option<IfExprThen> {
        support::child(&self.syntax)
    }

    pub fn else_branch(&self) -> Option<IfExprElse> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(IfExpr, MySyntaxKind::EXPR_IF);
impl_display_via_syntax!(IfExpr);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfExprCond {
    pub(crate) syntax: MySyntaxNode,
}

impl IfExprCond {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(IfExprCond, MySyntaxKind::EXPR_IF_COND);
impl_display_via_syntax!(IfExprCond);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfExprThen {
    pub(crate) syntax: MySyntaxNode,
}

impl IfExprThen {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    pub fn block(&self) -> Option<Block> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(IfExprThen, MySyntaxKind::EXPR_IF_THEN);
impl_display_via_syntax!(IfExprThen);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfExprElse {
    pub(crate) syntax: MySyntaxNode,
}

impl IfExprElse {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    pub fn block(&self) -> Option<Block> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(IfExprElse, MySyntaxKind::EXPR_IF_ELSE);
impl_display_via_syntax!(IfExprElse);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhileExpr {
    pub(crate) syntax: MySyntaxNode,
}

impl WhileExpr {
    pub fn cond(&self) -> Option<WhileExprCond> {
        support::child(&self.syntax)
    }

    pub fn body(&self) -> Option<WhileExprBody> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(WhileExpr, MySyntaxKind::EXPR_WHILE);
impl_display_via_syntax!(WhileExpr);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhileExprCond {
    pub(crate) syntax: MySyntaxNode,
}

impl WhileExprCond {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(WhileExprCond, MySyntaxKind::EXPR_WHILE_COND);
impl_display_via_syntax!(WhileExprCond);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhileExprBody {
    pub(crate) syntax: MySyntaxNode,
}

impl WhileExprBody {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    pub fn block(&self) -> Option<Block> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(WhileExprBody, MySyntaxKind::EXPR_WHILE_BODY);
impl_display_via_syntax!(WhileExprBody);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MatchExpr {
    pub(crate) syntax: MySyntaxNode,
}

impl MatchExpr {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    pub fn match_arm_list(&self) -> Option<MatchArmList> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(MatchExpr, MySyntaxKind::EXPR_MATCH);
impl_display_via_syntax!(MatchExpr);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MatchArmList {
    pub(crate) syntax: MySyntaxNode,
}

impl MatchArmList {
    pub fn arms(&self) -> CstChildren<MatchArm> {
        support::children(&self.syntax)
    }
}

impl_cst_node_simple!(MatchArmList, MySyntaxKind::MATCH_ARM_LIST);
impl_display_via_syntax!(MatchArmList);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MatchArm {
    pub(crate) syntax: MySyntaxNode,
}

impl MatchArm {
    pub fn pattern(&self) -> Option<Pattern> {
        support::child(&self.syntax)
    }

    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(MatchArm, MySyntaxKind::MATCH_ARM);
impl_display_via_syntax!(MatchArm);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallExpr {
    pub(crate) syntax: MySyntaxNode,
}

impl CallExpr {
    pub fn l_name(&self) -> Option<String> {
        let node: Option<Expr> = support::child(&self.syntax);
        match node {
            Some(Expr::IdentExpr(it)) => it.ident_token().map(|t| t.text().to_string()),
            _ => None,
        }
    }

    pub fn u_name(&self) -> Option<String> {
        let node: Option<Expr> = support::child(&self.syntax);
        match node {
            Some(Expr::IdentExpr(it)) => it.ident_token().map(|t| t.text().to_string()),
            _ => None,
        }
    }

    pub fn arg_list(&self) -> Option<ArgList> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(CallExpr, MySyntaxKind::EXPR_CALL);
impl_display_via_syntax!(CallExpr);

////////////////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArgList {
    pub(crate) syntax: MySyntaxNode,
}

impl ArgList {
    pub fn args(&self) -> CstChildren<Arg> {
        support::children(&self.syntax)
    }
}

impl_cst_node_simple!(ArgList, MySyntaxKind::ARG_LIST);
impl_display_via_syntax!(ArgList);

////////////////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Arg {
    pub(crate) syntax: MySyntaxNode,
}

impl Arg {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(Arg, MySyntaxKind::ARG);
impl_display_via_syntax!(Arg);

////////////////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IdentExpr {
    pub(crate) syntax: MySyntaxNode,
}

impl IdentExpr {
    pub fn ident_token(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }
}

impl_cst_node_simple!(IdentExpr, MySyntaxKind::EXPR_IDENT);
impl_display_via_syntax!(IdentExpr);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TupleExpr {
    pub(crate) syntax: MySyntaxNode,
}

impl TupleExpr {
    pub fn exprs(&self) -> CstChildren<Expr> {
        support::children(&self.syntax)
    }
}

impl_cst_node_simple!(TupleExpr, MySyntaxKind::EXPR_TUPLE);
impl_display_via_syntax!(TupleExpr);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinaryExpr {
    pub(crate) syntax: MySyntaxNode,
}

impl BinaryExpr {
    pub fn exprs(&self) -> CstChildren<Expr> {
        support::children(&self.syntax)
    }

    pub fn op(&self) -> Option<MySyntaxToken> {
        self.syntax.children_with_tokens().find_map(|element| {
            element.into_token().and_then(|token| {
                if matches!(
                    token.kind(),
                    MySyntaxKind::Plus
                        | MySyntaxKind::Minus
                        | MySyntaxKind::Star
                        | MySyntaxKind::Slash
                        | MySyntaxKind::AndAnd
                        | MySyntaxKind::OrOr
                        | MySyntaxKind::Dot
                ) {
                    Some(token)
                } else {
                    None
                }
            })
        })
    }
}

impl_cst_node_simple!(BinaryExpr, MySyntaxKind::EXPR_BINARY);
impl_display_via_syntax!(BinaryExpr);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PrefixExpr {
    pub(crate) syntax: MySyntaxNode,
}

impl PrefixExpr {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    pub fn op(&self) -> Option<MySyntaxToken> {
        self.syntax.children_with_tokens().find_map(|element| {
            element.into_token().and_then(|token| {
                if matches!(token.kind(), MySyntaxKind::Minus | MySyntaxKind::Bang) {
                    Some(token)
                } else {
                    None
                }
            })
        })
    }
}

impl_cst_node_simple!(PrefixExpr, MySyntaxKind::EXPR_PREFIX);
impl_display_via_syntax!(PrefixExpr);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClosureExpr {
    pub(crate) syntax: MySyntaxNode,
}

impl ClosureExpr {
    pub fn params(&self) -> Option<ClosureParamList> {
        support::child(&self.syntax)
    }

    pub fn body(&self) -> Option<ClosureBody> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(ClosureExpr, MySyntaxKind::EXPR_CLOSURE);
impl_display_via_syntax!(ClosureExpr);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GoExpr {
    pub(crate) syntax: MySyntaxNode,
}

impl GoExpr {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(GoExpr, MySyntaxKind::EXPR_GO);
impl_display_via_syntax!(GoExpr);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClosureParamList {
    pub(crate) syntax: MySyntaxNode,
}

impl ClosureParamList {
    pub fn params(&self) -> CstChildren<ClosureParam> {
        support::children(&self.syntax)
    }
}

impl_cst_node_simple!(ClosureParamList, MySyntaxKind::CLOSURE_PARAM_LIST);
impl_display_via_syntax!(ClosureParamList);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClosureParam {
    pub(crate) syntax: MySyntaxNode,
}

impl ClosureParam {
    pub fn lident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }

    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(ClosureParam, MySyntaxKind::CLOSURE_PARAM);
impl_display_via_syntax!(ClosureParam);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClosureBody {
    pub(crate) syntax: MySyntaxNode,
}

impl ClosureBody {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    pub fn block(&self) -> Option<Block> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(ClosureBody, MySyntaxKind::EXPR_CLOSURE_BODY);
impl_display_via_syntax!(ClosureBody);

////////////////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    VarPat(VarPat),
    UnitPat(UnitPat),
    BoolPat(BoolPat),
    StringPat(StringPat),
    IntPat(IntPat),
    ConstrPat(ConstrPat),
    TuplePat(TuplePat),
    WildPat(WildPat),
}

impl CstNode for Pattern {
    fn can_cast(kind: MySyntaxKind) -> bool {
        matches!(
            kind,
            PATTERN_VARIABLE
                | PATTERN_UNIT
                | PATTERN_BOOL
                | PATTERN_STRING
                | PATTERN_INT
                | PATTERN_CONSTR
                | PATTERN_TUPLE
                | PATTERN_WILDCARD
        )
    }
    fn cast(syntax: MySyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            PATTERN_VARIABLE => Pattern::VarPat(VarPat { syntax }),
            PATTERN_UNIT => Pattern::UnitPat(UnitPat { syntax }),
            PATTERN_BOOL => Pattern::BoolPat(BoolPat { syntax }),
            PATTERN_STRING => Pattern::StringPat(StringPat { syntax }),
            PATTERN_INT => Pattern::IntPat(IntPat { syntax }),
            PATTERN_CONSTR => Pattern::ConstrPat(ConstrPat { syntax }),
            PATTERN_TUPLE => Pattern::TuplePat(TuplePat { syntax }),
            PATTERN_WILDCARD => Pattern::WildPat(WildPat { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &MySyntaxNode {
        match self {
            Self::VarPat(it) => &it.syntax,
            Self::UnitPat(it) => &it.syntax,
            Self::BoolPat(it) => &it.syntax,
            Self::StringPat(it) => &it.syntax,
            Self::IntPat(it) => &it.syntax,
            Self::ConstrPat(it) => &it.syntax,
            Self::TuplePat(it) => &it.syntax,
            Self::WildPat(it) => &it.syntax,
        }
    }
}

impl_display_via_syntax!(Pattern);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarPat {
    pub(crate) syntax: MySyntaxNode,
}

impl VarPat {
    pub fn lident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }
}

impl_cst_node_simple!(VarPat, MySyntaxKind::PATTERN_VARIABLE);
impl_display_via_syntax!(VarPat);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnitPat {
    pub(crate) syntax: MySyntaxNode,
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BoolPat {
    pub(crate) syntax: MySyntaxNode,
}

impl BoolPat {
    pub fn value(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::TrueKeyword)
            .or_else(|| support::token(&self.syntax, MySyntaxKind::FalseKeyword))
    }
}

impl_cst_node_simple!(BoolPat, MySyntaxKind::PATTERN_BOOL);
impl_display_via_syntax!(BoolPat);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringPat {
    pub(crate) syntax: MySyntaxNode,
}

impl StringPat {
    pub fn value(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Str)
    }
}

impl_cst_node_simple!(StringPat, MySyntaxKind::PATTERN_STRING);
impl_display_via_syntax!(StringPat);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntPat {
    pub(crate) syntax: MySyntaxNode,
}

impl IntPat {
    pub fn value(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Int)
    }
}

impl_cst_node_simple!(IntPat, MySyntaxKind::PATTERN_INT);
impl_display_via_syntax!(IntPat);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstrPat {
    pub(crate) syntax: MySyntaxNode,
}

impl ConstrPat {
    pub fn uident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }

    pub fn patterns(&self) -> CstChildren<Pattern> {
        support::children(&self.syntax)
    }

    pub fn field_list(&self) -> Option<StructPatternFieldList> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(ConstrPat, MySyntaxKind::PATTERN_CONSTR);
impl_display_via_syntax!(ConstrPat);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TuplePat {
    pub(crate) syntax: MySyntaxNode,
}

impl TuplePat {
    pub fn patterns(&self) -> CstChildren<Pattern> {
        support::children(&self.syntax)
    }
}

impl_cst_node_simple!(TuplePat, MySyntaxKind::PATTERN_TUPLE);
impl_display_via_syntax!(TuplePat);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructPatternFieldList {
    pub(crate) syntax: MySyntaxNode,
}

impl StructPatternFieldList {
    pub fn fields(&self) -> CstChildren<StructPatternField> {
        support::children(&self.syntax)
    }
}

impl_cst_node_simple!(
    StructPatternFieldList,
    MySyntaxKind::STRUCT_PATTERN_FIELD_LIST
);
impl_display_via_syntax!(StructPatternFieldList);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructPatternField {
    pub(crate) syntax: MySyntaxNode,
}

impl StructPatternField {
    pub fn lident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }

    pub fn pattern(&self) -> Option<Pattern> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(StructPatternField, MySyntaxKind::STRUCT_PATTERN_FIELD);
impl_display_via_syntax!(StructPatternField);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WildPat {
    pub(crate) syntax: MySyntaxNode,
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    UnitTy(UnitTy),
    BoolTy(BoolTy),
    Int8Ty(Int8Ty),
    Int16Ty(Int16Ty),
    Int32Ty(Int32Ty),
    Int64Ty(Int64Ty),
    Uint8Ty(Uint8Ty),
    Uint16Ty(Uint16Ty),
    Uint32Ty(Uint32Ty),
    Uint64Ty(Uint64Ty),
    Float32Ty(Float32Ty),
    Float64Ty(Float64Ty),
    StringTy(StringTy),
    TupleTy(TupleTy),
    TAppTy(TAppTy),
    ArrayTy(ArrayTy),
    FuncTy(FuncTy),
}

impl CstNode for Type {
    fn can_cast(kind: MySyntaxKind) -> bool {
        matches!(
            kind,
            TYPE_UNIT
                | TYPE_BOOL
                | TYPE_INT8
                | TYPE_INT16
                | TYPE_INT32
                | TYPE_INT64
                | TYPE_UINT8
                | TYPE_UINT16
                | TYPE_UINT32
                | TYPE_UINT64
                | TYPE_FLOAT32
                | TYPE_FLOAT64
                | TYPE_STRING
                | TYPE_TUPLE
                | TYPE_TAPP
                | TYPE_ARRAY
                | TYPE_FUNC
        )
    }
    fn cast(syntax: MySyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            TYPE_UNIT => Type::UnitTy(UnitTy { syntax }),
            TYPE_BOOL => Type::BoolTy(BoolTy { syntax }),
            TYPE_INT8 => Type::Int8Ty(Int8Ty { syntax }),
            TYPE_INT16 => Type::Int16Ty(Int16Ty { syntax }),
            TYPE_INT32 => Type::Int32Ty(Int32Ty { syntax }),
            TYPE_INT64 => Type::Int64Ty(Int64Ty { syntax }),
            TYPE_UINT8 => Type::Uint8Ty(Uint8Ty { syntax }),
            TYPE_UINT16 => Type::Uint16Ty(Uint16Ty { syntax }),
            TYPE_UINT32 => Type::Uint32Ty(Uint32Ty { syntax }),
            TYPE_UINT64 => Type::Uint64Ty(Uint64Ty { syntax }),
            TYPE_FLOAT32 => Type::Float32Ty(Float32Ty { syntax }),
            TYPE_FLOAT64 => Type::Float64Ty(Float64Ty { syntax }),
            TYPE_STRING => Type::StringTy(StringTy { syntax }),
            TYPE_TUPLE => Type::TupleTy(TupleTy { syntax }),
            TYPE_TAPP => Type::TAppTy(TAppTy { syntax }),
            TYPE_ARRAY => Type::ArrayTy(ArrayTy { syntax }),
            TYPE_FUNC => Type::FuncTy(FuncTy { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &MySyntaxNode {
        match self {
            Type::UnitTy(it) => &it.syntax,
            Type::BoolTy(it) => &it.syntax,
            Type::Int8Ty(it) => &it.syntax,
            Type::Int16Ty(it) => &it.syntax,
            Type::Int32Ty(it) => &it.syntax,
            Type::Int64Ty(it) => &it.syntax,
            Type::Uint8Ty(it) => &it.syntax,
            Type::Uint16Ty(it) => &it.syntax,
            Type::Uint32Ty(it) => &it.syntax,
            Type::Uint64Ty(it) => &it.syntax,
            Type::Float32Ty(it) => &it.syntax,
            Type::Float64Ty(it) => &it.syntax,
            Type::StringTy(it) => &it.syntax,
            Type::TupleTy(it) => &it.syntax,
            Type::TAppTy(it) => &it.syntax,
            Type::ArrayTy(it) => &it.syntax,
            Type::FuncTy(it) => &it.syntax,
        }
    }
}

impl_display_via_syntax!(Type);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnitTy {
    pub(crate) syntax: MySyntaxNode,
}

impl UnitTy {}

impl_cst_node_simple!(UnitTy, MySyntaxKind::TYPE_UNIT);
impl_display_via_syntax!(UnitTy);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BoolTy {
    pub(crate) syntax: MySyntaxNode,
}

impl BoolTy {}

impl_cst_node_simple!(BoolTy, MySyntaxKind::TYPE_BOOL);
impl_display_via_syntax!(BoolTy);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Int8Ty {
    pub(crate) syntax: MySyntaxNode,
}

impl Int8Ty {}

impl_cst_node_simple!(Int8Ty, MySyntaxKind::TYPE_INT8);
impl_display_via_syntax!(Int8Ty);
////////////////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Int16Ty {
    pub(crate) syntax: MySyntaxNode,
}

impl Int16Ty {}

impl_cst_node_simple!(Int16Ty, MySyntaxKind::TYPE_INT16);
impl_display_via_syntax!(Int16Ty);
////////////////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Int32Ty {
    pub(crate) syntax: MySyntaxNode,
}

impl Int32Ty {}

impl_cst_node_simple!(Int32Ty, MySyntaxKind::TYPE_INT32);
impl_display_via_syntax!(Int32Ty);
////////////////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Int64Ty {
    pub(crate) syntax: MySyntaxNode,
}

impl Int64Ty {}

impl_cst_node_simple!(Int64Ty, MySyntaxKind::TYPE_INT64);
impl_display_via_syntax!(Int64Ty);
////////////////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Uint8Ty {
    pub(crate) syntax: MySyntaxNode,
}

impl Uint8Ty {}

impl_cst_node_simple!(Uint8Ty, MySyntaxKind::TYPE_UINT8);
impl_display_via_syntax!(Uint8Ty);
////////////////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Uint16Ty {
    pub(crate) syntax: MySyntaxNode,
}

impl Uint16Ty {}

impl_cst_node_simple!(Uint16Ty, MySyntaxKind::TYPE_UINT16);
impl_display_via_syntax!(Uint16Ty);
////////////////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Uint32Ty {
    pub(crate) syntax: MySyntaxNode,
}

impl Uint32Ty {}

impl_cst_node_simple!(Uint32Ty, MySyntaxKind::TYPE_UINT32);
impl_display_via_syntax!(Uint32Ty);
////////////////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Uint64Ty {
    pub(crate) syntax: MySyntaxNode,
}

impl Uint64Ty {}

impl_cst_node_simple!(Uint64Ty, MySyntaxKind::TYPE_UINT64);
impl_display_via_syntax!(Uint64Ty);
////////////////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Float32Ty {
    pub(crate) syntax: MySyntaxNode,
}

impl Float32Ty {}

impl_cst_node_simple!(Float32Ty, MySyntaxKind::TYPE_FLOAT32);
impl_display_via_syntax!(Float32Ty);
////////////////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Float64Ty {
    pub(crate) syntax: MySyntaxNode,
}

impl Float64Ty {}

impl_cst_node_simple!(Float64Ty, MySyntaxKind::TYPE_FLOAT64);
impl_display_via_syntax!(Float64Ty);
////////////////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringTy {
    pub(crate) syntax: MySyntaxNode,
}

impl StringTy {}

impl_cst_node_simple!(StringTy, MySyntaxKind::TYPE_STRING);
impl_display_via_syntax!(StringTy);
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TupleTy {
    pub(crate) syntax: MySyntaxNode,
}

////////////////////////////////////////////////////////////////////////////////

impl TupleTy {
    pub fn type_list(&self) -> Option<TypeList> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(TupleTy, MySyntaxKind::TYPE_TUPLE);
impl_display_via_syntax!(TupleTy);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TAppTy {
    pub(crate) syntax: MySyntaxNode,
}

impl TAppTy {
    pub fn uident(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Ident)
    }

    pub fn type_param_list(&self) -> Option<TypeParamList> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(TAppTy, MySyntaxKind::TYPE_TAPP);
impl_display_via_syntax!(TAppTy);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayTy {
    pub(crate) syntax: MySyntaxNode,
}

impl ArrayTy {
    pub fn len(&self) -> Option<MySyntaxToken> {
        support::token(&self.syntax, MySyntaxKind::Int)
    }

    pub fn elem_type(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
}

impl_cst_node_simple!(ArrayTy, MySyntaxKind::TYPE_ARRAY);
impl_display_via_syntax!(ArrayTy);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncTy {
    pub(crate) syntax: MySyntaxNode,
}

impl FuncTy {
    pub fn types(&self) -> CstChildren<Type> {
        support::children(&self.syntax)
    }
}

impl_cst_node_simple!(FuncTy, MySyntaxKind::TYPE_FUNC);
impl_display_via_syntax!(FuncTy);

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeList {
    pub(crate) syntax: MySyntaxNode,
}

impl TypeList {
    pub fn types(&self) -> CstChildren<Type> {
        support::children(&self.syntax)
    }
}

impl_cst_node_simple!(TypeList, MySyntaxKind::TYPE_LIST);
impl_display_via_syntax!(TypeList);
