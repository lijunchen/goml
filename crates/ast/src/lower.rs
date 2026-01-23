use crate::ast;

use ::cst::cst::CstNode;
use ::cst::{cst, support};
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use parser::syntax::{MySyntaxKind, MySyntaxNodePtr};
use std::collections::HashSet;
use text_size::TextRange;

pub struct LowerResult {
    ast: Option<ast::File>,
    diagnostics: Diagnostics,
}

impl LowerResult {
    pub fn has_errors(&self) -> bool {
        self.diagnostics.has_errors()
    }

    pub fn diagnostics(&self) -> &Diagnostics {
        &self.diagnostics
    }

    pub fn into_diagnostics(self) -> Diagnostics {
        self.diagnostics
    }

    pub fn ast(&self) -> Option<&ast::File> {
        self.ast.as_ref()
    }

    pub fn into_ast(self) -> Option<ast::File> {
        self.ast
    }

    pub fn into_parts(self) -> (Option<ast::File>, Diagnostics) {
        (self.ast, self.diagnostics)
    }

    pub fn into_result(self) -> Result<ast::File, Diagnostics> {
        if self.diagnostics.has_errors() {
            Err(self.diagnostics)
        } else if let Some(ast) = self.ast {
            Ok(ast)
        } else {
            Err(self.diagnostics)
        }
    }
}

struct LowerCtx {
    stage: Stage,
    diagnostics: Diagnostics,
    constructor_names: HashSet<String>,
}

impl LowerCtx {
    fn new(file: &cst::File) -> Self {
        let constructor_names = collect_constructor_names(file);
        Self {
            stage: Stage::other("lower"),
            diagnostics: Diagnostics::new(),
            constructor_names,
        }
    }

    fn push_error(&mut self, range: Option<TextRange>, message: impl Into<String>) {
        let diagnostic =
            Diagnostic::new(self.stage.clone(), Severity::Error, message).with_range(range);
        self.diagnostics.push(diagnostic);
    }

    fn has_errors(&self) -> bool {
        self.diagnostics.has_errors()
    }

    fn into_diagnostics(self) -> Diagnostics {
        self.diagnostics
    }
    fn is_constructor(&self, ident: &ast::AstIdent) -> bool {
        self.constructor_names.contains(&ident.0)
    }
}

fn collect_constructor_names(file: &cst::File) -> HashSet<String> {
    let mut constructor_names = HashSet::new();

    for item in file.items() {
        match item {
            cst::Item::Enum(enum_node) => {
                if let Some(list) = enum_node.variant_list() {
                    for variant in list.variants() {
                        if let Some(token) = variant.uident() {
                            constructor_names.insert(token.to_string());
                        }
                    }
                }
            }
            cst::Item::Struct(struct_node) => {
                if let Some(token) = struct_node.uident() {
                    constructor_names.insert(token.to_string());
                }
            }
            _ => {}
        }
    }

    constructor_names
}

pub fn lower(node: cst::File) -> LowerResult {
    let mut ctx = LowerCtx::new(&node);
    let package = node
        .package_decl()
        .and_then(|decl| decl.name_token())
        .map(|token| ast::AstIdent::new(&token.to_string()))
        .unwrap_or_else(|| ast::AstIdent::new("Main"));
    let imports = node
        .import_decls()
        .filter_map(|decl| decl.name_token())
        .map(|token| ast::AstIdent::new(&token.to_string()))
        .collect();
    let items = node
        .items()
        .flat_map(|item| lower_item(&mut ctx, item))
        .collect();
    let ast = if ctx.has_errors() {
        None
    } else {
        Some(ast::File {
            package,
            imports,
            toplevels: items,
        })
    };

    LowerResult {
        ast,
        diagnostics: ctx.into_diagnostics(),
    }
}

fn lower_attributes(list: Option<cst::AttributeList>) -> Vec<ast::Attribute> {
    list.map(|list| {
        list.attributes()
            .map(|attr| {
                let syntax = attr.syntax();
                ast::Attribute {
                    ast: MySyntaxNodePtr::new(syntax),
                    text: syntax.text().to_string(),
                }
            })
            .collect()
    })
    .unwrap_or_default()
}

fn attribute_path(attr: &ast::Attribute) -> Option<&str> {
    let trimmed = attr.text.trim();
    let inner = trimmed.strip_prefix("#[")?.strip_suffix(']')?.trim();
    let name_part = match inner.find('(') {
        Some(idx) => inner[..idx].trim(),
        None => inner,
    };
    if name_part.is_empty() {
        None
    } else {
        Some(name_part)
    }
}

fn find_attribute<'a>(attrs: &'a [ast::Attribute], target: &str) -> Option<&'a ast::Attribute> {
    attrs.iter().find(|attr| {
        attribute_path(attr)
            .and_then(|path| path.split("::").last())
            .map(|segment| segment == target)
            .unwrap_or(false)
    })
}

fn lower_item(ctx: &mut LowerCtx, node: cst::Item) -> Option<ast::Item> {
    match node {
        cst::Item::Enum(it) => Some(ast::Item::EnumDef(lower_enum(ctx, it)?)),
        cst::Item::Struct(it) => Some(ast::Item::StructDef(lower_struct(ctx, it)?)),
        cst::Item::Trait(it) => Some(ast::Item::TraitDef(lower_trait(ctx, it)?)),
        cst::Item::Impl(it) => Some(ast::Item::ImplBlock(lower_impl_block(ctx, it)?)),
        cst::Item::Fn(it) => Some(ast::Item::Fn(lower_fn(ctx, it)?)),
        cst::Item::Extern(it) => lower_extern(ctx, it),
    }
}

fn lower_enum(ctx: &mut LowerCtx, node: cst::Enum) -> Option<ast::EnumDef> {
    let attrs = lower_attributes(node.attributes());
    let name = node.uident().unwrap().to_string();
    let generics: Vec<ast::AstIdent> = node
        .generic_list()
        .map(|list| {
            list.generics()
                .flat_map(|x| {
                    let name = x.uident().unwrap().to_string();
                    Some(ast::AstIdent::new(&name))
                })
                .collect()
        })
        .unwrap_or_default();
    let variants = if let Some(list) = node.variant_list() {
        list.variants()
            .flat_map(|variant| lower_variant(ctx, variant))
            .collect()
    } else {
        ctx.push_error(
            Some(node.syntax().text_range()),
            format!("Enum {} has no variants", name),
        );
        Vec::new()
    };
    Some(ast::EnumDef {
        attrs,
        name: ast::AstIdent::new(&name),
        generics,
        variants,
    })
}

fn lower_struct(ctx: &mut LowerCtx, node: cst::Struct) -> Option<ast::StructDef> {
    let attrs = lower_attributes(node.attributes());
    let name = node.uident()?.to_string();
    let generics: Vec<ast::AstIdent> = node
        .generic_list()
        .map(|list| {
            list.generics()
                .flat_map(|generic| {
                    let name = generic.uident()?.to_string();
                    Some(ast::AstIdent::new(&name))
                })
                .collect()
        })
        .unwrap_or_default();

    let fields = node
        .field_list()
        .map(|list| {
            list.fields()
                .flat_map(|field| lower_struct_field(ctx, field))
                .collect()
        })
        .unwrap_or_default();

    Some(ast::StructDef {
        attrs,
        name: ast::AstIdent::new(&name),
        generics,
        fields,
    })
}

fn lower_struct_field(
    ctx: &mut LowerCtx,
    node: cst::StructField,
) -> Option<(ast::AstIdent, ast::TypeExpr)> {
    let name = node.lident()?.to_string();
    let ty = node.ty().and_then(|ty| lower_ty(ctx, ty))?;
    Some((ast::AstIdent(name), ty))
}

fn lower_trait(ctx: &mut LowerCtx, node: cst::Trait) -> Option<ast::TraitDef> {
    let attrs = lower_attributes(node.attributes());
    let name = node.uident().unwrap().to_string();
    let methods = if let Some(list) = node.trait_method_list() {
        list.methods()
            .flat_map(|method| lower_trait_method(ctx, method))
            .collect()
    } else {
        ctx.push_error(
            Some(node.syntax().text_range()),
            format!("Trait {} has no methods", name),
        );
        Vec::new()
    };
    Some(ast::TraitDef {
        attrs,
        name: ast::AstIdent::new(&name),
        method_sigs: methods,
    })
}

fn lower_trait_method(
    ctx: &mut LowerCtx,
    node: cst::TraitMethod,
) -> Option<ast::TraitMethodSignature> {
    let name = node.lident().unwrap().to_string();
    let params = if let Some(list) = node.type_list() {
        list.types().flat_map(|ty| lower_ty(ctx, ty)).collect()
    } else {
        ctx.push_error(
            Some(node.syntax().text_range()),
            format!("TraitMethod {} has no params", name),
        );
        Vec::new()
    };
    let ret_ty = match node.return_type() {
        None => ast::TypeExpr::TUnit,
        Some(it) => match lower_ty(ctx, it) {
            Some(ty) => ty,
            None => {
                ctx.push_error(
                    Some(node.syntax().text_range()),
                    format!("TraitMethod {} has no return type", name),
                );
                return None;
            }
        },
    };
    Some(ast::TraitMethodSignature {
        name: ast::AstIdent(name),
        params,
        ret_ty,
    })
}

fn lower_impl_block(ctx: &mut LowerCtx, node: cst::Impl) -> Option<ast::ImplBlock> {
    let attrs = lower_attributes(node.attributes());
    let generics: Vec<ast::AstIdent> = node
        .generic_list()
        .map(|list| {
            list.generics()
                .flat_map(|x| {
                    let name = x.uident().unwrap().to_string();
                    Some(ast::AstIdent::new(&name))
                })
                .collect()
        })
        .unwrap_or_default();
    let trait_name = node.trait_path().and_then(|path| lower_path(ctx, &path));
    let for_type = match node.for_type().and_then(|ty| lower_ty(ctx, ty)) {
        Some(ty) => ty,
        None => {
            ctx.push_error(
                Some(node.syntax().text_range()),
                "Impl block is missing a target type".to_string(),
            );
            return None;
        }
    };
    let methods: Vec<ast::Fn> = node
        .functions()
        .flat_map(|function| lower_fn(ctx, function))
        .collect();
    Some(ast::ImplBlock {
        attrs,
        generics,
        trait_name,
        for_type,
        methods,
    })
}

fn lower_variant(
    ctx: &mut LowerCtx,
    node: cst::Variant,
) -> Option<(ast::AstIdent, Vec<ast::TypeExpr>)> {
    let name = node.uident().unwrap().to_string();
    let typs = match node.type_list() {
        None => vec![],
        Some(xs) => xs.types().flat_map(|ty| lower_ty(ctx, ty)).collect(),
    };
    Some((ast::AstIdent::new(&name), typs))
}

fn lower_ty(ctx: &mut LowerCtx, node: cst::Type) -> Option<ast::TypeExpr> {
    match node {
        cst::Type::UnitTy(_) => Some(ast::TypeExpr::TUnit),
        cst::Type::BoolTy(_) => Some(ast::TypeExpr::TBool),
        cst::Type::Int8Ty(_) => Some(ast::TypeExpr::TInt8),
        cst::Type::Int16Ty(_) => Some(ast::TypeExpr::TInt16),
        cst::Type::Int32Ty(_) => Some(ast::TypeExpr::TInt32),
        cst::Type::Int64Ty(_) => Some(ast::TypeExpr::TInt64),
        cst::Type::Uint8Ty(_) => Some(ast::TypeExpr::TUint8),
        cst::Type::Uint16Ty(_) => Some(ast::TypeExpr::TUint16),
        cst::Type::Uint32Ty(_) => Some(ast::TypeExpr::TUint32),
        cst::Type::Uint64Ty(_) => Some(ast::TypeExpr::TUint64),
        cst::Type::Float32Ty(_) => Some(ast::TypeExpr::TFloat32),
        cst::Type::Float64Ty(_) => Some(ast::TypeExpr::TFloat64),
        cst::Type::StringTy(_) => Some(ast::TypeExpr::TString),
        cst::Type::TupleTy(it) => {
            let typs = it
                .type_list()?
                .types()
                .flat_map(|ty| lower_ty(ctx, ty))
                .collect();
            Some(ast::TypeExpr::TTuple { typs })
        }
        cst::Type::TAppTy(it) => {
            let path = it.path().and_then(|path| lower_path(ctx, &path))?;
            let args: Vec<ast::TypeExpr> = it
                .type_param_list()
                .map(|list| list.types().flat_map(|ty| lower_ty(ctx, ty)).collect())
                .unwrap_or_default();

            let base = ast::TypeExpr::TCon { path };

            if args.is_empty() {
                Some(base)
            } else {
                Some(ast::TypeExpr::TApp {
                    ty: Box::new(base),
                    args,
                })
            }
        }
        cst::Type::DynTy(it) => {
            let trait_path = it.path().and_then(|path| lower_path(ctx, &path))?;
            Some(ast::TypeExpr::TDyn { trait_path })
        }
        cst::Type::ArrayTy(it) => {
            let len_token = match it.len() {
                Some(token) => token,
                None => {
                    ctx.push_error(Some(it.syntax().text_range()), "Array type missing length");
                    return None;
                }
            };
            let len_text = len_token.text().to_string();
            let len = match len_text.parse::<usize>() {
                Ok(value) => value,
                Err(_) => {
                    ctx.push_error(
                        Some(len_token.text_range()),
                        format!("Invalid array length: {}", len_text),
                    );
                    return None;
                }
            };

            let elem_ty = match it.elem_type().and_then(|ty| lower_ty(ctx, ty)) {
                Some(ty) => ty,
                None => {
                    ctx.push_error(
                        Some(it.syntax().text_range()),
                        "Array type missing element type",
                    );
                    return None;
                }
            };

            Some(ast::TypeExpr::TArray {
                len,
                elem: Box::new(elem_ty),
            })
        }

        cst::Type::FuncTy(it) => {
            let mut types = it.types();
            let Some(param_node) = types.next() else {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Function type missing parameter type",
                );
                return None;
            };
            let Some(ret_node) = types.next() else {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Function type missing return type",
                );
                return None;
            };

            let param_ty = lower_ty(ctx, param_node)?;
            let ret_ty = lower_ty(ctx, ret_node)?;

            let params = match param_ty {
                ast::TypeExpr::TTuple { typs } => typs,
                other => vec![other],
            };

            Some(ast::TypeExpr::TFunc {
                params,
                ret_ty: Box::new(ret_ty),
            })
        }
    }
}

fn lower_fn(ctx: &mut LowerCtx, node: cst::Fn) -> Option<ast::Fn> {
    let attrs = lower_attributes(node.attributes());
    let name = node.lident().unwrap().to_string();
    let (generics, generic_bounds): (Vec<ast::AstIdent>, Vec<(ast::AstIdent, Vec<ast::Path>)>) =
        node.generic_list()
            .map(|list| {
                let mut generics = Vec::new();
                let mut bounds = Vec::new();
                for generic in list.generics() {
                    let Some(token) = generic.uident() else {
                        continue;
                    };
                    let name = token.to_string();
                    let ident = ast::AstIdent::new(&name);
                    generics.push(ident.clone());

                    if let Some(trait_set) = generic.trait_set() {
                        let traits = trait_set
                            .traits()
                            .flat_map(|path| lower_path(ctx, &path))
                            .collect::<Vec<_>>();
                        bounds.push((ident, traits));
                    }
                }
                (generics, bounds)
            })
            .unwrap_or_default();
    let params = match node.param_list() {
        Some(list) => list
            .params()
            .flat_map(|param| lower_param(ctx, param))
            .collect(),
        None => {
            ctx.push_error(
                Some(node.syntax().text_range()),
                format!("Fn {} has no params", name),
            );
            return None;
        }
    };
    let ret_ty = node.return_type().and_then(|ty| lower_ty(ctx, ty));
    let body = match node.block().and_then(|block| lower_block(ctx, block)) {
        Some(body) => body,
        None => {
            ctx.push_error(
                Some(node.syntax().text_range()),
                format!("Fn {} has no body", name),
            );
            return None;
        }
    };
    Some(ast::Fn {
        attrs,
        name: ast::AstIdent(name),
        generics,
        generic_bounds,
        params,
        ret_ty,
        body,
    })
}

fn lower_extern(ctx: &mut LowerCtx, node: cst::Extern) -> Option<ast::Item> {
    let attrs = lower_attributes(node.attributes());

    if let Some(attr) = find_attribute(&attrs, "builtin") {
        if node.type_keyword().is_some() {
            ctx.push_error(
                Some(attr.ast.text_range()),
                "Builtin extern declarations cannot declare types",
            );
            return None;
        }

        if let Some(lang_token) = node.lang() {
            ctx.push_error(
                Some(lang_token.text_range()),
                "Builtin extern declarations should not specify a language string",
            );
        }

        let Some(name_token) = node.lident() else {
            ctx.push_error(
                Some(node.syntax().text_range()),
                "Extern builtin declaration is missing function name",
            );
            return None;
        };
        let name = name_token.to_string();
        let params = node
            .param_list()
            .map(|list| {
                list.params()
                    .flat_map(|param| lower_param(ctx, param))
                    .collect()
            })
            .unwrap_or_default();
        let ret_ty = node.return_type().and_then(|ty| lower_ty(ctx, ty));
        return Some(ast::Item::ExternBuiltin(ast::ExternBuiltin {
            attrs,
            name: ast::AstIdent(name),
            params,
            ret_ty,
        }));
    }

    if node.type_keyword().is_some() && node.lang().is_none() {
        let Some(name_token) = node.uident() else {
            ctx.push_error(
                Some(node.syntax().text_range()),
                "Extern type declaration is missing type name",
            );
            return None;
        };
        let name = name_token.to_string();
        return Some(ast::Item::ExternType(ast::ExternType {
            attrs: attrs.clone(),
            goml_name: ast::AstIdent::new(&name),
        }));
    }

    let lang_token = node.lang();
    let lang = match lang_token {
        Some(token) => {
            let raw = token.to_string();
            raw.strip_prefix('\"')
                .and_then(|s| s.strip_suffix('\"'))
                .map(|s| s.to_string())
        }
        None => None,
    };
    let Some(lang) = lang else {
        ctx.push_error(
            Some(node.syntax().text_range()),
            "Extern declaration is missing language string; builtin externs should use `#[builtin] extern fn`.",
        );
        return None;
    };

    if lang.as_str() != "go" {
        ctx.push_error(
            Some(node.syntax().text_range()),
            format!("Unsupported extern language: {}", lang),
        );
        return None;
    }

    let package_token = node.package();
    let package_path = match package_token {
        Some(token) => {
            let raw = token.to_string();
            raw.strip_prefix('\"')
                .and_then(|s| s.strip_suffix('\"'))
                .map(|s| s.to_string())
        }
        None => None,
    };
    let Some(package_path) = package_path else {
        ctx.push_error(
            Some(node.syntax().text_range()),
            "Extern declaration is missing package string",
        );
        return None;
    };

    if node.type_keyword().is_some() {
        let Some(name_token) = node.uident() else {
            ctx.push_error(
                Some(node.syntax().text_range()),
                "Extern type declaration is missing type name",
            );
            return None;
        };
        let name = name_token.to_string();
        return Some(ast::Item::ExternType(ast::ExternType {
            attrs: attrs.clone(),
            goml_name: ast::AstIdent::new(&name),
        }));
    }

    let go_symbol_override = node.symbol().and_then(|token| {
        let raw = token.to_string();
        raw.strip_prefix('\"')
            .and_then(|s| s.strip_suffix('\"'))
            .map(|s| s.to_string())
    });

    let Some(name_token) = node.lident() else {
        ctx.push_error(
            Some(node.syntax().text_range()),
            "Extern declaration is missing function name",
        );
        return None;
    };
    let name = name_token.to_string();
    let (go_symbol, explicit_go_symbol) = match go_symbol_override {
        Some(symbol) => (symbol, true),
        None => (name.clone(), false),
    };

    let params = node
        .param_list()
        .map(|list| {
            list.params()
                .flat_map(|param| lower_param(ctx, param))
                .collect()
        })
        .unwrap_or_default();
    let ret_ty = node.return_type().and_then(|ty| lower_ty(ctx, ty));

    Some(ast::Item::ExternGo(ast::ExternGo {
        attrs,
        package_path,
        go_symbol,
        goml_name: ast::AstIdent(name),
        explicit_go_symbol,
        params,
        ret_ty,
    }))
}

fn lower_block(ctx: &mut LowerCtx, node: cst::Block) -> Option<ast::Expr> {
    let mut exprs: Vec<ast::Expr> = Vec::new();
    let astptr = MySyntaxNodePtr::new(node.syntax());

    for stmt in node.stmts() {
        if let Some(expr) = lower_stmt(ctx, stmt) {
            exprs.push(expr);
        }
    }

    if let Some(tail_expr) = node.expr() {
        if let Some(expr) = lower_expr(ctx, tail_expr) {
            exprs.push(expr);
        }
    } else {
        exprs.push(ast::Expr::EUnit { astptr });
    }

    Some(ast::Expr::EBlock { exprs, astptr })
}

fn lower_stmt(ctx: &mut LowerCtx, stmt: cst::Stmt) -> Option<ast::Expr> {
    match stmt {
        cst::Stmt::LetStmt(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let pattern = match it.pattern() {
                Some(pattern) => pattern,
                None => {
                    ctx.push_error(
                        Some(it.syntax().text_range()),
                        "Let statement missing pattern",
                    );
                    return None;
                }
            };
            let pat = lower_pat(ctx, pattern)?;
            let value_node = match it.value() {
                Some(value) => value,
                None => {
                    ctx.push_error(
                        Some(it.syntax().text_range()),
                        "Let statement missing value",
                    );
                    return None;
                }
            };
            let annotation = it.ty().and_then(|ty| lower_ty(ctx, ty));
            let value = lower_expr(ctx, value_node)?;
            Some(ast::Expr::ELet {
                pat,
                annotation,
                value: Box::new(value),
                astptr,
            })
        }
        cst::Stmt::ExprStmt(it) => {
            let expr_node = match it.expr() {
                Some(expr) => expr,
                None => {
                    ctx.push_error(
                        Some(it.syntax().text_range()),
                        "Expression statement missing expression",
                    );
                    return None;
                }
            };
            lower_expr(ctx, expr_node)
        }
    }
}

fn lower_param(ctx: &mut LowerCtx, node: cst::Param) -> Option<(ast::AstIdent, ast::TypeExpr)> {
    let name = node.lident().unwrap().to_string();
    let ty = match node.ty().and_then(|ty| lower_ty(ctx, ty)) {
        Some(ty) => ty,
        None => {
            ctx.push_error(
                Some(node.syntax().text_range()),
                format!("Param {} has no type", name),
            );
            return None;
        }
    };
    Some((ast::AstIdent(name), ty))
}

fn lower_closure_param(ctx: &mut LowerCtx, node: cst::ClosureParam) -> Option<ast::ClosureParam> {
    let Some(name_token) = node.lident() else {
        ctx.push_error(
            Some(node.syntax().text_range()),
            "Closure parameter missing name",
        );
        return None;
    };

    let name = ast::AstIdent(name_token.to_string());
    let ty = match node.ty() {
        Some(ty_node) => Some(lower_ty(ctx, ty_node)?),
        None => None,
    };
    let astptr = MySyntaxNodePtr::new(node.syntax());

    Some(ast::ClosureParam { name, ty, astptr })
}

fn lower_expr(ctx: &mut LowerCtx, node: cst::Expr) -> Option<ast::Expr> {
    lower_expr_with_args(ctx, node, Vec::new())
}

fn lower_expr_with_args(
    ctx: &mut LowerCtx,
    node: cst::Expr,
    trailing_args: Vec<ast::Expr>,
) -> Option<ast::Expr> {
    let node_range = node.syntax().text_range();
    let node_astptr = MySyntaxNodePtr::new(node.syntax());
    match node {
        cst::Expr::UnitExpr(_) => {
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(node_range),
                    "Cannot apply arguments to unit expression",
                );
                return None;
            }
            Some(ast::Expr::EUnit {
                astptr: node_astptr,
            })
        }
        cst::Expr::BoolExpr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let Some(token) = it.value() else {
                ctx.push_error(Some(it.syntax().text_range()), "BoolExpr has no value");
                return None;
            };
            let value = token.to_string();
            let expr = match value.as_str() {
                "true" => ast::Expr::EBool {
                    value: true,
                    astptr,
                },
                "false" => ast::Expr::EBool {
                    value: false,
                    astptr,
                },
                _ => {
                    ctx.push_error(
                        Some(token.text_range()),
                        format!("Invalid boolean literal: {}", value),
                    );
                    return None;
                }
            };
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to bool literal",
                );
                return None;
            }
            Some(expr)
        }
        cst::Expr::IntExpr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let Some(token) = it.value() else {
                ctx.push_error(Some(it.syntax().text_range()), "IntExpr has no value");
                return None;
            };
            let value = token.to_string();
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to integer literal",
                );
                return None;
            }
            Some(ast::Expr::EInt { value, astptr })
        }
        cst::Expr::Int8Expr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let Some(token) = it.value() else {
                ctx.push_error(Some(it.syntax().text_range()), "Int8Expr has no value");
                return None;
            };
            let text = token.to_string();
            let value = text.strip_suffix("i8").unwrap_or(&text).to_string();
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to integer literal",
                );
                return None;
            }
            Some(ast::Expr::EInt8 { value, astptr })
        }
        cst::Expr::Int16Expr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let Some(token) = it.value() else {
                ctx.push_error(Some(it.syntax().text_range()), "Int16Expr has no value");
                return None;
            };
            let text = token.to_string();
            let value = text.strip_suffix("i16").unwrap_or(&text).to_string();
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to integer literal",
                );
                return None;
            }
            Some(ast::Expr::EInt16 { value, astptr })
        }
        cst::Expr::Int32Expr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let Some(token) = it.value() else {
                ctx.push_error(Some(it.syntax().text_range()), "Int32Expr has no value");
                return None;
            };
            let text = token.to_string();
            let value = text.strip_suffix("i32").unwrap_or(&text).to_string();
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to integer literal",
                );
                return None;
            }
            Some(ast::Expr::EInt32 { value, astptr })
        }
        cst::Expr::Int64Expr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let Some(token) = it.value() else {
                ctx.push_error(Some(it.syntax().text_range()), "Int64Expr has no value");
                return None;
            };
            let text = token.to_string();
            let value = text.strip_suffix("i64").unwrap_or(&text).to_string();
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to integer literal",
                );
                return None;
            }
            Some(ast::Expr::EInt64 { value, astptr })
        }
        cst::Expr::UInt8Expr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let Some(token) = it.value() else {
                ctx.push_error(Some(it.syntax().text_range()), "UInt8Expr has no value");
                return None;
            };
            let text = token.to_string();
            let value = text.strip_suffix("u8").unwrap_or(&text).to_string();
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to integer literal",
                );
                return None;
            }
            Some(ast::Expr::EUInt8 { value, astptr })
        }
        cst::Expr::UInt16Expr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let Some(token) = it.value() else {
                ctx.push_error(Some(it.syntax().text_range()), "UInt16Expr has no value");
                return None;
            };
            let text = token.to_string();
            let value = text.strip_suffix("u16").unwrap_or(&text).to_string();
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to integer literal",
                );
                return None;
            }
            Some(ast::Expr::EUInt16 { value, astptr })
        }
        cst::Expr::UInt32Expr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let Some(token) = it.value() else {
                ctx.push_error(Some(it.syntax().text_range()), "UInt32Expr has no value");
                return None;
            };
            let text = token.to_string();
            let value = text.strip_suffix("u32").unwrap_or(&text).to_string();
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to integer literal",
                );
                return None;
            }
            Some(ast::Expr::EUInt32 { value, astptr })
        }
        cst::Expr::UInt64Expr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let Some(token) = it.value() else {
                ctx.push_error(Some(it.syntax().text_range()), "UInt64Expr has no value");
                return None;
            };
            let text = token.to_string();
            let value = text.strip_suffix("u64").unwrap_or(&text).to_string();
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to integer literal",
                );
                return None;
            }
            Some(ast::Expr::EUInt64 { value, astptr })
        }
        cst::Expr::FloatExpr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let Some(token) = it.value() else {
                ctx.push_error(Some(it.syntax().text_range()), "FloatExpr has no value");
                return None;
            };
            let text = token.to_string();
            let value = match text.parse::<f64>() {
                Ok(value) => value,
                Err(_) => {
                    ctx.push_error(
                        Some(token.text_range()),
                        format!("Invalid float literal: {}", text),
                    );
                    return None;
                }
            };
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to float literal",
                );
                return None;
            }
            Some(ast::Expr::EFloat { value, astptr })
        }
        cst::Expr::Float32Expr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let Some(token) = it.value() else {
                ctx.push_error(Some(it.syntax().text_range()), "Float32Expr has no value");
                return None;
            };
            let text = token.to_string();
            let value = text.strip_suffix("f32").unwrap_or(&text).to_string();
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to float literal",
                );
                return None;
            }
            Some(ast::Expr::EFloat32 { value, astptr })
        }
        cst::Expr::Float64Expr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let Some(token) = it.value() else {
                ctx.push_error(Some(it.syntax().text_range()), "Float64Expr has no value");
                return None;
            };
            let text = token.to_string();
            let value = text.strip_suffix("f64").unwrap_or(&text).to_string();
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to float literal",
                );
                return None;
            }
            Some(ast::Expr::EFloat64 { value, astptr })
        }
        cst::Expr::StrExpr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let Some(token) = it.value() else {
                ctx.push_error(Some(it.syntax().text_range()), "StrExpr has no value");
                return None;
            };
            let raw = token.to_string();
            let Some(value) = raw.strip_prefix('"').and_then(|s| s.strip_suffix('"')) else {
                ctx.push_error(Some(token.text_range()), "StrExpr has no value");
                return None;
            };
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to string literal",
                );
                return None;
            }
            Some(ast::Expr::EString {
                value: value.to_string(),
                astptr,
            })
        }
        cst::Expr::MultilineStrExpr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let Some(token) = it.value() else {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "MultilineStrExpr has no value",
                );
                return None;
            };
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to string literal",
                );
                return None;
            }
            let text = token.to_string();
            let lines: Vec<&str> = text.lines().collect();
            let mut parts = Vec::with_capacity(lines.len());
            for line in lines {
                let trimmed = line.trim_start_matches([' ', '\t']);
                let Some(rest) = trimmed.strip_prefix("\\\\") else {
                    ctx.push_error(Some(token.text_range()), "Invalid multiline string content");
                    return None;
                };
                parts.push(rest);
            }
            let value = parts.join("\n");
            Some(ast::Expr::EString { value, astptr })
        }
        cst::Expr::CallExpr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let args: Vec<ast::Expr> = it
                .arg_list()
                .map(|list| list.args().flat_map(|arg| lower_arg(ctx, arg)).collect())
                .unwrap_or_default();
            if let Some(callee) = support::child::<cst::Expr>(it.syntax()) {
                return match callee {
                    cst::Expr::IdentExpr(ident_expr) => {
                        let constructor = lower_constructor_path_from_ident_expr(ctx, &ident_expr)?;
                        let variant_ident = constructor
                            .last_ident()
                            .cloned()
                            .expect("paths must contain at least one segment");
                        let callee_astptr = MySyntaxNodePtr::new(ident_expr.syntax());

                        if ctx.is_constructor(&variant_ident) {
                            let constr = ast::Expr::EConstr {
                                constructor,
                                args,
                                astptr,
                            };
                            apply_trailing_args(
                                ctx,
                                constr,
                                trailing_args,
                                Some(it.syntax().text_range()),
                            )
                        } else {
                            let path_expr = ast::Expr::EPath {
                                path: constructor,
                                astptr: callee_astptr,
                            };
                            let call = ast::Expr::ECall {
                                func: Box::new(path_expr),
                                args,
                                astptr,
                            };
                            apply_trailing_args(
                                ctx,
                                call,
                                trailing_args,
                                Some(it.syntax().text_range()),
                            )
                        }
                    }
                    other => {
                        if matches!(&other, cst::Expr::CallExpr(_) | cst::Expr::ClosureExpr(_)) {
                            let func_expr = lower_expr(ctx, other)?;
                            let call = ast::Expr::ECall {
                                func: Box::new(func_expr),
                                args,
                                astptr,
                            };
                            apply_trailing_args(
                                ctx,
                                call,
                                trailing_args,
                                Some(it.syntax().text_range()),
                            )
                        } else if let cst::Expr::BinaryExpr(bin_expr) = &other {
                            if matches!(
                                bin_expr.op().map(|tok| tok.kind()),
                                Some(MySyntaxKind::Dot)
                            ) {
                                let func_expr = lower_expr(ctx, other)?;
                                let call = ast::Expr::ECall {
                                    func: Box::new(func_expr),
                                    args,
                                    astptr,
                                };
                                apply_trailing_args(
                                    ctx,
                                    call,
                                    trailing_args,
                                    Some(it.syntax().text_range()),
                                )
                            } else {
                                let mut combined_args = args;
                                combined_args.extend(trailing_args);
                                lower_expr_with_args(ctx, other, combined_args)
                            }
                        } else {
                            let mut combined_args = args;
                            combined_args.extend(trailing_args);
                            lower_expr_with_args(ctx, other, combined_args)
                        }
                    }
                };
            }
            ctx.push_error(
                Some(it.syntax().text_range()),
                "CallExpr has no function name",
            );
            None
        }
        cst::Expr::MatchExpr(it) => {
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to match expression",
                );
                return None;
            }
            let Some(scrutinee) = it.expr() else {
                ctx.push_error(Some(it.syntax().text_range()), "MatchExpr has no expr");
                return None;
            };
            let expr = lower_expr(ctx, scrutinee)?;
            let Some(arm_list) = it.match_arm_list() else {
                ctx.push_error(Some(it.syntax().text_range()), "MatchExpr has no arms");
                return None;
            };
            let arms = arm_list
                .arms()
                .flat_map(|arm| lower_arm(ctx, arm))
                .collect();
            Some(ast::Expr::EMatch {
                expr: Box::new(expr),
                arms,
                astptr: MySyntaxNodePtr::new(it.syntax()),
            })
        }
        cst::Expr::GoExpr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to go expression",
                );
                return None;
            }

            let Some(inner) = it.expr() else {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "go statement missing expression",
                );
                return None;
            };

            let expr = lower_expr(ctx, inner)?;

            Some(ast::Expr::EGo {
                expr: Box::new(expr),
                astptr,
            })
        }
        cst::Expr::IfExpr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to if expression",
                );
                return None;
            }

            let cond = it
                .cond()
                .and_then(|cond| cond.expr())
                .and_then(|expr| lower_expr(ctx, expr));

            let cond = match cond {
                Some(cond) => cond,
                None => {
                    ctx.push_error(
                        Some(it.syntax().text_range()),
                        "If expression missing condition",
                    );
                    return None;
                }
            };

            let then_branch = match it.then_branch() {
                Some(branch) => {
                    if let Some(block) = branch.block() {
                        lower_block(ctx, block)
                    } else if let Some(expr) = branch.expr() {
                        lower_expr(ctx, expr)
                    } else {
                        ctx.push_error(
                            Some(branch.syntax().text_range()),
                            "If expression then-branch missing body",
                        );
                        None
                    }
                }
                None => {
                    ctx.push_error(
                        Some(it.syntax().text_range()),
                        "If expression missing then branch",
                    );
                    None
                }
            }?;

            let else_branch = match it.else_branch() {
                Some(branch) => {
                    if let Some(block) = branch.block() {
                        lower_block(ctx, block)
                    } else if let Some(expr) = branch.expr() {
                        lower_expr(ctx, expr)
                    } else {
                        ctx.push_error(
                            Some(branch.syntax().text_range()),
                            "If expression else-branch missing body",
                        );
                        None
                    }
                }
                None => {
                    ctx.push_error(
                        Some(it.syntax().text_range()),
                        "If expression missing else branch",
                    );
                    None
                }
            }?;

            Some(ast::Expr::EIf {
                cond: Box::new(cond),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
                astptr,
            })
        }
        cst::Expr::WhileExpr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to while expression",
                );
                return None;
            }

            let cond = it
                .cond()
                .and_then(|cond| cond.expr())
                .and_then(|expr| lower_expr(ctx, expr));

            let cond = match cond {
                Some(cond) => cond,
                None => {
                    ctx.push_error(
                        Some(it.syntax().text_range()),
                        "While expression missing condition",
                    );
                    return None;
                }
            };

            let body = match it.body() {
                Some(body) => {
                    if let Some(block) = body.block() {
                        lower_block(ctx, block)
                    } else if let Some(expr) = body.expr() {
                        lower_expr(ctx, expr)
                    } else {
                        ctx.push_error(
                            Some(body.syntax().text_range()),
                            "While expression body missing expression",
                        );
                        None
                    }
                }
                None => {
                    ctx.push_error(
                        Some(it.syntax().text_range()),
                        "While expression missing body",
                    );
                    None
                }
            }?;

            Some(ast::Expr::EWhile {
                cond: Box::new(cond),
                body: Box::new(body),
                astptr,
            })
        }
        cst::Expr::StructLiteralExpr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to struct literal",
                );
                return None;
            }
            let name = lower_path(ctx, &it.path()?)?;
            let fields = it
                .field_list()
                .map(|list| {
                    list.fields()
                        .flat_map(|field| {
                            let fname = field.lident()?.to_string();
                            let ident = ast::AstIdent::new(&fname);
                            let expr = field
                                .expr()
                                .and_then(|expr| lower_expr(ctx, expr))
                                .or_else(|| {
                                    Some(ast::Expr::EPath {
                                        path: ast::Path::from_ident(ident.clone()),
                                        astptr: MySyntaxNodePtr::new(field.syntax()),
                                    })
                                })?;
                            Some((ident, expr))
                        })
                        .collect()
                })
                .unwrap_or_default();
            Some(ast::Expr::EStructLiteral {
                name,
                fields,
                astptr,
            })
        }
        cst::Expr::ArrayLiteralExpr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to array literal",
                );
                return None;
            }
            let items = it.exprs().flat_map(|expr| lower_expr(ctx, expr)).collect();
            Some(ast::Expr::EArray { items, astptr })
        }
        cst::Expr::IdentExpr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let constructor = lower_constructor_path_from_ident_expr(ctx, &it)?;
            let variant_ident = constructor
                .last_ident()
                .cloned()
                .expect("paths must contain at least one segment");
            if ctx.is_constructor(&variant_ident) {
                let expr = ast::Expr::EConstr {
                    constructor,
                    args: vec![],
                    astptr,
                };
                apply_trailing_args(ctx, expr, trailing_args, Some(it.syntax().text_range()))
            } else {
                let expr = ast::Expr::EPath {
                    path: constructor,
                    astptr,
                };
                apply_trailing_args(ctx, expr, trailing_args, Some(it.syntax().text_range()))
            }
        }
        cst::Expr::TupleExpr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to tuple literal",
                );
                return None;
            }
            let items = it.exprs().flat_map(|expr| lower_expr(ctx, expr)).collect();
            Some(ast::Expr::ETuple { items, astptr })
        }
        cst::Expr::ParenExpr(it) => {
            // Parenthesized expression - just unwrap and process the inner expression
            let inner = it
                .expr()
                .and_then(|expr| lower_expr_with_args(ctx, expr, trailing_args))?;
            Some(inner)
        }
        cst::Expr::PrefixExpr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let expr = match it
                .expr()
                .and_then(|expr| lower_expr_with_args(ctx, expr, Vec::new()))
            {
                Some(expr) => expr,
                None => {
                    ctx.push_error(
                        Some(it.syntax().text_range()),
                        "Prefix expression missing operand",
                    );
                    return None;
                }
            };
            let Some(op_token) = it.op() else {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Prefix expression missing operator",
                );
                return None;
            };
            let unary = match op_token.kind() {
                MySyntaxKind::Minus => ast::Expr::EUnary {
                    op: common_defs::UnaryOp::Neg,
                    expr: Box::new(expr),
                    astptr,
                },
                MySyntaxKind::Bang => ast::Expr::EUnary {
                    op: common_defs::UnaryOp::Not,
                    expr: Box::new(expr),
                    astptr,
                },
                kind => {
                    ctx.push_error(
                        Some(op_token.text_range()),
                        format!("Unsupported prefix operator: {:?}", kind),
                    );
                    return None;
                }
            };
            apply_trailing_args(ctx, unary, trailing_args, Some(it.syntax().text_range()))
        }
        cst::Expr::BinaryExpr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let mut exprs = it.exprs();
            let Some(lhs_cst) = exprs.next() else {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Binary expression missing lhs",
                );
                return None;
            };
            let Some(rhs_cst) = exprs.next() else {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Binary expression missing rhs",
                );
                return None;
            };
            let lhs = lower_expr_with_args(ctx, lhs_cst, Vec::new())?;
            let Some(op_token) = it.op() else {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Binary expression missing operator",
                );
                return None;
            };
            match op_token.kind() {
                MySyntaxKind::Plus => {
                    let rhs = lower_expr_with_args(ctx, rhs_cst, trailing_args)?;
                    Some(ast::Expr::EBinary {
                        op: common_defs::BinaryOp::Add,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        astptr,
                    })
                }
                MySyntaxKind::Minus => {
                    let rhs = lower_expr_with_args(ctx, rhs_cst, trailing_args)?;
                    Some(ast::Expr::EBinary {
                        op: common_defs::BinaryOp::Sub,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        astptr,
                    })
                }
                MySyntaxKind::Star => {
                    let rhs = lower_expr_with_args(ctx, rhs_cst, trailing_args)?;
                    Some(ast::Expr::EBinary {
                        op: common_defs::BinaryOp::Mul,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        astptr,
                    })
                }
                MySyntaxKind::Slash => {
                    let rhs = lower_expr_with_args(ctx, rhs_cst, trailing_args)?;
                    Some(ast::Expr::EBinary {
                        op: common_defs::BinaryOp::Div,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        astptr,
                    })
                }
                MySyntaxKind::AndAnd => {
                    let rhs = lower_expr_with_args(ctx, rhs_cst, trailing_args)?;
                    Some(ast::Expr::EBinary {
                        op: common_defs::BinaryOp::And,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        astptr,
                    })
                }
                MySyntaxKind::OrOr => {
                    let rhs = lower_expr_with_args(ctx, rhs_cst, trailing_args)?;
                    Some(ast::Expr::EBinary {
                        op: common_defs::BinaryOp::Or,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        astptr,
                    })
                }
                MySyntaxKind::Less => {
                    let rhs = lower_expr_with_args(ctx, rhs_cst, trailing_args)?;
                    Some(ast::Expr::EBinary {
                        op: common_defs::BinaryOp::Less,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        astptr,
                    })
                }
                MySyntaxKind::Greater => {
                    let rhs = lower_expr_with_args(ctx, rhs_cst, trailing_args)?;
                    Some(ast::Expr::EBinary {
                        op: common_defs::BinaryOp::Greater,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        astptr,
                    })
                }
                MySyntaxKind::LessEq => {
                    let rhs = lower_expr_with_args(ctx, rhs_cst, trailing_args)?;
                    Some(ast::Expr::EBinary {
                        op: common_defs::BinaryOp::LessEq,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        astptr,
                    })
                }
                MySyntaxKind::GreaterEq => {
                    let rhs = lower_expr_with_args(ctx, rhs_cst, trailing_args)?;
                    Some(ast::Expr::EBinary {
                        op: common_defs::BinaryOp::GreaterEq,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        astptr,
                    })
                }
                MySyntaxKind::EqEq => {
                    let rhs = lower_expr_with_args(ctx, rhs_cst, trailing_args)?;
                    Some(ast::Expr::EBinary {
                        op: common_defs::BinaryOp::Eq,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        astptr,
                    })
                }
                MySyntaxKind::NotEq => {
                    let rhs = lower_expr_with_args(ctx, rhs_cst, trailing_args)?;
                    Some(ast::Expr::EBinary {
                        op: common_defs::BinaryOp::NotEq,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        astptr,
                    })
                }
                MySyntaxKind::Dot => match rhs_cst {
                    cst::Expr::IntExpr(int_expr) => {
                        let Some(token) = int_expr.value() else {
                            ctx.push_error(
                                Some(int_expr.syntax().text_range()),
                                "Tuple projection missing index",
                            );
                            return None;
                        };
                        let text = token.to_string();
                        let index = match text.parse::<usize>() {
                            Ok(index) => index,
                            Err(_) => {
                                ctx.push_error(
                                    Some(token.text_range()),
                                    format!("Invalid tuple index: {}", text),
                                );
                                return None;
                            }
                        };
                        Some(ast::Expr::EProj {
                            tuple: Box::new(lhs),
                            index,
                            astptr,
                        })
                    }
                    cst::Expr::IdentExpr(ident_expr) => {
                        let Some(token) = ident_expr.path().and_then(|p| p.ident_tokens().last())
                        else {
                            ctx.push_error(
                                Some(ident_expr.syntax().text_range()),
                                "Field access missing name",
                            );
                            return None;
                        };
                        let field = ast::AstIdent(token.to_string());
                        let field_expr = ast::Expr::EField {
                            expr: Box::new(lhs),
                            field,
                            astptr: MySyntaxNodePtr::new(it.syntax()),
                        };
                        if trailing_args.is_empty() {
                            Some(field_expr)
                        } else {
                            Some(ast::Expr::ECall {
                                func: Box::new(field_expr),
                                args: trailing_args,
                                astptr,
                            })
                        }
                    }
                    other => {
                        ctx.push_error(
                            Some(other.syntax().text_range()),
                            "Unsupported field access expression",
                        );
                        None
                    }
                },
                kind => {
                    let message = if trailing_args.is_empty() {
                        format!("Unsupported binary operator: {:?}", kind)
                    } else {
                        format!("Unsupported binary operator with trailing args: {:?}", kind)
                    };
                    ctx.push_error(Some(op_token.text_range()), message);
                    None
                }
            }
        }
        cst::Expr::ClosureExpr(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to closure expression",
                );
                return None;
            }

            let params = match it.params() {
                Some(list) => list
                    .params()
                    .flat_map(|param| lower_closure_param(ctx, param))
                    .collect(),
                None => {
                    ctx.push_error(
                        Some(it.syntax().text_range()),
                        "Closure missing parameter list",
                    );
                    Vec::new()
                }
            };

            let Some(body_node) = it.body() else {
                ctx.push_error(Some(it.syntax().text_range()), "Closure missing body");
                return None;
            };

            let body = if let Some(block) = body_node.block() {
                lower_block(ctx, block)?
            } else if let Some(expr) = body_node.expr() {
                lower_expr(ctx, expr)?
            } else {
                ctx.push_error(
                    Some(body_node.syntax().text_range()),
                    "Closure body missing expr",
                );
                return None;
            };

            Some(ast::Expr::EClosure {
                params,
                body: Box::new(body),
                astptr,
            })
        }
    }
}

fn apply_trailing_args(
    ctx: &mut LowerCtx,
    expr: ast::Expr,
    trailing_args: Vec<ast::Expr>,
    range: Option<TextRange>,
) -> Option<ast::Expr> {
    if trailing_args.is_empty() {
        return Some(expr);
    }

    match expr {
        ast::Expr::EPath { path, astptr } => Some(ast::Expr::ECall {
            func: Box::new(ast::Expr::EPath { path, astptr }),
            args: trailing_args,
            astptr,
        }),
        ast::Expr::ECall { func, args, astptr } => {
            let mut result = ast::Expr::ECall { func, args, astptr };
            for arg in trailing_args {
                let call_astptr = match &result {
                    ast::Expr::ECall { astptr, .. } => *astptr,
                    _ => unreachable!(),
                };
                result = ast::Expr::ECall {
                    func: Box::new(result),
                    args: vec![arg],
                    astptr: call_astptr,
                };
            }
            Some(result)
        }
        ast::Expr::EConstr {
            constructor,
            mut args,
            astptr,
        } => {
            args.extend(trailing_args);
            Some(ast::Expr::EConstr {
                constructor,
                args,
                astptr,
            })
        }
        ast::Expr::EField {
            expr,
            field,
            astptr,
        } => Some(ast::Expr::ECall {
            func: Box::new(ast::Expr::EField {
                expr,
                field,
                astptr,
            }),
            args: trailing_args,
            astptr,
        }),
        ast::Expr::EBinary {
            op,
            lhs,
            rhs,
            astptr,
        } => {
            let rhs = apply_trailing_args(ctx, *rhs, trailing_args, range)?;
            Some(ast::Expr::EBinary {
                op,
                lhs,
                rhs: Box::new(rhs),
                astptr,
            })
        }
        ast::Expr::EUnary { op, expr, astptr } => {
            let expr = apply_trailing_args(ctx, *expr, trailing_args, range)?;
            Some(ast::Expr::EUnary {
                op,
                expr: Box::new(expr),
                astptr,
            })
        }
        other => {
            ctx.push_error(
                range,
                format!("Cannot apply arguments to expression {:?}", other),
            );
            None
        }
    }
}

fn lower_arg(ctx: &mut LowerCtx, node: cst::Arg) -> Option<ast::Expr> {
    match node.expr() {
        Some(expr) => lower_expr(ctx, expr),
        None => {
            ctx.push_error(Some(node.syntax().text_range()), "Arg has no expr");
            None
        }
    }
}

fn lower_arm(ctx: &mut LowerCtx, node: cst::MatchArm) -> Option<ast::Arm> {
    let pat = node.pattern().and_then(|pat| lower_pat(ctx, pat))?;
    let body = if let Some(expr) = node.expr() {
        lower_expr(ctx, expr)
    } else if let Some(block) = support::child::<cst::Block>(node.syntax()) {
        lower_block(ctx, block)
    } else {
        ctx.push_error(Some(node.syntax().text_range()), "Match arm has no body");
        None
    }?;
    Some(ast::Arm { pat, body })
}

fn lower_pat(ctx: &mut LowerCtx, node: cst::Pattern) -> Option<ast::Pat> {
    let node_astptr = MySyntaxNodePtr::new(node.syntax());
    match node {
        cst::Pattern::VarPat(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let name = it.lident().unwrap().to_string();
            let ident = ast::AstIdent(name);
            if ctx.is_constructor(&ident) {
                Some(ast::Pat::PConstr {
                    constructor: ast::Path::from_ident(ident),
                    args: Vec::new(),
                    astptr,
                })
            } else {
                Some(ast::Pat::PVar {
                    name: ident,
                    astptr,
                })
            }
        }
        cst::Pattern::UnitPat(_) => Some(ast::Pat::PUnit {
            astptr: node_astptr,
        }),
        cst::Pattern::BoolPat(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let value = it.value()?.to_string();

            match value.as_str() {
                "true" => Some(ast::Pat::PBool {
                    value: true,
                    astptr,
                }),
                "false" => Some(ast::Pat::PBool {
                    value: false,
                    astptr,
                }),
                _ => {
                    ctx.push_error(
                        Some(it.syntax().text_range()),
                        format!("Invalid boolean pattern: {}", value),
                    );
                    None
                }
            }
        }
        cst::Pattern::IntPat(it) => Some(ast::Pat::PInt {
            value: it.value()?.to_string(),
            astptr: MySyntaxNodePtr::new(it.syntax()),
        }),
        cst::Pattern::Int8Pat(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let text = it.value()?.to_string();
            let value = text.strip_suffix("i8").unwrap_or(&text).to_string();
            Some(ast::Pat::PInt8 { value, astptr })
        }
        cst::Pattern::Int16Pat(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let text = it.value()?.to_string();
            let value = text.strip_suffix("i16").unwrap_or(&text).to_string();
            Some(ast::Pat::PInt16 { value, astptr })
        }
        cst::Pattern::Int32Pat(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let text = it.value()?.to_string();
            let value = text.strip_suffix("i32").unwrap_or(&text).to_string();
            Some(ast::Pat::PInt32 { value, astptr })
        }
        cst::Pattern::Int64Pat(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let text = it.value()?.to_string();
            let value = text.strip_suffix("i64").unwrap_or(&text).to_string();
            Some(ast::Pat::PInt64 { value, astptr })
        }
        cst::Pattern::UInt8Pat(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let text = it.value()?.to_string();
            let value = text.strip_suffix("u8").unwrap_or(&text).to_string();
            Some(ast::Pat::PUInt8 { value, astptr })
        }
        cst::Pattern::UInt16Pat(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let text = it.value()?.to_string();
            let value = text.strip_suffix("u16").unwrap_or(&text).to_string();
            Some(ast::Pat::PUInt16 { value, astptr })
        }
        cst::Pattern::UInt32Pat(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let text = it.value()?.to_string();
            let value = text.strip_suffix("u32").unwrap_or(&text).to_string();
            Some(ast::Pat::PUInt32 { value, astptr })
        }
        cst::Pattern::UInt64Pat(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let text = it.value()?.to_string();
            let value = text.strip_suffix("u64").unwrap_or(&text).to_string();
            Some(ast::Pat::PUInt64 { value, astptr })
        }
        cst::Pattern::StringPat(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            let Some(token) = it.value() else {
                ctx.push_error(Some(it.syntax().text_range()), "StringPat has no value");
                return None;
            };
            let raw = token.to_string();
            let Some(value) = raw.strip_prefix('"').and_then(|s| s.strip_suffix('"')) else {
                ctx.push_error(Some(token.text_range()), "StringPat has no value");
                return None;
            };
            Some(ast::Pat::PString {
                value: value.to_string(),
                astptr,
            })
        }
        cst::Pattern::ConstrPat(it) => {
            let astptr = MySyntaxNodePtr::new(it.syntax());
            if let Some(field_list) = it.field_list() {
                let name = lower_path(ctx, &it.path()?)?;
                let mut fields = Vec::new();
                for field in field_list.fields() {
                    let Some(fname_token) = field.lident() else {
                        ctx.push_error(
                            Some(field.syntax().text_range()),
                            "Struct pattern field missing name",
                        );
                        return None;
                    };
                    let fname = fname_token.to_string();
                    let pat = match field.pattern() {
                        Some(pattern) => lower_pat(ctx, pattern)?,
                        None => {
                            if support::token(field.syntax(), MySyntaxKind::Colon).is_some() {
                                ctx.push_error(
                                    Some(field.syntax().text_range()),
                                    "Struct pattern field missing pattern",
                                );
                                return None;
                            }

                            ast::Pat::PVar {
                                name: ast::AstIdent(fname.clone()),
                                astptr: MySyntaxNodePtr::new(field.syntax()),
                            }
                        }
                    };
                    fields.push((ast::AstIdent(fname), pat));
                }
                Some(ast::Pat::PStruct {
                    name,
                    fields,
                    astptr,
                })
            } else {
                let constructor = lower_constructor_path_from_constr_pat(ctx, &it)?;
                let pats = it.patterns().flat_map(|pat| lower_pat(ctx, pat)).collect();
                Some(ast::Pat::PConstr {
                    constructor,
                    args: pats,
                    astptr,
                })
            }
        }
        cst::Pattern::TuplePat(it) => {
            let items = it.patterns().flat_map(|pat| lower_pat(ctx, pat)).collect();
            Some(ast::Pat::PTuple {
                pats: items,
                astptr: MySyntaxNodePtr::new(it.syntax()),
            })
        }
        cst::Pattern::WildPat(_) => Some(ast::Pat::PWild {
            astptr: node_astptr,
        }),
    }
}

fn lower_constructor_path_from_ident_expr(
    ctx: &mut LowerCtx,
    expr: &cst::IdentExpr,
) -> Option<ast::Path> {
    if let Some(path) = expr.path() {
        lower_path(ctx, &path)
    } else {
        ctx.push_error(
            Some(expr.syntax().text_range()),
            "Missing identifier in expression",
        );
        None
    }
}

fn lower_constructor_path_from_constr_pat(
    ctx: &mut LowerCtx,
    pat: &cst::ConstrPat,
) -> Option<ast::Path> {
    if let Some(path) = pat.path() {
        lower_path(ctx, &path)
    } else {
        ctx.push_error(
            Some(pat.syntax().text_range()),
            "Missing constructor name in pattern",
        );
        None
    }
}

fn lower_path(ctx: &mut LowerCtx, path: &cst::Path) -> Option<ast::Path> {
    let segments: Vec<ast::PathSegment> = path
        .ident_tokens()
        .map(|token| ast::PathSegment::new(ast::AstIdent(token.to_string())))
        .collect();

    if segments.is_empty() {
        ctx.push_error(
            Some(path.syntax().text_range()),
            "Paths must contain at least one identifier",
        );
        None
    } else {
        Some(ast::Path::new(segments))
    }
}
