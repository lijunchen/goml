use crate::ast;

use ::cst::cst::CstNode;
use ::cst::{cst, support};
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use parser::syntax::{MySyntaxKind, MySyntaxNodePtr};
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
}

impl LowerCtx {
    fn new() -> Self {
        Self {
            stage: Stage::other("lower"),
            diagnostics: Diagnostics::new(),
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
}

pub fn lower(node: cst::File) -> LowerResult {
    let mut ctx = LowerCtx::new();
    let items = node
        .items()
        .flat_map(|item| lower_item(&mut ctx, item))
        .collect();
    let ast = if ctx.has_errors() {
        None
    } else {
        Some(ast::File { toplevels: items })
    };

    LowerResult {
        ast,
        diagnostics: ctx.into_diagnostics(),
    }
}

fn lower_item(ctx: &mut LowerCtx, node: cst::Item) -> Option<ast::Item> {
    match node {
        cst::Item::Enum(it) => Some(ast::Item::EnumDef(lower_enum(ctx, it)?)),
        cst::Item::Struct(it) => Some(ast::Item::StructDef(lower_struct(ctx, it)?)),
        cst::Item::Trait(it) => Some(ast::Item::TraitDef(lower_trait(ctx, it)?)),
        cst::Item::Impl(it) => Some(ast::Item::ImplBlock(lower_impl_block(ctx, it)?)),
        cst::Item::Fn(it) => Some(ast::Item::Fn(lower_fn(ctx, it)?)),
        cst::Item::Extern(it) => Some(ast::Item::ExternGo(lower_extern(ctx, it)?)),
    }
}

fn lower_enum(ctx: &mut LowerCtx, node: cst::Enum) -> Option<ast::EnumDef> {
    let name = node.uident().unwrap().to_string();
    let generics: Vec<ast::Uident> = node
        .generic_list()
        .map(|list| {
            list.generics()
                .flat_map(|x| {
                    let name = x.uident().unwrap().to_string();
                    Some(ast::Uident::new(&name))
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
        name: ast::Uident::new(&name),
        generics,
        variants,
    })
}

fn lower_struct(ctx: &mut LowerCtx, node: cst::Struct) -> Option<ast::StructDef> {
    let name = node.uident()?.to_string();
    let generics: Vec<ast::Uident> = node
        .generic_list()
        .map(|list| {
            list.generics()
                .flat_map(|generic| {
                    let name = generic.uident()?.to_string();
                    Some(ast::Uident::new(&name))
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
        name: ast::Uident::new(&name),
        generics,
        fields,
    })
}

fn lower_struct_field(
    ctx: &mut LowerCtx,
    node: cst::StructField,
) -> Option<(ast::Lident, ast::Ty)> {
    let name = node.lident()?.to_string();
    let ty = node.ty().and_then(|ty| lower_ty(ctx, ty))?;
    Some((ast::Lident(name), ty))
}

fn lower_trait(ctx: &mut LowerCtx, node: cst::Trait) -> Option<ast::TraitDef> {
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
        name: ast::Uident::new(&name),
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
        None => ast::Ty::TUnit,
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
        name: ast::Lident(name),
        params,
        ret_ty,
    })
}

fn lower_impl_block(ctx: &mut LowerCtx, node: cst::Impl) -> Option<ast::ImplBlock> {
    let trait_name = node.uident().unwrap().to_string();
    let for_type = match node.for_type().and_then(|ty| lower_ty(ctx, ty)) {
        Some(ty) => ty,
        None => {
            ctx.push_error(
                Some(node.syntax().text_range()),
                format!("ImplBlock {} has no for type", trait_name),
            );
            return None;
        }
    };
    let methods: Vec<ast::Fn> = node
        .functions()
        .flat_map(|function| lower_fn(ctx, function))
        .collect();
    Some(ast::ImplBlock {
        trait_name: ast::Uident::new(&trait_name),
        for_type,
        methods,
    })
}

fn lower_variant(ctx: &mut LowerCtx, node: cst::Variant) -> Option<(ast::Uident, Vec<ast::Ty>)> {
    let name = node.uident().unwrap().to_string();
    let typs = match node.type_list() {
        None => vec![],
        Some(xs) => xs.types().flat_map(|ty| lower_ty(ctx, ty)).collect(),
    };
    Some((ast::Uident::new(&name), typs))
}

fn lower_ty(ctx: &mut LowerCtx, node: cst::Type) -> Option<ast::Ty> {
    match node {
        cst::Type::UnitTy(_) => Some(ast::Ty::TUnit),
        cst::Type::BoolTy(_) => Some(ast::Ty::TBool),
        cst::Type::IntTy(_) => Some(ast::Ty::TInt),
        cst::Type::StringTy(_) => Some(ast::Ty::TString),
        cst::Type::TupleTy(it) => {
            let typs = it
                .type_list()?
                .types()
                .flat_map(|ty| lower_ty(ctx, ty))
                .collect();
            Some(ast::Ty::TTuple { typs })
        }
        cst::Type::TAppTy(it) => {
            let name = it.uident().unwrap().to_string();
            let args: Vec<ast::Ty> = it
                .type_param_list()
                .map(|list| list.types().flat_map(|ty| lower_ty(ctx, ty)).collect())
                .unwrap_or_default();

            if name == "Ref" {
                match args.len() {
                    1 => Some(ast::Ty::TRef {
                        elem: Box::new(args.into_iter().next().unwrap()),
                    }),
                    0 => {
                        ctx.push_error(
                            Some(it.syntax().text_range()),
                            "Ref type missing element type",
                        );
                        None
                    }
                    _ => {
                        ctx.push_error(
                            Some(it.syntax().text_range()),
                            "Ref type takes exactly one argument",
                        );
                        None
                    }
                }
            } else {
                let base = ast::Ty::TCon { name };

                if args.is_empty() {
                    Some(base)
                } else {
                    Some(ast::Ty::TApp {
                        ty: Box::new(base),
                        args,
                    })
                }
            }
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

            Some(ast::Ty::TArray {
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
                ast::Ty::TTuple { typs } => typs,
                other => vec![other],
            };

            Some(ast::Ty::TFunc {
                params,
                ret_ty: Box::new(ret_ty),
            })
        }
    }
}

fn lower_fn(ctx: &mut LowerCtx, node: cst::Fn) -> Option<ast::Fn> {
    let name = node.lident().unwrap().to_string();
    let generics: Vec<ast::Uident> = node
        .generic_list()
        .map(|list| {
            list.generics()
                .flat_map(|x| {
                    let name = x.uident().unwrap().to_string();
                    Some(ast::Uident::new(&name))
                })
                .collect()
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
        name: ast::Lident(name),
        generics,
        params,
        ret_ty,
        body,
    })
}

fn lower_extern(ctx: &mut LowerCtx, node: cst::Extern) -> Option<ast::ExternGo> {
    let lang_token = node.lang();
    let lang = match lang_token {
        Some(token) => {
            let raw = token.to_string();
            raw.strip_prefix('"')
                .and_then(|s| s.strip_suffix('"'))
                .map(|s| s.to_string())
        }
        None => None,
    };
    let Some(lang) = lang else {
        ctx.push_error(
            Some(node.syntax().text_range()),
            "Extern declaration is missing language string",
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
            raw.strip_prefix('"')
                .and_then(|s| s.strip_suffix('"'))
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

    let Some(name_token) = node.lident() else {
        ctx.push_error(
            Some(node.syntax().text_range()),
            "Extern declaration is missing function name",
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

    Some(ast::ExternGo {
        package_path,
        go_symbol: name.clone(),
        goml_name: ast::Lident(name),
        params,
        ret_ty,
    })
}

fn lower_block(ctx: &mut LowerCtx, node: cst::Block) -> Option<ast::Expr> {
    let mut result = match node.expr() {
        Some(expr) => lower_expr(ctx, expr)?,
        None => ast::Expr::EUnit,
    };

    let mut stmts: Vec<cst::Stmt> = node.stmts().collect();
    while let Some(stmt) = stmts.pop() {
        result = lower_stmt(ctx, stmt, result);
    }

    Some(result)
}

fn lower_stmt(ctx: &mut LowerCtx, stmt: cst::Stmt, body: ast::Expr) -> ast::Expr {
    match stmt {
        cst::Stmt::LetStmt(it) => {
            let pattern = match it.pattern() {
                Some(pattern) => pattern,
                None => {
                    ctx.push_error(
                        Some(it.syntax().text_range()),
                        "Let statement missing pattern",
                    );
                    return body;
                }
            };
            let pat = match lower_pat(ctx, pattern) {
                Some(pat) => pat,
                None => return body,
            };
            let value_node = match it.value() {
                Some(value) => value,
                None => {
                    ctx.push_error(
                        Some(it.syntax().text_range()),
                        "Let statement missing value",
                    );
                    return body;
                }
            };
            let value = match lower_expr(ctx, value_node) {
                Some(expr) => expr,
                None => return body,
            };
            ast::Expr::ELet {
                pat,
                value: Box::new(value),
                body: Box::new(body),
            }
        }
        cst::Stmt::ExprStmt(it) => {
            let expr_node = match it.expr() {
                Some(expr) => expr,
                None => {
                    ctx.push_error(
                        Some(it.syntax().text_range()),
                        "Expression statement missing expression",
                    );
                    return body;
                }
            };
            let value = match lower_expr(ctx, expr_node) {
                Some(expr) => expr,
                None => return body,
            };
            ast::Expr::ELet {
                pat: ast::Pat::PWild,
                value: Box::new(value),
                body: Box::new(body),
            }
        }
    }
}

fn lower_param(ctx: &mut LowerCtx, node: cst::Param) -> Option<(ast::Lident, ast::Ty)> {
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
    Some((ast::Lident(name), ty))
}

fn lower_closure_param(ctx: &mut LowerCtx, node: cst::ClosureParam) -> Option<ast::ClosureParam> {
    let Some(name_token) = node.lident() else {
        ctx.push_error(
            Some(node.syntax().text_range()),
            "Closure parameter missing name",
        );
        return None;
    };

    let name = ast::Lident(name_token.to_string());
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
    match node {
        cst::Expr::UnitExpr(_) => {
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(node_range),
                    "Cannot apply arguments to unit expression",
                );
                return None;
            }
            Some(ast::Expr::EUnit)
        }
        cst::Expr::BoolExpr(it) => {
            let Some(token) = it.value() else {
                ctx.push_error(Some(it.syntax().text_range()), "BoolExpr has no value");
                return None;
            };
            let value = token.to_string();
            let expr = match value.as_str() {
                "true" => ast::Expr::EBool { value: true },
                "false" => ast::Expr::EBool { value: false },
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
            let Some(token) = it.value() else {
                ctx.push_error(Some(it.syntax().text_range()), "IntExpr has no value");
                return None;
            };
            let text = token.to_string();
            let value = match text.parse::<i32>() {
                Ok(value) => value,
                Err(_) => {
                    ctx.push_error(
                        Some(token.text_range()),
                        format!("Invalid integer literal: {}", text),
                    );
                    return None;
                }
            };
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to int literal",
                );
                return None;
            }
            Some(ast::Expr::EInt { value })
        }
        cst::Expr::StrExpr(it) => {
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
            })
        }
        cst::Expr::CallExpr(it) => {
            let args: Vec<ast::Expr> = it
                .arg_list()
                .map(|list| list.args().flat_map(|arg| lower_arg(ctx, arg)).collect())
                .unwrap_or_default();
            if let Some(callee) = support::child::<cst::Expr>(it.syntax()) {
                return match callee {
                    cst::Expr::LidentExpr(lident_expr) => {
                        let name = lident_expr.lident_token().unwrap().to_string();
                        let var_expr = ast::Expr::EVar {
                            name: ast::Lident(name),
                            astptr: MySyntaxNodePtr::new(lident_expr.syntax()),
                        };
                        let call = ast::Expr::ECall {
                            func: Box::new(var_expr),
                            args,
                        };
                        apply_trailing_args(
                            ctx,
                            call,
                            trailing_args,
                            Some(it.syntax().text_range()),
                        )
                    }
                    cst::Expr::UidentExpr(uident_expr) => {
                        let name = uident_expr.uident().unwrap().to_string();
                        let constr = ast::Expr::EConstr {
                            vcon: ast::Uident::new(&name),
                            args,
                        };
                        apply_trailing_args(
                            ctx,
                            constr,
                            trailing_args,
                            Some(it.syntax().text_range()),
                        )
                    }
                    other => {
                        if matches!(other, cst::Expr::CallExpr(_)) {
                            let func_expr = lower_expr(ctx, other)?;
                            let call = ast::Expr::ECall {
                                func: Box::new(func_expr),
                                args,
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
            })
        }
        cst::Expr::IfExpr(it) => {
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
            })
        }
        cst::Expr::WhileExpr(it) => {
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
            })
        }
        cst::Expr::StructLiteralExpr(it) => {
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to struct literal",
                );
                return None;
            }
            let name = it.uident()?.to_string();
            let fields = it
                .field_list()
                .map(|list| {
                    list.fields()
                        .flat_map(|field| {
                            let fname = field.lident()?.to_string();
                            let expr = field.expr().and_then(|expr| lower_expr(ctx, expr))?;
                            Some((ast::Lident(fname), expr))
                        })
                        .collect()
                })
                .unwrap_or_default();
            Some(ast::Expr::EStructLiteral {
                name: ast::Uident::new(&name),
                fields,
            })
        }
        cst::Expr::ArrayLiteralExpr(it) => {
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to array literal",
                );
                return None;
            }
            let items = it.exprs().flat_map(|expr| lower_expr(ctx, expr)).collect();
            Some(ast::Expr::EArray { items })
        }
        cst::Expr::UidentExpr(it) => {
            let name = it.uident().unwrap().to_string();
            let expr = ast::Expr::EConstr {
                vcon: ast::Uident::new(&name),
                args: vec![],
            };
            apply_trailing_args(ctx, expr, trailing_args, Some(it.syntax().text_range()))
        }
        cst::Expr::LidentExpr(it) => {
            let name = it.lident_token().unwrap().to_string();
            let expr = ast::Expr::EVar {
                name: ast::Lident(name),
                astptr: MySyntaxNodePtr::new(it.syntax()),
            };
            apply_trailing_args(ctx, expr, trailing_args, Some(it.syntax().text_range()))
        }
        cst::Expr::TupleExpr(it) => {
            if !trailing_args.is_empty() {
                ctx.push_error(
                    Some(it.syntax().text_range()),
                    "Cannot apply arguments to tuple literal",
                );
                return None;
            }
            let items = it.exprs().flat_map(|expr| lower_expr(ctx, expr)).collect();
            Some(ast::Expr::ETuple { items })
        }
        cst::Expr::PrefixExpr(it) => {
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
                    op: ast::UnaryOp::Neg,
                    expr: Box::new(expr),
                },
                MySyntaxKind::Bang => ast::Expr::EUnary {
                    op: ast::UnaryOp::Not,
                    expr: Box::new(expr),
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
                        op: ast::BinaryOp::Add,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    })
                }
                MySyntaxKind::Minus => {
                    let rhs = lower_expr_with_args(ctx, rhs_cst, trailing_args)?;
                    Some(ast::Expr::EBinary {
                        op: ast::BinaryOp::Sub,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    })
                }
                MySyntaxKind::Star => {
                    let rhs = lower_expr_with_args(ctx, rhs_cst, trailing_args)?;
                    Some(ast::Expr::EBinary {
                        op: ast::BinaryOp::Mul,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    })
                }
                MySyntaxKind::Slash => {
                    let rhs = lower_expr_with_args(ctx, rhs_cst, trailing_args)?;
                    Some(ast::Expr::EBinary {
                        op: ast::BinaryOp::Div,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    })
                }
                MySyntaxKind::AndAnd => {
                    let rhs = lower_expr_with_args(ctx, rhs_cst, trailing_args)?;
                    Some(ast::Expr::EBinary {
                        op: ast::BinaryOp::And,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    })
                }
                MySyntaxKind::OrOr => {
                    let rhs = lower_expr_with_args(ctx, rhs_cst, trailing_args)?;
                    Some(ast::Expr::EBinary {
                        op: ast::BinaryOp::Or,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    })
                }
                MySyntaxKind::Dot => {
                    if !trailing_args.is_empty() {
                        ctx.push_error(
                            Some(it.syntax().text_range()),
                            "Cannot apply arguments to field access expression",
                        );
                        return None;
                    }
                    match rhs_cst {
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
                            })
                        }
                        other => {
                            ctx.push_error(
                                Some(other.syntax().text_range()),
                                "Unsupported field access expression",
                            );
                            None
                        }
                    }
                }
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
        ast::Expr::EVar { name, astptr } => Some(ast::Expr::ECall {
            func: Box::new(ast::Expr::EVar { name, astptr }),
            args: trailing_args,
        }),
        ast::Expr::ECall { func, args } => {
            let mut result = ast::Expr::ECall { func, args };
            for arg in trailing_args {
                result = ast::Expr::ECall {
                    func: Box::new(result),
                    args: vec![arg],
                };
            }
            Some(result)
        }
        ast::Expr::EConstr { vcon, mut args } => {
            args.extend(trailing_args);
            Some(ast::Expr::EConstr { vcon, args })
        }
        ast::Expr::EBinary { op, lhs, rhs } => {
            let rhs = apply_trailing_args(ctx, *rhs, trailing_args, range)?;
            Some(ast::Expr::EBinary {
                op,
                lhs,
                rhs: Box::new(rhs),
            })
        }
        ast::Expr::EUnary { op, expr } => {
            let expr = apply_trailing_args(ctx, *expr, trailing_args, range)?;
            Some(ast::Expr::EUnary {
                op,
                expr: Box::new(expr),
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
    match node {
        cst::Pattern::VarPat(it) => {
            let name = it.lident().unwrap().to_string();
            Some(ast::Pat::PVar {
                name: ast::Lident(name),
                astptr: MySyntaxNodePtr::new(it.syntax()),
            })
        }
        cst::Pattern::UnitPat(_) => Some(ast::Pat::PUnit),
        cst::Pattern::BoolPat(it) => {
            let value = it.value()?.to_string();

            match value.as_str() {
                "true" => Some(ast::Pat::PBool { value: true }),
                "false" => Some(ast::Pat::PBool { value: false }),
                _ => {
                    ctx.push_error(
                        Some(it.syntax().text_range()),
                        format!("Invalid boolean pattern: {}", value),
                    );
                    None
                }
            }
        }
        cst::Pattern::IntPat(it) => {
            let value = it.value()?.to_string();
            match value.parse::<i32>() {
                Ok(value) => Some(ast::Pat::PInt { value }),
                Err(_) => {
                    ctx.push_error(
                        Some(it.syntax().text_range()),
                        format!("Invalid integer pattern: {}", value),
                    );
                    None
                }
            }
        }
        cst::Pattern::StringPat(it) => {
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
            })
        }
        cst::Pattern::ConstrPat(it) => {
            let name = it.uident().unwrap().to_string();
            if let Some(field_list) = it.field_list() {
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
                            ctx.push_error(
                                Some(field.syntax().text_range()),
                                "Struct pattern field missing pattern",
                            );
                            return None;
                        }
                    };
                    fields.push((ast::Lident(fname), pat));
                }
                Some(ast::Pat::PStruct {
                    name: ast::Uident::new(&name),
                    fields,
                })
            } else {
                let pats = it.patterns().flat_map(|pat| lower_pat(ctx, pat)).collect();
                Some(ast::Pat::PConstr {
                    vcon: ast::Uident::new(&name),
                    args: pats,
                })
            }
        }
        cst::Pattern::TuplePat(it) => {
            let items = it.patterns().flat_map(|pat| lower_pat(ctx, pat)).collect();
            Some(ast::Pat::PTuple { pats: items })
        }
        cst::Pattern::WildPat(_) => Some(ast::Pat::PWild),
    }
}
