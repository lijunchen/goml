use crate::ast;

use ::cst::cst::CstNode;
use cst::cst;
use parser::syntax::MySyntaxNodePtr;

pub fn lower(node: cst::File) -> Option<ast::File> {
    let items = node.items().flat_map(lower_item).collect();
    let ast = ast::File { toplevels: items };
    Some(ast)
}

fn lower_item(node: cst::Item) -> Option<ast::Item> {
    match node {
        cst::Item::Enum(it) => Some(ast::Item::EnumDef(lower_enum(it)?)),
        cst::Item::Trait(it) => Some(ast::Item::TraitDef(lower_trait(it)?)),
        cst::Item::Impl(it) => Some(ast::Item::ImplBlock(lower_impl_block(it)?)),
        cst::Item::Fn(it) => Some(ast::Item::Fn(lower_fn(it)?)),
    }
}

fn lower_enum(node: cst::Enum) -> Option<ast::EnumDef> {
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
    let variants = node
        .variant_list()
        .unwrap_or_else(|| panic!("Enum {} has no variants", name))
        .variants()
        .flat_map(lower_variant)
        .collect();
    Some(ast::EnumDef {
        name: ast::Uident::new(&name),
        generics,
        variants,
    })
}

fn lower_trait(node: cst::Trait) -> Option<ast::TraitDef> {
    let name = node.uident().unwrap().to_string();
    let methods = node
        .trait_method_list()
        .unwrap_or_else(|| panic!("Trait {} has no methods", name))
        .methods()
        .flat_map(lower_trait_method)
        .collect();
    Some(ast::TraitDef {
        name: ast::Uident::new(&name),
        method_sigs: methods,
    })
}

fn lower_trait_method(node: cst::TraitMethod) -> Option<ast::TraitMethodSignature> {
    let name = node.lident().unwrap().to_string();
    let params = node
        .type_list()
        .unwrap_or_else(|| panic!("TraitMethod {} has no params", name))
        .types()
        .flat_map(lower_ty)
        .collect();
    let ret_ty = match node.return_type() {
        None => ast::Ty::TUnit,
        Some(it) => {
            lower_ty(it).unwrap_or_else(|| panic!("TraitMethod {} has no return type", name))
        }
    };
    Some(ast::TraitMethodSignature {
        name: ast::Lident(name),
        params,
        ret_ty,
    })
}

fn lower_impl_block(node: cst::Impl) -> Option<ast::ImplBlock> {
    let trait_name = node.uident().unwrap().to_string();
    let for_type = node
        .for_type()
        .and_then(lower_ty)
        .unwrap_or_else(|| panic!("ImplBlock {} has no for type", trait_name));
    let methods: Vec<ast::Fn> = node.functions().flat_map(lower_fn).collect();
    Some(ast::ImplBlock {
        trait_name: ast::Uident::new(&trait_name),
        for_type,
        methods,
    })
}

fn lower_variant(node: cst::Variant) -> Option<(ast::Uident, Vec<ast::Ty>)> {
    let name = node.uident().unwrap().to_string();
    let typs = match node.type_list() {
        None => vec![],
        Some(xs) => xs.types().flat_map(lower_ty).collect(),
    };
    Some((ast::Uident::new(&name), typs))
}

fn lower_ty(node: cst::Type) -> Option<ast::Ty> {
    match node {
        cst::Type::UnitTy(_) => Some(ast::Ty::TUnit),
        cst::Type::BoolTy(_) => Some(ast::Ty::TBool),
        cst::Type::IntTy(_) => Some(ast::Ty::TInt),
        cst::Type::StringTy(_) => Some(ast::Ty::TString),
        cst::Type::TupleTy(it) => {
            let typs = it.type_list()?.types().flat_map(lower_ty).collect();
            Some(ast::Ty::TTuple { typs })
        }
        cst::Type::TAppTy(it) => {
            let name = it.uident().unwrap().to_string();
            let args: Vec<ast::Ty> = it
                .type_param_list()
                .map(|list| list.types().flat_map(lower_ty).collect())
                .unwrap_or_default();
            Some(ast::Ty::TApp {
                name: ast::Uident::new(&name),
                args,
            })
        }

        cst::Type::FuncTy(..) => todo!(),
    }
}

fn lower_fn(node: cst::Fn) -> Option<ast::Fn> {
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
    let params = node
        .param_list()
        .unwrap_or_else(|| panic!("Fn {} has no params", name))
        .params()
        .flat_map(lower_param)
        .collect();
    let ret_ty = node.return_type().and_then(lower_ty);
    let body = node
        .block()
        .and_then(lower_block)
        .unwrap_or_else(|| panic!("Fn {} has no body", name));
    Some(ast::Fn {
        name: ast::Lident(name),
        generics,
        params,
        ret_ty,
        body,
    })
}

fn lower_block(node: cst::Block) -> Option<ast::Expr> {
    let cst_e = node.expr();

    cst_e.and_then(lower_expr)
}

fn lower_param(node: cst::Param) -> Option<(ast::Lident, ast::Ty)> {
    let name = node.lident().unwrap().to_string();
    let ty = node
        .ty()
        .and_then(lower_ty)
        .unwrap_or_else(|| panic!("Param {} has no type", name));
    Some((ast::Lident(name), ty))
}

fn lower_expr(node: cst::Expr) -> Option<ast::Expr> {
    match node {
        cst::Expr::UnitExpr(_) => Some(ast::Expr::EUnit),
        cst::Expr::BoolExpr(it) => {
            let value = it
                .value()
                .unwrap_or_else(|| panic!("BoolExpr has no value"))
                .to_string();

            match value.as_str() {
                "true" => Some(ast::Expr::EBool { value: true }),
                "false" => Some(ast::Expr::EBool { value: false }),
                _ => unreachable!(),
            }
        }
        cst::Expr::IntExpr(it) => {
            let value = it
                .value()
                .unwrap_or_else(|| panic!("IntExpr has no value"))
                .to_string();
            let value = value.parse::<i32>().ok()?;
            Some(ast::Expr::EInt { value })
        }
        cst::Expr::StrExpr(it) => {
            let value = it
                .value()
                .unwrap_or_else(|| panic!("StrExpr has no value"))
                .to_string();
            // parse string literal
            let value = value
                .strip_prefix('"')
                .and_then(|s| s.strip_suffix('"'))
                .unwrap_or_else(|| panic!("StrExpr has no value"))
                .to_string();
            Some(ast::Expr::EString { value })
        }
        cst::Expr::CallExpr(it) => {
            let l = it.l_name();
            let u = it.u_name();

            if l.is_some() {
                let func = l.unwrap().to_string();
                let args = it
                    .arg_list()
                    .unwrap_or_else(|| panic!("PrimExpr has no args"))
                    .args()
                    .flat_map(lower_arg)
                    .collect();
                return Some(ast::Expr::ECall {
                    func: ast::Lident(func),
                    args,
                });
            }

            if u.is_some() {
                let func = u.unwrap().to_string();
                let args = it
                    .arg_list()
                    .unwrap_or_else(|| panic!("PrimExpr has no args"))
                    .args()
                    .flat_map(lower_arg)
                    .collect();
                return Some(ast::Expr::EConstr {
                    vcon: ast::Uident::new(&func),
                    args,
                });
            }

            unreachable!()
        }
        cst::Expr::MatchExpr(it) => {
            let expr = it
                .expr()
                .and_then(lower_expr)
                .unwrap_or_else(|| panic!("MatchExpr has no expr"));
            let arms = it
                .match_arm_list()
                .unwrap_or_else(|| panic!("MatchExpr has no arms"))
                .arms()
                .flat_map(lower_arm)
                .collect();
            Some(ast::Expr::EMatch {
                expr: Box::new(expr),
                arms,
            })
        }
        cst::Expr::UidentExpr(it) => {
            let name = it.uident().unwrap().to_string();
            Some(ast::Expr::EConstr {
                vcon: ast::Uident::new(&name),
                args: vec![],
            })
        }
        cst::Expr::LidentExpr(it) => {
            let name = it.lident_token().unwrap().to_string();
            Some(ast::Expr::EVar {
                name: ast::Lident(name),
                astptr: MySyntaxNodePtr::new(it.syntax()),
            })
        }
        cst::Expr::TupleExpr(it) => {
            let items = it.exprs().flat_map(lower_expr).collect();
            Some(ast::Expr::ETuple { items })
        }
        cst::Expr::LetExpr(it) => {
            let pat = it
                .pattern()
                .and_then(lower_pat)
                .unwrap_or_else(|| panic!("LetExpr has no pattern"));
            let value = it
                .value()
                .unwrap_or_else(|| panic!("LetExpr has no value"))
                .expr()
                .and_then(lower_expr)
                .unwrap_or_else(|| panic!("failed to lower value {:#?}", it.value()));
            let body = it
                .body()
                .unwrap_or_else(|| panic!("LetExpr has no body"))
                .expr()
                .and_then(lower_expr)
                .unwrap_or_else(|| panic!("failed to lower body"));
            Some(ast::Expr::ELet {
                pat,
                value: Box::new(value),
                body: Box::new(body),
            })
        }
        cst::Expr::BinaryExpr(_it) => {
            todo!()
        }
    }
}

fn lower_arg(node: cst::Arg) -> Option<ast::Expr> {
    lower_expr(node.expr().unwrap_or_else(|| panic!("Arg has no expr")))
}

fn lower_arm(node: cst::MatchArm) -> Option<ast::Arm> {
    let pat = node.pattern().and_then(lower_pat)?;
    let expr = node.expr().and_then(lower_expr)?;
    Some(ast::Arm { pat, body: expr })
}

fn lower_pat(node: cst::Pattern) -> Option<ast::Pat> {
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
                _ => unreachable!(),
            }
        }
        cst::Pattern::ConstrPat(it) => {
            let name = it.uident().unwrap().to_string();
            let pats = it.patterns().flat_map(lower_pat).collect();
            Some(ast::Pat::PConstr {
                vcon: ast::Uident::new(&name),
                args: pats,
            })
        }
        cst::Pattern::TuplePat(it) => {
            let items = it.patterns().flat_map(lower_pat).collect();
            Some(ast::Pat::PTuple { pats: items })
        }
        cst::Pattern::WildPat(_) => Some(ast::Pat::PWild),
    }
}
