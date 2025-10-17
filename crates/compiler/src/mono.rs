use super::core;
use crate::env::{EnumDef, Env, StructDef};
use crate::tast::{self, Constructor, Ty};
use crate::type_encoding::encode_ty;
use ast::ast::Uident;
use indexmap::{IndexMap, IndexSet};
use std::collections::VecDeque;

// Monomorphize Core IR by specializing generic functions per concrete call site.
// Produces a file containing only monomorphic functions reachable from monomorphic roots.
pub fn mono(env: &mut Env, file: core::File) -> core::File {
    // Build original function map
    let mut orig_fns: IndexMap<String, core::Fn> = IndexMap::new();
    for f in file.toplevels.into_iter() {
        orig_fns.insert(f.name.clone(), f);
    }

    // Helpers
    fn has_tparam(ty: &Ty) -> bool {
        match ty {
            Ty::TParam { .. } => true,
            Ty::TVar(..) => false,
            Ty::TUnit | Ty::TBool | Ty::TInt | Ty::TString => false,
            Ty::TTuple { typs } => typs.iter().any(has_tparam),
            Ty::TCon { .. } => false,
            Ty::TApp { ty, args } => has_tparam(ty.as_ref()) || args.iter().any(has_tparam),
            Ty::TArray { elem, .. } => has_tparam(elem),
            Ty::TFunc { params, ret_ty } => params.iter().any(has_tparam) || has_tparam(ret_ty),
        }
    }

    fn update_constructor_type(constructor: &Constructor, new_ty: &Ty) -> Constructor {
        match (constructor, new_ty) {
            (Constructor::Enum(enum_constructor), Ty::TCon { name }) => {
                let ident = Uident::new(name);
                Constructor::Enum(tast::EnumConstructor {
                    type_name: ident,
                    variant: enum_constructor.variant.clone(),
                    index: enum_constructor.index,
                })
            }
            (Constructor::Enum(enum_constructor), Ty::TApp { ty, .. }) => {
                let base = ty.get_constr_name_unsafe();
                Constructor::Enum(tast::EnumConstructor {
                    type_name: Uident::new(&base),
                    variant: enum_constructor.variant.clone(),
                    index: enum_constructor.index,
                })
            }
            (Constructor::Struct(_), Ty::TCon { name }) => {
                let ident = Uident::new(name);
                Constructor::Struct(tast::StructConstructor { type_name: ident })
            }
            (Constructor::Struct(_), Ty::TApp { ty, .. }) => {
                let base = ty.get_constr_name_unsafe();
                Constructor::Struct(tast::StructConstructor {
                    type_name: Uident::new(&base),
                })
            }
            _ => constructor.clone(),
        }
    }

    fn fn_is_generic(f: &core::Fn) -> bool {
        f.params.iter().any(|(_, t)| has_tparam(t)) || has_tparam(&f.ret_ty)
    }

    type Subst = IndexMap<String, Ty>;

    fn subst_ty(ty: &Ty, s: &Subst) -> Ty {
        match ty {
            Ty::TParam { name } => s.get(name).cloned().unwrap_or_else(|| ty.clone()),
            Ty::TVar(..) => ty.clone(),
            Ty::TUnit | Ty::TBool | Ty::TInt | Ty::TString => ty.clone(),
            Ty::TTuple { typs } => Ty::TTuple {
                typs: typs.iter().map(|t| subst_ty(t, s)).collect(),
            },
            Ty::TCon { name } => Ty::TCon { name: name.clone() },
            Ty::TApp { ty, args } => Ty::TApp {
                ty: Box::new(subst_ty(ty, s)),
                args: args.iter().map(|t| subst_ty(t, s)).collect(),
            },
            Ty::TArray { len, elem } => Ty::TArray {
                len: *len,
                elem: Box::new(subst_ty(elem, s)),
            },
            Ty::TFunc { params, ret_ty } => Ty::TFunc {
                params: params.iter().map(|t| subst_ty(t, s)).collect(),
                ret_ty: Box::new(subst_ty(ret_ty, s)),
            },
        }
    }

    // Create stable key for a substitution (sorted by type parameter name)
    fn subst_key(s: &Subst) -> String {
        let mut entries = s.iter().collect::<Vec<_>>();
        entries.sort_by(|a, b| a.0.cmp(b.0));
        entries
            .into_iter()
            .map(|(k, v)| format!("{}={}", k, encode_ty(v)))
            .collect::<Vec<_>>()
            .join(";")
    }

    // Derive a specialized function name based on substitution
    fn spec_name_for(orig: &str, s: &Subst) -> String {
        if s.is_empty() {
            return orig.to_string();
        }
        let mut pairs = s.iter().collect::<Vec<_>>();
        pairs.sort_by(|a, b| a.0.cmp(b.0));
        let suffix = pairs
            .into_iter()
            .map(|(k, v)| format!("{}_{}", k, encode_ty(v)))
            .collect::<Vec<_>>()
            .join("__");
        format!("{}__{}", orig, suffix)
    }

    // Unify template type (may contain TParam) with actual type (concrete), filling `subst`.
    fn unify(template: &Ty, actual: &Ty, subst: &mut Subst) -> Result<(), String> {
        match (template, actual) {
            (Ty::TParam { name }, a) => {
                if let Some(prev) = subst.get(name) {
                    if prev != a {
                        return Err(format!(
                            "conflicting bindings for {}: prev={:?}, new={:?}",
                            name, prev, a
                        ));
                    }
                    Ok(())
                } else {
                    subst.insert(name.clone(), a.clone());
                    Ok(())
                }
            }
            (Ty::TUnit, Ty::TUnit)
            | (Ty::TBool, Ty::TBool)
            | (Ty::TInt, Ty::TInt)
            | (Ty::TString, Ty::TString) => Ok(()),
            (Ty::TTuple { typs: l }, Ty::TTuple { typs: r }) => {
                if l.len() != r.len() {
                    return Err("tuple length mismatch".to_string());
                }
                for (a, b) in l.iter().zip(r.iter()) {
                    unify(a, b, subst)?;
                }
                Ok(())
            }
            (Ty::TCon { name: ln }, Ty::TCon { name: rn }) => {
                if ln != rn {
                    return Err("type constructor mismatch".to_string());
                }
                Ok(())
            }
            (Ty::TApp { ty: lt, args: la }, Ty::TApp { ty: rt, args: ra }) => {
                if la.len() != ra.len() {
                    return Err("type constructor mismatch".to_string());
                }
                unify(lt, rt, subst)?;
                for (a, b) in la.iter().zip(ra.iter()) {
                    unify(a, b, subst)?;
                }
                Ok(())
            }
            (Ty::TArray { len: ll, elem: le }, Ty::TArray { len: rl, elem: re }) => {
                if ll != rl {
                    return Err("array length mismatch".to_string());
                }
                unify(le, re, subst)
            }
            (
                Ty::TFunc {
                    params: lp,
                    ret_ty: lr,
                },
                Ty::TFunc {
                    params: rp,
                    ret_ty: rr,
                },
            ) => {
                if lp.len() != rp.len() {
                    return Err("function arity mismatch".to_string());
                }
                for (a, b) in lp.iter().zip(rp.iter()) {
                    unify(a, b, subst)?;
                }
                unify(lr, rr, subst)
            }
            _ => Err(format!("cannot unify {:?} with {:?}", template, actual)),
        }
    }

    // State used during transformation
    struct Ctx {
        orig_fns: IndexMap<String, core::Fn>,
        // map (orig_name, subst_key) -> spec_name
        instances: IndexMap<(String, String), String>,
        queued: IndexSet<(String, String)>,
        out: Vec<core::Fn>,
        work: VecDeque<(String, Subst, String)>,
    }

    impl Ctx {
        fn new(orig_fns: IndexMap<String, core::Fn>) -> Self {
            Self {
                orig_fns,
                instances: IndexMap::new(),
                queued: IndexSet::new(),
                out: Vec::new(),
                work: VecDeque::new(),
            }
        }

        // Ensure an instance exists (or is queued) and return its specialized name
        fn ensure_instance(&mut self, name: &str, s: Subst) -> String {
            let key = subst_key(&s);
            let k = (name.to_string(), key.clone());
            if let Some(n) = self.instances.get(&k) {
                return n.clone();
            }
            let spec = spec_name_for(name, &s);
            self.instances
                .insert((name.to_string(), key.clone()), spec.clone());
            if !self.queued.contains(&(name.to_string(), key.clone())) {
                self.queued.insert((name.to_string(), key));
                self.work.push_back((name.to_string(), s, spec.clone()));
            }
            spec
        }
    }

    // Transform an expression under a given substitution; queue any needed instances
    fn mono_expr(ctx: &mut Ctx, e: &core::Expr, s: &Subst) -> core::Expr {
        match e.clone() {
            core::Expr::EVar { name, ty } => core::Expr::EVar {
                name,
                ty: subst_ty(&ty, s),
            },
            core::Expr::EUnit { ty } => core::Expr::EUnit {
                ty: subst_ty(&ty, s),
            },
            core::Expr::EBool { value, ty } => core::Expr::EBool {
                value,
                ty: subst_ty(&ty, s),
            },
            core::Expr::EInt { value, ty } => core::Expr::EInt {
                value,
                ty: subst_ty(&ty, s),
            },
            core::Expr::EString { value, ty } => core::Expr::EString {
                value,
                ty: subst_ty(&ty, s),
            },
            core::Expr::EConstr {
                constructor,
                args,
                ty,
            } => {
                let new_ty = subst_ty(&ty, s);
                let new_constructor = update_constructor_type(&constructor, &new_ty);
                core::Expr::EConstr {
                    constructor: new_constructor,
                    args: args.iter().map(|a| mono_expr(ctx, a, s)).collect(),
                    ty: new_ty,
                }
            }
            core::Expr::ETuple { items, ty } => core::Expr::ETuple {
                items: items.iter().map(|a| mono_expr(ctx, a, s)).collect(),
                ty: subst_ty(&ty, s),
            },
            core::Expr::EArray { items, ty } => core::Expr::EArray {
                items: items.iter().map(|a| mono_expr(ctx, a, s)).collect(),
                ty: subst_ty(&ty, s),
            },
            core::Expr::EClosure { .. } => {
                panic!("lambda lift should have removed closures before monomorphization");
            }
            core::Expr::ELet {
                name,
                value,
                body,
                ty,
            } => core::Expr::ELet {
                name,
                value: Box::new(mono_expr(ctx, &value, s)),
                body: Box::new(mono_expr(ctx, &body, s)),
                ty: subst_ty(&ty, s),
            },
            core::Expr::EMatch {
                expr,
                arms,
                default,
                ty,
            } => core::Expr::EMatch {
                expr: Box::new(mono_expr(ctx, &expr, s)),
                arms: arms
                    .iter()
                    .map(|arm| core::Arm {
                        lhs: mono_expr(ctx, &arm.lhs, s),
                        body: mono_expr(ctx, &arm.body, s),
                    })
                    .collect(),
                default: default.map(|d| Box::new(mono_expr(ctx, &d, s))),
                ty: subst_ty(&ty, s),
            },
            core::Expr::EIf {
                cond,
                then_branch,
                else_branch,
                ty,
            } => core::Expr::EIf {
                cond: Box::new(mono_expr(ctx, &cond, s)),
                then_branch: Box::new(mono_expr(ctx, &then_branch, s)),
                else_branch: Box::new(mono_expr(ctx, &else_branch, s)),
                ty: subst_ty(&ty, s),
            },
            core::Expr::EConstrGet {
                expr,
                constructor,
                field_index,
                ty,
            } => {
                let new_expr = mono_expr(ctx, &expr, s);
                let scrut_ty = subst_ty(&expr.get_ty(), s);
                let new_constructor = update_constructor_type(&constructor, &scrut_ty);
                core::Expr::EConstrGet {
                    expr: Box::new(new_expr),
                    constructor: new_constructor,
                    field_index,
                    ty: subst_ty(&ty, s),
                }
            }
            core::Expr::ECall { func, args, ty } => {
                let new_args: Vec<core::Expr> = args.iter().map(|a| mono_expr(ctx, a, s)).collect();
                let new_ty = subst_ty(&ty, s);

                // If function is not in current file (runtime/built-in), leave as is
                let Some(callee) = ctx.orig_fns.get(&func) else {
                    return core::Expr::ECall {
                        func,
                        args: new_args,
                        ty: new_ty,
                    };
                };

                if !fn_is_generic(callee) {
                    return core::Expr::ECall {
                        func,
                        args: new_args,
                        ty: new_ty,
                    };
                }

                // Derive concrete substitution for this call by unifying callee param/ret with arg/call types
                let mut call_subst: Subst = IndexMap::new();
                let callee_param_tys = callee.params.iter().map(|(_, t)| t).collect::<Vec<_>>();
                let arg_tys = new_args.iter().map(|a| a.get_ty()).collect::<Vec<_>>();
                for (pt, at) in callee_param_tys.iter().zip(arg_tys.iter()) {
                    if let Err(e) = unify(pt, at, &mut call_subst) {
                        panic!("monomorphization unification failed for {}: {}", func, e);
                    }
                }
                if let Err(e) = unify(&callee.ret_ty, &new_ty, &mut call_subst) {
                    panic!(
                        "monomorphization return type unification failed for {}: {}",
                        func, e
                    );
                }

                // Ensure the substitution yields concrete types (no TParam in bindings)
                if call_subst.values().any(has_tparam) {
                    // If we cannot determine concrete types here, keep the call as-is.
                    // This should not happen for reachable instances from monomorphic roots.
                    return core::Expr::ECall {
                        func,
                        args: new_args,
                        ty: new_ty,
                    };
                }

                let spec = ctx.ensure_instance(&func, call_subst);
                core::Expr::ECall {
                    func: spec,
                    args: new_args,
                    ty: new_ty,
                }
            }
            core::Expr::EProj { tuple, index, ty } => core::Expr::EProj {
                tuple: Box::new(mono_expr(ctx, &tuple, s)),
                index,
                ty: subst_ty(&ty, s),
            },
        }
    }

    let mut ctx = Ctx::new(orig_fns);

    // Seed worklist with non-generic top-level functions
    // collect names first to avoid borrowing ctx immutably while mutating
    let seed_names: Vec<String> = ctx
        .orig_fns
        .iter()
        .filter(|(_, f)| !fn_is_generic(f))
        .map(|(n, _)| n.clone())
        .collect();
    for name in seed_names.into_iter() {
        let _ = ctx.ensure_instance(&name, Subst::new());
    }

    // Process all queued instances
    while let Some((orig_name, s, spec_name)) = ctx.work.pop_front() {
        // Limit immutable borrow scope to clone necessary pieces
        let (orig_params, orig_ret, orig_body) = {
            let ofn = ctx
                .orig_fns
                .get(&orig_name)
                .unwrap_or_else(|| panic!("unknown function: {}", orig_name));
            (ofn.params.clone(), ofn.ret_ty.clone(), ofn.body.clone())
        };

        let new_params = orig_params
            .iter()
            .map(|(n, t)| (n.clone(), subst_ty(t, &s)))
            .collect();
        let new_ret = subst_ty(&orig_ret, &s);
        let new_body = mono_expr(&mut ctx, &orig_body, &s);

        ctx.out.push(core::Fn {
            name: spec_name,
            params: new_params,
            ret_ty: new_ret,
            body: new_body,
        });
    }

    // Phase 2: monomorphize enum type applications in types and update env
    struct TypeMono<'a> {
        env: &'a mut Env,
        // map generic (name, args) to new concrete Uident
        map: IndexMap<(String, Vec<Ty>), Uident>,
        // snapshot of original generic enum defs
        enum_base: IndexMap<Uident, EnumDef>,
        struct_base: IndexMap<Uident, StructDef>,
    }

    impl<'a> TypeMono<'a> {
        fn new(env: &'a mut Env) -> Self {
            let enum_base = env.enums.clone();
            let struct_base = env.structs.clone();
            Self {
                env,
                map: IndexMap::new(),
                enum_base,
                struct_base,
            }
        }

        fn ensure_instance(&mut self, name: &str, args: &[Ty]) -> Uident {
            let key = (name.to_string(), args.to_vec());
            if let Some(u) = self.map.get(&key) {
                return u.clone();
            }
            // Create a fresh concrete type name
            let suffix = if args.is_empty() {
                "".to_string()
            } else {
                format!(
                    "__{}",
                    args.iter().map(encode_ty).collect::<Vec<_>>().join("__")
                )
            };
            let new_name = Uident::new(&format!("{}{}", name, suffix));
            self.map.insert(key.clone(), new_name.clone());

            let ident = Uident::new(name);

            if let Some(generic_def) = self.enum_base.get(&ident) {
                // Build substitution from generics to args
                let mut subst: IndexMap<String, Ty> = IndexMap::new();
                if generic_def.generics.len() != args.len() {
                    panic!(
                        "enum generic argument length mismatch for {}: expected {}, got {}",
                        name,
                        generic_def.generics.len(),
                        args.len()
                    );
                }
                for (g, a) in generic_def.generics.iter().zip(args.iter()) {
                    subst.insert(g.0.clone(), a.clone());
                }

                // Substitute variant field types and also collapse nested enum/struct apps
                let mut new_variants: Vec<(Uident, Vec<Ty>)> = Vec::new();
                // Clone needed data to limit immutable borrow scope
                let variants = generic_def.variants.clone();
                for (vname, vfields) in variants.into_iter() {
                    let mut fields2 = Vec::new();
                    for t in vfields.into_iter() {
                        let t1 = subst_ty(&t, &subst);
                        let t2 = self.collapse_type_apps(&t1);
                        fields2.push(t2);
                    }
                    new_variants.push((vname.clone(), fields2));
                }

                let new_def = EnumDef {
                    name: new_name.clone(),
                    generics: vec![],
                    variants: new_variants,
                };
                self.env.enums.insert(new_name.clone(), new_def);
            } else if let Some(generic_def) = self.struct_base.get(&ident) {
                let mut subst: IndexMap<String, Ty> = IndexMap::new();
                if generic_def.generics.len() != args.len() {
                    panic!(
                        "struct generic argument length mismatch for {}: expected {}, got {}",
                        name,
                        generic_def.generics.len(),
                        args.len()
                    );
                }
                for (g, a) in generic_def.generics.iter().zip(args.iter()) {
                    subst.insert(g.0.clone(), a.clone());
                }

                let mut new_fields = Vec::new();
                let fields = generic_def.fields.clone();
                for (fname, fty) in fields.into_iter() {
                    let ty1 = subst_ty(&fty, &subst);
                    let ty2 = self.collapse_type_apps(&ty1);
                    new_fields.push((fname.clone(), ty2));
                }

                let new_def = StructDef {
                    name: new_name.clone(),
                    generics: vec![],
                    fields: new_fields,
                };
                self.env.structs.insert(new_name.clone(), new_def);
            } else {
                // Unknown type constructor; just return synthesized name without registering a def
            }
            new_name
        }

        fn collapse_type_apps(&mut self, ty: &Ty) -> Ty {
            match ty {
                Ty::TApp { ty: base, args } if !args.is_empty() => {
                    let base_name = base.get_constr_name_unsafe();
                    let ident = Uident::new(&base_name);
                    if self.enum_base.contains_key(&ident) || self.struct_base.contains_key(&ident)
                    {
                        let new_u = self.ensure_instance(&base_name, args);
                        Ty::TCon {
                            name: new_u.0.clone(),
                        }
                    } else {
                        Ty::TApp {
                            ty: Box::new(self.collapse_type_apps(base)),
                            args: args.iter().map(|t| self.collapse_type_apps(t)).collect(),
                        }
                    }
                }
                Ty::TApp { ty: base, args } => Ty::TApp {
                    ty: Box::new(self.collapse_type_apps(base)),
                    args: args.iter().map(|t| self.collapse_type_apps(t)).collect(),
                },
                Ty::TTuple { typs } => Ty::TTuple {
                    typs: typs.iter().map(|t| self.collapse_type_apps(t)).collect(),
                },
                Ty::TFunc { params, ret_ty } => Ty::TFunc {
                    params: params.iter().map(|t| self.collapse_type_apps(t)).collect(),
                    ret_ty: Box::new(self.collapse_type_apps(ret_ty)),
                },
                Ty::TCon { name } => Ty::TCon { name: name.clone() },
                _ => ty.clone(),
            }
        }
    }

    fn rewrite_expr_types<'a>(e: core::Expr, m: &mut TypeMono<'a>) -> core::Expr {
        match e {
            core::Expr::EVar { name, ty } => core::Expr::EVar {
                name,
                ty: m.collapse_type_apps(&ty),
            },
            core::Expr::EUnit { ty } => core::Expr::EUnit {
                ty: m.collapse_type_apps(&ty),
            },
            core::Expr::EBool { value, ty } => core::Expr::EBool {
                value,
                ty: m.collapse_type_apps(&ty),
            },
            core::Expr::EInt { value, ty } => core::Expr::EInt {
                value,
                ty: m.collapse_type_apps(&ty),
            },
            core::Expr::EString { value, ty } => core::Expr::EString {
                value,
                ty: m.collapse_type_apps(&ty),
            },
            core::Expr::EConstr {
                constructor,
                args,
                ty,
            } => {
                let new_ty = m.collapse_type_apps(&ty);
                let new_constructor = update_constructor_type(&constructor, &new_ty);
                core::Expr::EConstr {
                    constructor: new_constructor,
                    args: args.into_iter().map(|a| rewrite_expr_types(a, m)).collect(),
                    ty: new_ty,
                }
            }
            core::Expr::ETuple { items, ty } => core::Expr::ETuple {
                items: items
                    .into_iter()
                    .map(|a| rewrite_expr_types(a, m))
                    .collect(),
                ty: m.collapse_type_apps(&ty),
            },
            core::Expr::EArray { items, ty } => core::Expr::EArray {
                items: items
                    .into_iter()
                    .map(|a| rewrite_expr_types(a, m))
                    .collect(),
                ty: m.collapse_type_apps(&ty),
            },
            core::Expr::EClosure { .. } => {
                panic!("lambda lift should have removed closures before monomorphization");
            }
            core::Expr::ELet {
                name,
                value,
                body,
                ty,
            } => core::Expr::ELet {
                name,
                value: Box::new(rewrite_expr_types(*value, m)),
                body: Box::new(rewrite_expr_types(*body, m)),
                ty: m.collapse_type_apps(&ty),
            },
            core::Expr::EMatch {
                expr,
                arms,
                default,
                ty,
            } => core::Expr::EMatch {
                expr: Box::new(rewrite_expr_types(*expr, m)),
                arms: arms
                    .into_iter()
                    .map(|arm| core::Arm {
                        lhs: rewrite_expr_types(arm.lhs, m),
                        body: rewrite_expr_types(arm.body, m),
                    })
                    .collect(),
                default: default.map(|d| Box::new(rewrite_expr_types(*d, m))),
                ty: m.collapse_type_apps(&ty),
            },
            core::Expr::EIf {
                cond,
                then_branch,
                else_branch,
                ty,
            } => core::Expr::EIf {
                cond: Box::new(rewrite_expr_types(*cond, m)),
                then_branch: Box::new(rewrite_expr_types(*then_branch, m)),
                else_branch: Box::new(rewrite_expr_types(*else_branch, m)),
                ty: m.collapse_type_apps(&ty),
            },
            core::Expr::EConstrGet {
                expr,
                constructor,
                field_index,
                ty,
            } => {
                let new_expr = rewrite_expr_types(*expr, m);
                let scrut_ty = new_expr.get_ty();
                let new_constructor = update_constructor_type(&constructor, &scrut_ty);
                core::Expr::EConstrGet {
                    expr: Box::new(new_expr),
                    constructor: new_constructor,
                    field_index,
                    ty: m.collapse_type_apps(&ty),
                }
            }
            core::Expr::ECall { func, args, ty } => core::Expr::ECall {
                func,
                args: args.into_iter().map(|a| rewrite_expr_types(a, m)).collect(),
                ty: m.collapse_type_apps(&ty),
            },
            core::Expr::EProj { tuple, index, ty } => core::Expr::EProj {
                tuple: Box::new(rewrite_expr_types(*tuple, m)),
                index,
                ty: m.collapse_type_apps(&ty),
            },
        }
    }

    // Rewrite function signatures and bodies
    let mut m = TypeMono::new(env);
    let mut new_fns = Vec::new();
    for f in ctx.out.into_iter() {
        let params = f
            .params
            .into_iter()
            .map(|(n, t)| (n, m.collapse_type_apps(&t)))
            .collect();
        let ret_ty = m.collapse_type_apps(&f.ret_ty);
        let body = rewrite_expr_types(f.body, &mut m);
        new_fns.push(core::Fn {
            name: f.name,
            params,
            ret_ty,
            body,
        });
    }

    // Drop all generic enum defs to avoid Go backend panics
    m.env.enums.retain(|_n, def| def.generics.is_empty());
    m.env.structs.retain(|_n, def| def.generics.is_empty());

    let result = core::File { toplevels: new_fns };
    m.env.record_tuple_types_from_core(&result);
    result
}
