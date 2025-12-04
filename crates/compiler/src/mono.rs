use crate::common::{self, Constructor, Prim};
use crate::core::{self, Ty};
use crate::env::{EnumDef, GlobalTypeEnv, StructDef};
use crate::mangle::encode_ty;
use crate::tast;
use ast::ast::Ident;
use indexmap::{IndexMap, IndexSet};
use std::collections::VecDeque;

#[derive(Debug, Clone)]
pub struct MonoFile {
    pub toplevels: Vec<MonoFn>,
}

#[derive(Debug, Clone)]
pub struct MonoFn {
    pub name: String,
    pub params: Vec<(String, Ty)>,
    pub ret_ty: Ty,
    pub body: MonoExpr,
}

#[derive(Debug, Clone)]
pub enum MonoExpr {
    EVar {
        name: String,
        ty: Ty,
    },
    EPrim {
        value: Prim,
        ty: Ty,
    },
    EConstr {
        constructor: Constructor,
        args: Vec<MonoExpr>,
        ty: Ty,
    },
    ETuple {
        items: Vec<MonoExpr>,
        ty: Ty,
    },
    EArray {
        items: Vec<MonoExpr>,
        ty: Ty,
    },
    ELet {
        name: String,
        value: Box<MonoExpr>,
        body: Box<MonoExpr>,
        ty: Ty,
    },
    EMatch {
        expr: Box<MonoExpr>,
        arms: Vec<MonoArm>,
        default: Option<Box<MonoExpr>>,
        ty: Ty,
    },
    EIf {
        cond: Box<MonoExpr>,
        then_branch: Box<MonoExpr>,
        else_branch: Box<MonoExpr>,
        ty: Ty,
    },
    EWhile {
        cond: Box<MonoExpr>,
        body: Box<MonoExpr>,
        ty: Ty,
    },
    EConstrGet {
        expr: Box<MonoExpr>,
        constructor: Constructor,
        field_index: usize,
        ty: Ty,
    },
    ECall {
        func: Box<MonoExpr>,
        args: Vec<MonoExpr>,
        ty: Ty,
    },
    EClosure {
        params: Vec<tast::ClosureParam>,
        body: Box<MonoExpr>,
        ty: Ty,
    },
    EProj {
        tuple: Box<MonoExpr>,
        index: usize,
        ty: Ty,
    },
}

impl MonoExpr {
    pub fn get_ty(&self) -> Ty {
        match self {
            MonoExpr::EVar { ty, .. } => ty.clone(),
            MonoExpr::EPrim { ty, .. } => ty.clone(),
            MonoExpr::EConstr { ty, .. } => ty.clone(),
            MonoExpr::ETuple { ty, .. } => ty.clone(),
            MonoExpr::EArray { ty, .. } => ty.clone(),
            MonoExpr::EClosure { ty, .. } => ty.clone(),
            MonoExpr::ELet { ty, .. } => ty.clone(),
            MonoExpr::EMatch { ty, .. } => ty.clone(),
            MonoExpr::EIf { ty, .. } => ty.clone(),
            MonoExpr::EWhile { ty, .. } => ty.clone(),
            MonoExpr::EConstrGet { ty, .. } => ty.clone(),
            MonoExpr::ECall { ty, .. } => ty.clone(),
            MonoExpr::EProj { ty, .. } => ty.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MonoArm {
    pub lhs: MonoExpr,
    pub body: MonoExpr,
}

#[derive(Debug, Clone)]
pub struct GlobalMonoEnv {
    pub genv: GlobalTypeEnv,
    pub extra_enums: IndexMap<Ident, EnumDef>,
    pub extra_structs: IndexMap<Ident, StructDef>,
}

impl GlobalMonoEnv {
    pub fn from_genv(genv: GlobalTypeEnv) -> Self {
        Self {
            genv,
            extra_enums: IndexMap::new(),
            extra_structs: IndexMap::new(),
        }
    }

    pub fn enums(&self) -> impl Iterator<Item = (&Ident, &EnumDef)> {
        self.genv
            .enums()
            .iter()
            .chain(self.extra_enums.iter())
            .filter(|(_, def)| def.generics.is_empty())
    }

    pub fn get_enum(&self, name: &Ident) -> Option<&EnumDef> {
        self.extra_enums
            .get(name)
            .or_else(|| self.genv.enums().get(name))
            .filter(|def| def.generics.is_empty())
    }

    pub fn enums_cloned(&self) -> IndexMap<Ident, EnumDef> {
        let mut result = self.genv.enums().clone();
        result.extend(self.extra_enums.clone());
        result
    }

    pub fn struct_def_mut(&mut self, name: &Ident) -> Option<&mut StructDef> {
        if self.extra_structs.contains_key(name) {
            self.extra_structs.get_mut(name)
        } else {
            self.genv.struct_def_mut(name)
        }
    }

    pub fn insert_struct(&mut self, def: StructDef) {
        self.extra_structs.insert(def.name.clone(), def);
    }

    pub fn structs(&self) -> impl Iterator<Item = (&Ident, &StructDef)> {
        self.genv
            .structs()
            .iter()
            .chain(self.extra_structs.iter())
            .filter(|(_, def)| def.generics.is_empty())
    }

    pub fn structs_cloned(&self) -> IndexMap<Ident, StructDef> {
        let mut result: IndexMap<Ident, StructDef> = self
            .genv
            .structs()
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        result.extend(self.extra_structs.clone());
        result
    }

    pub fn insert_enum(&mut self, def: EnumDef) {
        self.extra_enums.insert(def.name.clone(), def);
    }

    pub fn retain_enums<F>(&mut self, mut f: F)
    where
        F: FnMut(&Ident, &mut EnumDef) -> bool,
    {
        self.extra_enums.retain(&mut f);
    }

    pub fn retain_structs<F>(&mut self, mut f: F)
    where
        F: FnMut(&Ident, &mut StructDef) -> bool,
    {
        self.extra_structs.retain(&mut f);
    }

    pub fn get_struct(&self, name: &Ident) -> Option<&StructDef> {
        self.extra_structs
            .get(name)
            .or_else(|| self.genv.structs().get(name))
    }
}

// Helpers
fn has_tparam(ty: &Ty) -> bool {
    match ty {
        Ty::TParam { .. } => true,
        Ty::TVar(..) => false,
        Ty::TUnit
        | Ty::TBool
        | Ty::TInt8
        | Ty::TInt16
        | Ty::TInt32
        | Ty::TInt64
        | Ty::TUint8
        | Ty::TUint16
        | Ty::TUint32
        | Ty::TUint64
        | Ty::TFloat32
        | Ty::TFloat64
        | Ty::TString => false,
        Ty::TTuple { typs } => typs.iter().any(has_tparam),
        Ty::TEnum { .. } | Ty::TStruct { .. } => false,
        Ty::TApp { ty, args } => has_tparam(ty.as_ref()) || args.iter().any(has_tparam),
        Ty::TArray { elem, .. } => has_tparam(elem),
        Ty::TRef { elem } => has_tparam(elem),
        Ty::TFunc { params, ret_ty } => params.iter().any(has_tparam) || has_tparam(ret_ty),
    }
}

fn update_constructor_type(constructor: &Constructor, new_ty: &Ty) -> Constructor {
    match (constructor, new_ty) {
        (Constructor::Enum(enum_constructor), Ty::TEnum { name }) => {
            let ident = Ident::new(name);
            Constructor::Enum(common::EnumConstructor {
                type_name: ident,
                variant: enum_constructor.variant.clone(),
                index: enum_constructor.index,
            })
        }
        (Constructor::Enum(enum_constructor), Ty::TApp { ty, .. }) => {
            let base = ty.get_constr_name_unsafe();
            Constructor::Enum(common::EnumConstructor {
                type_name: Ident::new(&base),
                variant: enum_constructor.variant.clone(),
                index: enum_constructor.index,
            })
        }
        (Constructor::Struct(_), Ty::TStruct { name }) => {
            let ident = Ident::new(name);
            Constructor::Struct(common::StructConstructor { type_name: ident })
        }
        (Constructor::Struct(_), Ty::TApp { ty, .. }) => {
            let base = ty.get_constr_name_unsafe();
            Constructor::Struct(common::StructConstructor {
                type_name: Ident::new(&base),
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
        Ty::TUnit
        | Ty::TBool
        | Ty::TInt8
        | Ty::TInt16
        | Ty::TInt32
        | Ty::TInt64
        | Ty::TUint8
        | Ty::TUint16
        | Ty::TUint32
        | Ty::TUint64
        | Ty::TFloat32
        | Ty::TFloat64
        | Ty::TString => ty.clone(),
        Ty::TTuple { typs } => Ty::TTuple {
            typs: typs.iter().map(|t| subst_ty(t, s)).collect(),
        },
        Ty::TEnum { name } => Ty::TEnum { name: name.clone() },
        Ty::TStruct { name } => Ty::TStruct { name: name.clone() },
        Ty::TApp { ty, args } => Ty::TApp {
            ty: Box::new(subst_ty(ty, s)),
            args: args.iter().map(|t| subst_ty(t, s)).collect(),
        },
        Ty::TArray { len, elem } => Ty::TArray {
            len: *len,
            elem: Box::new(subst_ty(elem, s)),
        },
        Ty::TRef { elem } => Ty::TRef {
            elem: Box::new(subst_ty(elem, s)),
        },
        Ty::TFunc { params, ret_ty } => Ty::TFunc {
            params: params.iter().map(|t| subst_ty(t, s)).collect(),
            ret_ty: Box::new(subst_ty(ret_ty, s)),
        },
    }
}

fn subst_closure_param(param: &tast::ClosureParam, s: &Subst) -> tast::ClosureParam {
    tast::ClosureParam {
        name: param.name.clone(),
        ty: subst_ty(&param.ty, s),
        astptr: param.astptr,
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
        | (Ty::TInt8, Ty::TInt8)
        | (Ty::TInt16, Ty::TInt16)
        | (Ty::TInt32, Ty::TInt32)
        | (Ty::TInt64, Ty::TInt64)
        | (Ty::TUint8, Ty::TUint8)
        | (Ty::TUint16, Ty::TUint16)
        | (Ty::TUint32, Ty::TUint32)
        | (Ty::TUint64, Ty::TUint64)
        | (Ty::TFloat32, Ty::TFloat32)
        | (Ty::TFloat64, Ty::TFloat64)
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
        (Ty::TEnum { name: ln }, Ty::TEnum { name: rn })
        | (Ty::TStruct { name: ln }, Ty::TStruct { name: rn }) => {
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
        (Ty::TRef { elem: le }, Ty::TRef { elem: re }) => unify(le, re, subst),
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
    out: Vec<MonoFn>,
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
fn mono_expr(ctx: &mut Ctx, e: &core::Expr, s: &Subst) -> MonoExpr {
    match e.clone() {
        core::Expr::EVar { name, ty } => MonoExpr::EVar {
            name,
            ty: subst_ty(&ty, s),
        },
        core::Expr::EPrim { value, ty } => {
            let ty = subst_ty(&ty, s);
            MonoExpr::EPrim {
                value: value.coerce(&ty),
                ty,
            }
        }
        core::Expr::EConstr {
            constructor,
            args,
            ty,
        } => {
            let new_ty = subst_ty(&ty, s);
            let new_constructor = update_constructor_type(&constructor, &new_ty);
            MonoExpr::EConstr {
                constructor: new_constructor,
                args: args.iter().map(|a| mono_expr(ctx, a, s)).collect(),
                ty: new_ty,
            }
        }
        core::Expr::ETuple { items, ty } => MonoExpr::ETuple {
            items: items.iter().map(|a| mono_expr(ctx, a, s)).collect(),
            ty: subst_ty(&ty, s),
        },
        core::Expr::EArray { items, ty } => MonoExpr::EArray {
            items: items.iter().map(|a| mono_expr(ctx, a, s)).collect(),
            ty: subst_ty(&ty, s),
        },
        core::Expr::EClosure { params, body, ty } => {
            let new_params: Vec<tast::ClosureParam> =
                params.iter().map(|p| subst_closure_param(p, s)).collect();
            let new_body = mono_expr(ctx, &body, s);
            let new_ty = subst_ty(&ty, s);
            MonoExpr::EClosure {
                params: new_params,
                body: Box::new(new_body),
                ty: new_ty,
            }
        }
        core::Expr::ELet {
            name,
            value,
            body,
            ty,
        } => MonoExpr::ELet {
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
        } => MonoExpr::EMatch {
            expr: Box::new(mono_expr(ctx, &expr, s)),
            arms: arms
                .iter()
                .map(|arm| MonoArm {
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
        } => MonoExpr::EIf {
            cond: Box::new(mono_expr(ctx, &cond, s)),
            then_branch: Box::new(mono_expr(ctx, &then_branch, s)),
            else_branch: Box::new(mono_expr(ctx, &else_branch, s)),
            ty: subst_ty(&ty, s),
        },
        core::Expr::EWhile { cond, body, ty } => MonoExpr::EWhile {
            cond: Box::new(mono_expr(ctx, &cond, s)),
            body: Box::new(mono_expr(ctx, &body, s)),
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
            MonoExpr::EConstrGet {
                expr: Box::new(new_expr),
                constructor: new_constructor,
                field_index,
                ty: subst_ty(&ty, s),
            }
        }
        core::Expr::ECall { func, args, ty } => {
            let new_func = mono_expr(ctx, &func, s);
            let new_args: Vec<MonoExpr> = args.iter().map(|a| mono_expr(ctx, a, s)).collect();
            let new_ty = subst_ty(&ty, s);

            let MonoExpr::EVar {
                name: func_name, ..
            } = &new_func
            else {
                return MonoExpr::ECall {
                    func: Box::new(new_func),
                    args: new_args,
                    ty: new_ty,
                };
            };

            // If function is not in current file (runtime/built-in), leave as is
            let Some(callee) = ctx.orig_fns.get(func_name) else {
                return MonoExpr::ECall {
                    func: Box::new(new_func),
                    args: new_args,
                    ty: new_ty,
                };
            };

            if !fn_is_generic(callee) {
                return MonoExpr::ECall {
                    func: Box::new(new_func),
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
                    panic!(
                        "monomorphization unification failed for {}: {}",
                        func_name, e
                    );
                }
            }
            if let Err(e) = unify(&callee.ret_ty, &new_ty, &mut call_subst) {
                panic!(
                    "monomorphization return type unification failed for {}: {}",
                    func_name, e
                );
            }

            // Ensure the substitution yields concrete types (no TParam in bindings)
            if call_subst.values().any(has_tparam) {
                // If we cannot determine concrete types here, keep the call as-is.
                // This should not happen for reachable instances from monomorphic roots.
                return MonoExpr::ECall {
                    func: Box::new(new_func),
                    args: new_args,
                    ty: new_ty,
                };
            }

            let spec = ctx.ensure_instance(func_name, call_subst);
            MonoExpr::ECall {
                func: Box::new(MonoExpr::EVar {
                    name: spec,
                    ty: new_func.get_ty(),
                }),
                args: new_args,
                ty: new_ty,
            }
        }
        core::Expr::EProj { tuple, index, ty } => MonoExpr::EProj {
            tuple: Box::new(mono_expr(ctx, &tuple, s)),
            index,
            ty: subst_ty(&ty, s),
        },
    }
}

// Phase 2: monomorphize enum type applications in types and update env
struct TypeMono<'a> {
    monoenv: &'a mut GlobalMonoEnv,
    // map generic (name, args) to new concrete Ident
    map: IndexMap<(String, Vec<Ty>), Ident>,
    // snapshot of original generic enum defs
    enum_base: IndexMap<Ident, EnumDef>,
    struct_base: IndexMap<Ident, StructDef>,
}

impl<'a> TypeMono<'a> {
    fn new(monoenv: &'a mut GlobalMonoEnv) -> Self {
        let enum_base = monoenv.enums_cloned();
        let struct_base = monoenv.structs_cloned();
        Self {
            monoenv,
            map: IndexMap::new(),
            enum_base,
            struct_base,
        }
    }

    fn ensure_instance(&mut self, name: &str, args: &[Ty]) -> Ident {
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
        let new_name = Ident::new(&format!("{}{}", name, suffix));
        self.map.insert(key.clone(), new_name.clone());

        let ident = Ident::new(name);

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
            let mut new_variants: Vec<(Ident, Vec<Ty>)> = Vec::new();
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
            self.monoenv.insert_enum(new_def);
        } else if let Some(generic_def) = self.struct_base.get(&ident).cloned() {
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
            self.monoenv.insert_struct(new_def);
        } else {
            // Unknown type constructor; just return synthesized name without registering a def
        }
        new_name
    }

    fn collapse_type_apps(&mut self, ty: &Ty) -> Ty {
        match ty {
            Ty::TApp { ty: base, args } if !args.is_empty() => {
                let base_name = base.get_constr_name_unsafe();
                let ident = Ident::new(&base_name);
                if self.enum_base.contains_key(&ident) {
                    let new_u = self.ensure_instance(&base_name, args);
                    Ty::TEnum {
                        name: new_u.0.clone(),
                    }
                } else if self.struct_base.contains_key(&ident) {
                    let new_u = self.ensure_instance(&base_name, args);
                    Ty::TStruct {
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
            Ty::TEnum { name } => Ty::TEnum { name: name.clone() },
            Ty::TStruct { name } => Ty::TStruct { name: name.clone() },
            Ty::TArray { len, elem } => Ty::TArray {
                len: *len,
                elem: Box::new(self.collapse_type_apps(elem)),
            },
            Ty::TRef { elem } => Ty::TRef {
                elem: Box::new(self.collapse_type_apps(elem)),
            },
            _ => ty.clone(),
        }
    }
}

fn rewrite_closure_param_types(
    param: &tast::ClosureParam,
    m: &mut TypeMono<'_>,
) -> tast::ClosureParam {
    tast::ClosureParam {
        name: param.name.clone(),
        ty: m.collapse_type_apps(&param.ty),
        astptr: param.astptr,
    }
}

fn rewrite_expr_types(e: MonoExpr, m: &mut TypeMono<'_>) -> MonoExpr {
    match e {
        MonoExpr::EVar { name, ty } => MonoExpr::EVar {
            name,
            ty: m.collapse_type_apps(&ty),
        },
        MonoExpr::EPrim { value, ty } => {
            let ty = m.collapse_type_apps(&ty);
            MonoExpr::EPrim {
                value: value.coerce(&ty),
                ty,
            }
        }
        MonoExpr::EConstr {
            constructor,
            args,
            ty,
        } => {
            let new_ty = m.collapse_type_apps(&ty);
            let new_constructor = update_constructor_type(&constructor, &new_ty);
            MonoExpr::EConstr {
                constructor: new_constructor,
                args: args.into_iter().map(|a| rewrite_expr_types(a, m)).collect(),
                ty: new_ty,
            }
        }
        MonoExpr::ETuple { items, ty } => MonoExpr::ETuple {
            items: items
                .into_iter()
                .map(|a| rewrite_expr_types(a, m))
                .collect(),
            ty: m.collapse_type_apps(&ty),
        },
        MonoExpr::EArray { items, ty } => MonoExpr::EArray {
            items: items
                .into_iter()
                .map(|a| rewrite_expr_types(a, m))
                .collect(),
            ty: m.collapse_type_apps(&ty),
        },
        MonoExpr::EClosure { params, body, ty } => {
            let new_params: Vec<tast::ClosureParam> = params
                .iter()
                .map(|p| rewrite_closure_param_types(p, m))
                .collect();
            let new_body = rewrite_expr_types(*body, m);
            let new_ty = m.collapse_type_apps(&ty);
            MonoExpr::EClosure {
                params: new_params,
                body: Box::new(new_body),
                ty: new_ty,
            }
        }
        MonoExpr::ELet {
            name,
            value,
            body,
            ty,
        } => MonoExpr::ELet {
            name,
            value: Box::new(rewrite_expr_types(*value, m)),
            body: Box::new(rewrite_expr_types(*body, m)),
            ty: m.collapse_type_apps(&ty),
        },
        MonoExpr::EMatch {
            expr,
            arms,
            default,
            ty,
        } => MonoExpr::EMatch {
            expr: Box::new(rewrite_expr_types(*expr, m)),
            arms: arms
                .into_iter()
                .map(|arm| MonoArm {
                    lhs: rewrite_expr_types(arm.lhs, m),
                    body: rewrite_expr_types(arm.body, m),
                })
                .collect(),
            default: default.map(|d| Box::new(rewrite_expr_types(*d, m))),
            ty: m.collapse_type_apps(&ty),
        },
        MonoExpr::EIf {
            cond,
            then_branch,
            else_branch,
            ty,
        } => MonoExpr::EIf {
            cond: Box::new(rewrite_expr_types(*cond, m)),
            then_branch: Box::new(rewrite_expr_types(*then_branch, m)),
            else_branch: Box::new(rewrite_expr_types(*else_branch, m)),
            ty: m.collapse_type_apps(&ty),
        },
        MonoExpr::EWhile { cond, body, ty } => MonoExpr::EWhile {
            cond: Box::new(rewrite_expr_types(*cond, m)),
            body: Box::new(rewrite_expr_types(*body, m)),
            ty: m.collapse_type_apps(&ty),
        },
        MonoExpr::EConstrGet {
            expr,
            constructor,
            field_index,
            ty,
        } => {
            let new_expr = rewrite_expr_types(*expr, m);
            let scrut_ty = new_expr.get_ty();
            let new_constructor = update_constructor_type(&constructor, &scrut_ty);
            MonoExpr::EConstrGet {
                expr: Box::new(new_expr),
                constructor: new_constructor,
                field_index,
                ty: m.collapse_type_apps(&ty),
            }
        }
        MonoExpr::ECall { func, args, ty } => MonoExpr::ECall {
            func: Box::new(rewrite_expr_types(*func, m)),
            args: args.into_iter().map(|a| rewrite_expr_types(a, m)).collect(),
            ty: m.collapse_type_apps(&ty),
        },
        MonoExpr::EProj { tuple, index, ty } => MonoExpr::EProj {
            tuple: Box::new(rewrite_expr_types(*tuple, m)),
            index,
            ty: m.collapse_type_apps(&ty),
        },
    }
}

// Monomorphize Core IR by specializing generic functions per concrete call site.
// Produces a file containing only monomorphic functions reachable from monomorphic roots.
pub fn mono(genv: GlobalTypeEnv, file: core::File) -> (MonoFile, GlobalMonoEnv) {
    let mut monoenv = GlobalMonoEnv::from_genv(genv);
    // Build original function map
    let mut orig_fns: IndexMap<String, core::Fn> = IndexMap::new();
    for f in file.toplevels.into_iter() {
        orig_fns.insert(f.name.clone(), f);
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

        ctx.out.push(MonoFn {
            name: spec_name,
            params: new_params,
            ret_ty: new_ret,
            body: new_body,
        });
    }

    // Rewrite function signatures and bodies
    let mut m = TypeMono::new(&mut monoenv);
    let mut new_fns = Vec::new();
    for f in ctx.out.into_iter() {
        let params = f
            .params
            .into_iter()
            .map(|(n, t)| (n, m.collapse_type_apps(&t)))
            .collect();
        let ret_ty = m.collapse_type_apps(&f.ret_ty);
        let body = rewrite_expr_types(f.body, &mut m);
        new_fns.push(MonoFn {
            name: f.name,
            params,
            ret_ty,
            body,
        });
    }

    // Drop all generic enum defs to avoid Go backend panics
    m.monoenv.retain_enums(|_n, def| def.generics.is_empty());
    m.monoenv.retain_structs(|_n, def| def.generics.is_empty());

    let result = MonoFile { toplevels: new_fns };
    (result, monoenv)
}
