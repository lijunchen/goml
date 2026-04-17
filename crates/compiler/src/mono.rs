use crate::common::{self, Constructor, Prim};
use crate::core::{self, Ty};
use crate::env::{EnumDef, GlobalTypeEnv, StructDef};
use crate::names::{
    parse_inherent_method_fn_name, parse_trait_impl_fn_name, trait_impl_fn_name, ty_compact,
};
use crate::tast::{self, TastIdent};
use indexmap::{IndexMap, IndexSet};
use std::collections::VecDeque;

const MONO_INSTANCE_LIMIT: usize = 4096;

#[derive(Debug, Clone)]
pub struct MonoFile {
    pub toplevels: Vec<MonoFn>,
}

#[derive(Debug, Clone)]
pub struct MonoFn {
    pub name: String,
    pub params: Vec<(String, Ty)>,
    pub ret_ty: Ty,
    pub body: MonoBlock,
}

#[derive(Debug, Clone)]
pub struct MonoLetStmt {
    pub name: String,
    pub value: MonoExpr,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct MonoBlock {
    pub stmts: Vec<MonoLetStmt>,
    pub tail: Option<Box<MonoExpr>>,
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
    EBlock {
        block: Box<MonoBlock>,
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
    EBreak {
        ty: Ty,
    },
    EContinue {
        ty: Ty,
    },
    EReturn {
        expr: Option<Box<MonoExpr>>,
        ty: Ty,
    },
    EGo {
        expr: Box<MonoExpr>,
        ty: Ty,
    },
    EConstrGet {
        expr: Box<MonoExpr>,
        constructor: Constructor,
        field_index: usize,
        ty: Ty,
    },
    EUnary {
        op: common_defs::UnaryOp,
        expr: Box<MonoExpr>,
        ty: Ty,
    },
    EBinary {
        op: common_defs::BinaryOp,
        lhs: Box<MonoExpr>,
        rhs: Box<MonoExpr>,
        ty: Ty,
    },
    EAssign {
        name: String,
        value: Box<MonoExpr>,
        target_ty: Ty,
        ty: Ty,
    },
    ECall {
        func: Box<MonoExpr>,
        args: Vec<MonoExpr>,
        ty: Ty,
    },
    EToDyn {
        trait_name: TastIdent,
        for_ty: Ty,
        expr: Box<MonoExpr>,
        ty: Ty,
    },
    EDynCall {
        trait_name: TastIdent,
        method_name: TastIdent,
        receiver: Box<MonoExpr>,
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
            MonoExpr::EBlock { ty, .. } => ty.clone(),
            MonoExpr::EMatch { ty, .. } => ty.clone(),
            MonoExpr::EIf { ty, .. } => ty.clone(),
            MonoExpr::EWhile { ty, .. } => ty.clone(),
            MonoExpr::EBreak { ty, .. } => ty.clone(),
            MonoExpr::EContinue { ty, .. } => ty.clone(),
            MonoExpr::EReturn { ty, .. } => ty.clone(),
            MonoExpr::EGo { ty, .. } => ty.clone(),
            MonoExpr::EConstrGet { ty, .. } => ty.clone(),
            MonoExpr::EUnary { ty, .. } => ty.clone(),
            MonoExpr::EBinary { ty, .. } => ty.clone(),
            MonoExpr::EAssign { ty, .. } => ty.clone(),
            MonoExpr::ECall { ty, .. } => ty.clone(),
            MonoExpr::EToDyn { ty, .. } => ty.clone(),
            MonoExpr::EDynCall { ty, .. } => ty.clone(),
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
    pub mono_enums: IndexMap<TastIdent, EnumDef>,
    pub mono_structs: IndexMap<TastIdent, StructDef>,
    pub mono_funcs: IndexMap<String, Ty>,
}

impl GlobalMonoEnv {
    pub fn from_genv(genv: GlobalTypeEnv) -> Self {
        Self {
            genv,
            mono_enums: IndexMap::new(),
            mono_structs: IndexMap::new(),
            mono_funcs: IndexMap::new(),
        }
    }

    pub fn enums(&self) -> impl Iterator<Item = (&TastIdent, &EnumDef)> {
        self.genv
            .enums()
            .iter()
            .chain(self.mono_enums.iter())
            .filter(|(_, def)| def.generics.is_empty())
    }

    pub fn get_enum(&self, name: &TastIdent) -> Option<&EnumDef> {
        self.mono_enums
            .get(name)
            .or_else(|| self.genv.enums().get(name))
            .filter(|def| def.generics.is_empty())
    }

    pub fn get_enum_mut(&mut self, name: &TastIdent) -> Option<&mut EnumDef> {
        if self.mono_enums.contains_key(name) {
            self.mono_enums.get_mut(name)
        } else {
            self.genv.enum_def_mut(name)
        }
    }

    pub fn enums_cloned(&self) -> IndexMap<TastIdent, EnumDef> {
        let mut result = self.genv.enums().clone();
        result.extend(self.mono_enums.clone());
        result
    }

    pub fn struct_def_mut(&mut self, name: &TastIdent) -> Option<&mut StructDef> {
        if self.mono_structs.contains_key(name) {
            self.mono_structs.get_mut(name)
        } else {
            self.genv.struct_def_mut(name)
        }
    }

    pub fn insert_struct(&mut self, def: StructDef) {
        self.mono_structs.insert(def.name.clone(), def);
    }

    pub fn structs(&self) -> impl Iterator<Item = (&TastIdent, &StructDef)> {
        self.genv
            .structs()
            .iter()
            .chain(self.mono_structs.iter())
            .filter(|(_, def)| def.generics.is_empty())
    }

    pub fn structs_cloned(&self) -> IndexMap<TastIdent, StructDef> {
        let mut result: IndexMap<TastIdent, StructDef> = self
            .genv
            .structs()
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        result.extend(self.mono_structs.clone());
        result
    }

    pub fn insert_enum(&mut self, def: EnumDef) {
        self.mono_enums.insert(def.name.clone(), def);
    }

    pub fn retain_enums<F>(&mut self, mut f: F)
    where
        F: FnMut(&TastIdent, &mut EnumDef) -> bool,
    {
        self.mono_enums.retain(&mut f);
    }

    pub fn retain_structs<F>(&mut self, mut f: F)
    where
        F: FnMut(&TastIdent, &mut StructDef) -> bool,
    {
        self.mono_structs.retain(&mut f);
    }

    pub fn get_struct(&self, name: &TastIdent) -> Option<&StructDef> {
        self.mono_structs
            .get(name)
            .or_else(|| self.genv.structs().get(name))
    }

    pub fn get_func(&self, name: &str) -> Option<&Ty> {
        self.mono_funcs.get(name)
    }

    pub fn insert_func(&mut self, name: String, ty: Ty) {
        self.mono_funcs.insert(name, ty);
    }

    pub fn rename_func(&mut self, old_name: &str, new_name: &str) {
        if let Some(ty) = self.mono_funcs.swap_remove(old_name) {
            self.mono_funcs.insert(new_name.to_string(), ty);
        }
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
        | Ty::TString
        | Ty::TChar => false,
        Ty::TTuple { typs } => typs.iter().any(has_tparam),
        Ty::TEnum { .. } | Ty::TStruct { .. } | Ty::TDyn { .. } => false,
        Ty::TApp { ty, args } => has_tparam(ty.as_ref()) || args.iter().any(has_tparam),
        Ty::TArray { elem, .. } => has_tparam(elem),
        Ty::TSlice { elem } => has_tparam(elem),
        Ty::TVec { elem } => has_tparam(elem),
        Ty::TRef { elem } => has_tparam(elem),
        Ty::THashMap { key, value } => has_tparam(key) || has_tparam(value),
        Ty::TFunc { params, ret_ty } => params.iter().any(has_tparam) || has_tparam(ret_ty),
    }
}

fn mono_expr_always_exits_control_flow(expr: &MonoExpr) -> bool {
    match expr {
        MonoExpr::EBreak { .. } | MonoExpr::EContinue { .. } | MonoExpr::EReturn { .. } => true,
        MonoExpr::EBlock { block, .. } => block
            .tail
            .as_deref()
            .is_some_and(mono_expr_always_exits_control_flow),
        MonoExpr::EIf {
            then_branch,
            else_branch,
            ..
        } => {
            mono_expr_always_exits_control_flow(then_branch)
                && mono_expr_always_exits_control_flow(else_branch)
        }
        MonoExpr::EMatch { arms, default, .. } => {
            default
                .as_deref()
                .is_some_and(mono_expr_always_exits_control_flow)
                && arms
                    .iter()
                    .all(|arm| mono_expr_always_exits_control_flow(&arm.body))
        }
        _ => false,
    }
}

fn subst_is_fully_concrete(generics: &[String], subst: &Subst) -> bool {
    generics.iter().all(|param| subst.contains_key(param)) && !subst.values().any(has_tparam)
}

fn format_ty_for_mono_diag(ty: &Ty) -> String {
    match ty {
        Ty::TVar(_) => "unknown".to_string(),
        Ty::TUnit => "unit".to_string(),
        Ty::TBool => "bool".to_string(),
        Ty::TInt8 => "int8".to_string(),
        Ty::TInt16 => "int16".to_string(),
        Ty::TInt32 => "int32".to_string(),
        Ty::TInt64 => "int64".to_string(),
        Ty::TUint8 => "uint8".to_string(),
        Ty::TUint16 => "uint16".to_string(),
        Ty::TUint32 => "uint32".to_string(),
        Ty::TUint64 => "uint64".to_string(),
        Ty::TFloat32 => "float32".to_string(),
        Ty::TFloat64 => "float64".to_string(),
        Ty::TString => "string".to_string(),
        Ty::TChar => "char".to_string(),
        Ty::TTuple { typs } => {
            let items = typs
                .iter()
                .map(format_ty_for_mono_diag)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", items)
        }
        Ty::TEnum { name } | Ty::TStruct { name } => name.clone(),
        Ty::TDyn { trait_name } => format!("dyn {}", trait_name),
        Ty::TApp { ty, args } => {
            let base = format_ty_for_mono_diag(ty.as_ref());
            if args.is_empty() {
                base
            } else {
                let args = args
                    .iter()
                    .map(format_ty_for_mono_diag)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}[{}]", base, args)
            }
        }
        Ty::TArray { len, elem } => format!("[{}; {}]", format_ty_for_mono_diag(elem), len),
        Ty::TSlice { elem } => format!("Slice[{}]", format_ty_for_mono_diag(elem)),
        Ty::TVec { elem } => format!("Vec[{}]", format_ty_for_mono_diag(elem)),
        Ty::TRef { elem } => format!("Ref[{}]", format_ty_for_mono_diag(elem)),
        Ty::THashMap { key, value } => format!(
            "HashMap[{}, {}]",
            format_ty_for_mono_diag(key),
            format_ty_for_mono_diag(value)
        ),
        Ty::TParam { name } => name.clone(),
        Ty::TFunc { params, ret_ty } => {
            let params = params
                .iter()
                .map(format_ty_for_mono_diag)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({}) -> {}", params, format_ty_for_mono_diag(ret_ty))
        }
    }
}

fn update_constructor_type(constructor: &Constructor, new_ty: &Ty) -> Constructor {
    match (constructor, new_ty) {
        (Constructor::Enum(enum_constructor), Ty::TEnum { name }) => {
            let ident = TastIdent::new(name);
            Constructor::Enum(common::EnumConstructor {
                type_name: ident,
                variant: enum_constructor.variant.clone(),
                index: enum_constructor.index,
            })
        }
        (Constructor::Enum(enum_constructor), Ty::TApp { ty, .. }) => {
            let base = ty.get_constr_name_unsafe();
            Constructor::Enum(common::EnumConstructor {
                type_name: TastIdent::new(&base),
                variant: enum_constructor.variant.clone(),
                index: enum_constructor.index,
            })
        }
        (Constructor::Struct(_), Ty::TStruct { name }) => {
            let ident = TastIdent::new(name);
            Constructor::Struct(common::StructConstructor { type_name: ident })
        }
        (Constructor::Struct(_), Ty::TApp { ty, .. }) => {
            let base = ty.get_constr_name_unsafe();
            Constructor::Struct(common::StructConstructor {
                type_name: TastIdent::new(&base),
            })
        }
        _ => constructor.clone(),
    }
}

fn fn_is_generic(f: &core::Fn) -> bool {
    !f.generics.is_empty() || f.params.iter().any(|(_, t)| has_tparam(t)) || has_tparam(&f.ret_ty)
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
        | Ty::TString
        | Ty::TChar => ty.clone(),
        Ty::TTuple { typs } => Ty::TTuple {
            typs: typs.iter().map(|t| subst_ty(t, s)).collect(),
        },
        Ty::TEnum { name } => Ty::TEnum { name: name.clone() },
        Ty::TStruct { name } => Ty::TStruct { name: name.clone() },
        Ty::TDyn { trait_name } => Ty::TDyn {
            trait_name: trait_name.clone(),
        },
        Ty::TApp { ty, args } => Ty::TApp {
            ty: Box::new(subst_ty(ty, s)),
            args: args.iter().map(|t| subst_ty(t, s)).collect(),
        },
        Ty::TArray { len, elem } => Ty::TArray {
            len: *len,
            elem: Box::new(subst_ty(elem, s)),
        },
        Ty::TSlice { elem } => Ty::TSlice {
            elem: Box::new(subst_ty(elem, s)),
        },
        Ty::TVec { elem } => Ty::TVec {
            elem: Box::new(subst_ty(elem, s)),
        },
        Ty::TRef { elem } => Ty::TRef {
            elem: Box::new(subst_ty(elem, s)),
        },
        Ty::THashMap { key, value } => Ty::THashMap {
            key: Box::new(subst_ty(key, s)),
            value: Box::new(subst_ty(value, s)),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct SubstKey(Vec<(String, Ty)>);

impl SubstKey {
    fn new(s: &Subst) -> Self {
        let mut entries: Vec<(String, Ty)> =
            s.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
        entries.sort_by(|a, b| a.0.cmp(&b.0));
        Self(entries)
    }
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
        .map(|(k, v)| format!("{}_{}", k, ty_compact(v)))
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
        | (Ty::TString, Ty::TString)
        | (Ty::TChar, Ty::TChar) => Ok(()),
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
        (Ty::TDyn { trait_name: ln }, Ty::TDyn { trait_name: rn }) => {
            if ln != rn {
                return Err("dyn trait mismatch".to_string());
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
        (Ty::TSlice { elem: le }, Ty::TSlice { elem: re }) => unify(le, re, subst),
        (Ty::TVec { elem: le }, Ty::TVec { elem: re }) => unify(le, re, subst),
        (Ty::TRef { elem: le }, Ty::TRef { elem: re }) => unify(le, re, subst),
        (Ty::THashMap { key: lk, value: lv }, Ty::THashMap { key: rk, value: rv }) => {
            unify(lk, rk, subst)?;
            unify(lv, rv, subst)
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
    instances: IndexMap<(String, SubstKey), String>,
    queued: IndexSet<(String, SubstKey)>,
    out: Vec<MonoFn>,
    work: VecDeque<(String, Subst, String)>,
    active_instances: Vec<(String, Subst)>,
    error: Option<String>,
    // Index for inherent methods: (base_type, method_name) -> generic_func_name
    // Example: ("Point", "new") -> "impl_inherent_Point_TParam_U_TParam_V_new"
    inherent_method_index: IndexMap<(String, String), String>,
    // Index for trait impl methods: (trait_name, method_name) -> generic_func_names
    trait_impl_method_index: IndexMap<(String, String), Vec<String>>,
}

impl Ctx {
    fn new(orig_fns: IndexMap<String, core::Fn>) -> Self {
        let mut inherent_method_index = IndexMap::new();
        let mut trait_impl_method_index: IndexMap<(String, String), Vec<String>> = IndexMap::new();

        // Build index for generic inherent methods
        for (fname, f) in orig_fns.iter() {
            if fn_is_generic(f)
                && fname.starts_with("inherent#")
                && let Some((base_type, method_name)) = parse_inherent_method_fn_name(fname)
            {
                inherent_method_index.insert(
                    (base_type.to_string(), method_name.to_string()),
                    fname.clone(),
                );
            }
            if fn_is_generic(f)
                && fname.starts_with("trait_impl#")
                && let Some((trait_name, _, method_name)) = parse_trait_impl_fn_name(fname)
            {
                trait_impl_method_index
                    .entry((trait_name.to_string(), method_name.to_string()))
                    .or_default()
                    .push(fname.clone());
            }
        }

        Self {
            orig_fns,
            instances: IndexMap::new(),
            queued: IndexSet::new(),
            out: Vec::new(),
            work: VecDeque::new(),
            active_instances: Vec::new(),
            error: None,
            inherent_method_index,
            trait_impl_method_index,
        }
    }

    fn find_generic_trait_impl(
        &self,
        trait_name: &str,
        method_name: &str,
        arg_tys: &[Ty],
        ret_ty: &Ty,
    ) -> Option<String> {
        let candidates = self
            .trait_impl_method_index
            .get(&(trait_name.to_string(), method_name.to_string()))?;
        let mut matched: Option<String> = None;
        for candidate_name in candidates {
            let Some(callee) = self.orig_fns.get(candidate_name) else {
                continue;
            };
            if callee.params.len() != arg_tys.len() {
                continue;
            }
            let mut trial_subst = Subst::new();
            let mut ok = true;
            for ((_, param_ty), arg_ty) in callee.params.iter().zip(arg_tys.iter()) {
                if unify(param_ty, arg_ty, &mut trial_subst).is_err() {
                    ok = false;
                    break;
                }
            }
            if !ok {
                continue;
            }
            if unify(&callee.ret_ty, ret_ty, &mut trial_subst).is_err() {
                continue;
            }
            if matched.is_some() {
                return None;
            }
            matched = Some(candidate_name.clone());
        }
        matched
    }

    fn resolve_generic_callee_name(
        &self,
        func_name: &str,
        arg_tys: &[Ty],
        ret_ty: &Ty,
    ) -> Option<String> {
        if self.orig_fns.contains_key(func_name) {
            return Some(func_name.to_string());
        }
        if let Some((base_type, method_name)) = parse_inherent_method_fn_name(func_name)
            && let Some(generic_fname) = self
                .inherent_method_index
                .get(&(base_type.to_string(), method_name.to_string()))
        {
            return Some(generic_fname.clone());
        }
        if let Some((trait_name, _, method_name)) = parse_trait_impl_fn_name(func_name) {
            return self.find_generic_trait_impl(trait_name, method_name, arg_tys, ret_ty);
        }
        None
    }

    fn recursive_specialization_error(&self, name: &str, new_subst: &Subst) -> Option<String> {
        let (_, active_subst) = self
            .active_instances
            .iter()
            .rev()
            .find(|(active_name, _)| active_name == name)?;

        let active_fn = self.orig_fns.get(name)?;
        for param in active_fn.generics.iter() {
            let Some(old_ty) = active_subst.get(param) else {
                continue;
            };
            let Some(new_ty) = new_subst.get(param) else {
                continue;
            };
            if ty_contains_proper_subterm(new_ty, old_ty) {
                return Some(format!(
                    "Infinite monomorphization detected for generic function {}: recursive specialization grows type parameter {} from {} to {}",
                    name,
                    param,
                    format_ty_for_mono_diag(old_ty),
                    format_ty_for_mono_diag(new_ty),
                ));
            }
        }

        for (param, new_ty) in new_subst.iter() {
            for old_ty in active_subst.values() {
                if ty_contains_proper_subterm(new_ty, old_ty) {
                    return Some(format!(
                        "Infinite monomorphization detected for generic function {}: recursive specialization grows type parameter {} from {} to {}",
                        name,
                        param,
                        format_ty_for_mono_diag(old_ty),
                        format_ty_for_mono_diag(new_ty),
                    ));
                }
            }
        }

        None
    }

    // Ensure an instance exists (or is queued) and return its specialized name
    fn ensure_instance(&mut self, name: &str, s: Subst) -> String {
        let key = SubstKey::new(&s);
        let k = (name.to_string(), key.clone());
        if let Some(n) = self.instances.get(&k) {
            return n.clone();
        }

        if self.error.is_none() {
            if let Some(message) = self.recursive_specialization_error(name, &s) {
                self.error = Some(message);
            } else if self.instances.len() >= MONO_INSTANCE_LIMIT {
                self.error = Some(format!(
                    "Monomorphization generated more than {} specialized functions; possible infinite generic specialization involving {}",
                    MONO_INSTANCE_LIMIT, name
                ));
            }
        }

        let spec = spec_name_for(name, &s);
        self.instances
            .insert((name.to_string(), key.clone()), spec.clone());
        if self.error.is_none() && !self.queued.contains(&(name.to_string(), key.clone())) {
            self.queued.insert((name.to_string(), key));
            self.work.push_back((name.to_string(), s, spec.clone()));
        }
        spec
    }
}

fn ty_contains_proper_subterm(ty: &Ty, needle: &Ty) -> bool {
    match ty {
        Ty::TTuple { typs } => typs
            .iter()
            .any(|item| item == needle || ty_contains_proper_subterm(item, needle)),
        Ty::TApp { ty, args } => {
            ty.as_ref() == needle
                || ty_contains_proper_subterm(ty, needle)
                || args
                    .iter()
                    .any(|arg| arg == needle || ty_contains_proper_subterm(arg, needle))
        }
        Ty::TArray { elem, .. } | Ty::TSlice { elem } | Ty::TVec { elem } | Ty::TRef { elem } => {
            elem.as_ref() == needle || ty_contains_proper_subterm(elem, needle)
        }
        Ty::THashMap { key, value } => {
            key.as_ref() == needle
                || ty_contains_proper_subterm(key, needle)
                || value.as_ref() == needle
                || ty_contains_proper_subterm(value, needle)
        }
        Ty::TFunc { params, ret_ty } => {
            params
                .iter()
                .any(|param| param == needle || ty_contains_proper_subterm(param, needle))
                || ret_ty.as_ref() == needle
                || ty_contains_proper_subterm(ret_ty, needle)
        }
        _ => false,
    }
}

fn mono_block(ctx: &mut Ctx, block: &core::Block, s: &Subst) -> MonoBlock {
    MonoBlock {
        stmts: block
            .stmts
            .iter()
            .map(|stmt| MonoLetStmt {
                name: stmt.name.clone(),
                value: mono_expr(ctx, &stmt.value, s),
                ty: subst_ty(&stmt.ty, s),
            })
            .collect(),
        tail: block
            .tail
            .as_ref()
            .map(|tail| Box::new(mono_expr(ctx, tail, s))),
    }
}

fn ensure_trait_impls_for_dyn(ctx: &mut Ctx, trait_name: &str, for_ty: &Ty) {
    let method_keys: Vec<(String, String)> = ctx
        .trait_impl_method_index
        .keys()
        .filter(|(tn, _)| tn == trait_name)
        .cloned()
        .collect();

    for (tn, method_name) in method_keys {
        let impl_fn_name = trait_impl_fn_name(&TastIdent(tn.clone()), for_ty, &method_name);
        if ctx.orig_fns.contains_key(&impl_fn_name) {
            continue;
        }
        let candidates = match ctx.trait_impl_method_index.get(&(tn, method_name)) {
            Some(c) => c.clone(),
            None => continue,
        };
        for candidate_name in candidates {
            let callee = match ctx.orig_fns.get(&candidate_name) {
                Some(c) => c.clone(),
                None => continue,
            };
            if callee.params.is_empty() {
                continue;
            }
            let mut trial_subst = Subst::new();
            if unify(&callee.params[0].1, for_ty, &mut trial_subst).is_ok() {
                ctx.ensure_instance(&candidate_name, trial_subst);
                break;
            }
        }
    }
}

// Transform an expression under a given substitution; queue any needed instances
fn mono_expr(ctx: &mut Ctx, e: &core::Expr, s: &Subst) -> MonoExpr {
    match e.clone() {
        core::Expr::EVar { name, ty } => {
            let concrete_ty = subst_ty(&ty, s);
            if let Some(callee) = ctx.orig_fns.get(&name).cloned()
                && fn_is_generic(&callee)
                && !has_tparam(&concrete_ty)
            {
                let generic_ty = Ty::TFunc {
                    params: callee.params.iter().map(|(_, t)| t.clone()).collect(),
                    ret_ty: Box::new(callee.ret_ty.clone()),
                };
                let mut call_subst = Subst::new();
                if unify(&generic_ty, &concrete_ty, &mut call_subst).is_ok()
                    && !call_subst.values().any(has_tparam)
                {
                    let spec = ctx.ensure_instance(&name, call_subst);
                    return MonoExpr::EVar {
                        name: spec,
                        ty: concrete_ty,
                    };
                }
            }
            MonoExpr::EVar {
                name,
                ty: concrete_ty,
            }
        }
        core::Expr::EPrim { value, ty } => {
            let ty = subst_ty(&ty, s);
            MonoExpr::EPrim { value, ty }
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
        core::Expr::EBlock { block, ty } => MonoExpr::EBlock {
            block: Box::new(mono_block(ctx, &block, s)),
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
        core::Expr::EBreak { ty } => MonoExpr::EBreak {
            ty: subst_ty(&ty, s),
        },
        core::Expr::EContinue { ty } => MonoExpr::EContinue {
            ty: subst_ty(&ty, s),
        },
        core::Expr::EReturn { expr, ty } => MonoExpr::EReturn {
            expr: expr.map(|expr| Box::new(mono_expr(ctx, &expr, s))),
            ty: subst_ty(&ty, s),
        },
        core::Expr::EGo { expr, ty } => MonoExpr::EGo {
            expr: Box::new(mono_expr(ctx, &expr, s)),
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
        core::Expr::EUnary { op, expr, ty } => MonoExpr::EUnary {
            op,
            expr: Box::new(mono_expr(ctx, &expr, s)),
            ty: subst_ty(&ty, s),
        },
        core::Expr::EBinary { op, lhs, rhs, ty } => MonoExpr::EBinary {
            op,
            lhs: Box::new(mono_expr(ctx, &lhs, s)),
            rhs: Box::new(mono_expr(ctx, &rhs, s)),
            ty: subst_ty(&ty, s),
        },
        core::Expr::EAssign {
            name,
            value,
            target_ty,
            ty,
        } => MonoExpr::EAssign {
            name: name.clone(),
            value: Box::new(mono_expr(ctx, &value, s)),
            target_ty: subst_ty(&target_ty, s),
            ty: subst_ty(&ty, s),
        },
        core::Expr::ECall { func, args, ty } => {
            let new_func = mono_expr(ctx, &func, s);
            let new_args: Vec<MonoExpr> = args.iter().map(|a| mono_expr(ctx, a, s)).collect();
            let new_ty = subst_ty(&ty, s);
            let arg_tys = new_args.iter().map(|a| a.get_ty()).collect::<Vec<_>>();

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

            let Some(generic_func_name) =
                ctx.resolve_generic_callee_name(func_name, &arg_tys, &new_ty)
            else {
                return MonoExpr::ECall {
                    func: Box::new(new_func),
                    args: new_args,
                    ty: new_ty,
                };
            };
            let Some(callee) = ctx.orig_fns.get(&generic_func_name) else {
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
            for ((pt, at), arg) in callee_param_tys
                .iter()
                .zip(arg_tys.iter())
                .zip(new_args.iter())
            {
                if let Err(e) = unify(pt, at, &mut call_subst) {
                    if mono_expr_always_exits_control_flow(arg) {
                        continue;
                    }
                    panic!(
                        "monomorphization unification failed for {}: {}",
                        generic_func_name, e
                    );
                }
            }
            if let Err(e) = unify(&callee.ret_ty, &new_ty, &mut call_subst) {
                panic!(
                    "monomorphization return type unification failed for {}: {}",
                    generic_func_name, e
                );
            }

            // Ensure the substitution yields concrete types (no TParam in bindings)
            if !subst_is_fully_concrete(&callee.generics, &call_subst) {
                // If we cannot determine concrete types here, keep the call as-is.
                // This should not happen for reachable instances from monomorphic roots.
                return MonoExpr::ECall {
                    func: Box::new(new_func),
                    args: new_args,
                    ty: new_ty,
                };
            }

            let spec = ctx.ensure_instance(&generic_func_name, call_subst);
            MonoExpr::ECall {
                func: Box::new(MonoExpr::EVar {
                    name: spec,
                    ty: new_func.get_ty(),
                }),
                args: new_args,
                ty: new_ty,
            }
        }
        core::Expr::EToDyn {
            trait_name,
            for_ty,
            expr,
            ty,
        } => {
            let concrete_for_ty = subst_ty(&for_ty, s);
            ensure_trait_impls_for_dyn(ctx, &trait_name.0, &concrete_for_ty);
            MonoExpr::EToDyn {
                trait_name,
                for_ty: concrete_for_ty,
                expr: Box::new(mono_expr(ctx, &expr, s)),
                ty: subst_ty(&ty, s),
            }
        }
        core::Expr::EDynCall {
            trait_name,
            method_name,
            receiver,
            args,
            ty,
        } => MonoExpr::EDynCall {
            trait_name,
            method_name,
            receiver: Box::new(mono_expr(ctx, &receiver, s)),
            args: args.iter().map(|a| mono_expr(ctx, a, s)).collect(),
            ty: subst_ty(&ty, s),
        },
        core::Expr::ETraitCall {
            trait_name,
            method_name,
            receiver,
            args,
            ty,
        } => {
            if trait_name.0 == "ToString" && method_name.0 == "to_string" && args.is_empty() {
                fn call_unary_builtin(func_name: &str, arg: MonoExpr, ret_ty: Ty) -> MonoExpr {
                    let arg_ty = arg.get_ty();
                    MonoExpr::ECall {
                        func: Box::new(MonoExpr::EVar {
                            name: func_name.to_string(),
                            ty: Ty::TFunc {
                                params: vec![arg_ty],
                                ret_ty: Box::new(ret_ty.clone()),
                            },
                        }),
                        args: vec![arg],
                        ty: ret_ty,
                    }
                }

                fn to_string_expr(expr: MonoExpr, ty: Ty) -> MonoExpr {
                    match ty.clone() {
                        Ty::TString => expr,
                        Ty::TUnit => call_unary_builtin("unit_to_string", expr, Ty::TString),
                        Ty::TBool => call_unary_builtin("bool_to_string", expr, Ty::TString),
                        Ty::TInt8 => call_unary_builtin("int8_to_string", expr, Ty::TString),
                        Ty::TInt16 => call_unary_builtin("int16_to_string", expr, Ty::TString),
                        Ty::TInt32 => call_unary_builtin("int32_to_string", expr, Ty::TString),
                        Ty::TInt64 => call_unary_builtin("int64_to_string", expr, Ty::TString),
                        Ty::TUint8 => call_unary_builtin("uint8_to_string", expr, Ty::TString),
                        Ty::TUint16 => call_unary_builtin("uint16_to_string", expr, Ty::TString),
                        Ty::TUint32 => call_unary_builtin("uint32_to_string", expr, Ty::TString),
                        Ty::TUint64 => call_unary_builtin("uint64_to_string", expr, Ty::TString),
                        Ty::TFloat32 => call_unary_builtin("float32_to_string", expr, Ty::TString),
                        Ty::TFloat64 => call_unary_builtin("float64_to_string", expr, Ty::TString),
                        Ty::TChar => call_unary_builtin("char_to_string", expr, Ty::TString),
                        Ty::TStruct { name } if name == "GoError" => {
                            call_unary_builtin("go_error_to_string", expr, Ty::TString)
                        }
                        Ty::TRef { elem } => {
                            let inner_ty = *elem;
                            let inner = call_unary_builtin("ref_get", expr, inner_ty.clone());
                            let inner_str = to_string_expr(inner, inner_ty);
                            let prefix = MonoExpr::EPrim {
                                value: Prim::string("ref(".to_string()),
                                ty: Ty::TString,
                            };
                            let suffix = MonoExpr::EPrim {
                                value: Prim::string(")".to_string()),
                                ty: Ty::TString,
                            };
                            let with_prefix = MonoExpr::EBinary {
                                op: common_defs::BinaryOp::Add,
                                lhs: Box::new(prefix),
                                rhs: Box::new(inner_str),
                                ty: Ty::TString,
                            };
                            MonoExpr::EBinary {
                                op: common_defs::BinaryOp::Add,
                                lhs: Box::new(with_prefix),
                                rhs: Box::new(suffix),
                                ty: Ty::TString,
                            }
                        }
                        Ty::TDyn { trait_name } if trait_name == "ToString" => MonoExpr::EDynCall {
                            trait_name: TastIdent("ToString".to_string()),
                            method_name: TastIdent("to_string".to_string()),
                            receiver: Box::new(expr),
                            args: vec![],
                            ty: Ty::TString,
                        },
                        _ => {
                            let func_name = trait_impl_fn_name(
                                &TastIdent("ToString".to_string()),
                                &ty,
                                "to_string",
                            );
                            MonoExpr::ECall {
                                func: Box::new(MonoExpr::EVar {
                                    name: func_name,
                                    ty: Ty::TFunc {
                                        params: vec![ty.clone()],
                                        ret_ty: Box::new(Ty::TString),
                                    },
                                }),
                                args: vec![expr],
                                ty: Ty::TString,
                            }
                        }
                    }
                }

                let receiver = mono_expr(ctx, &receiver, s);
                let receiver_ty = receiver.get_ty();
                return to_string_expr(receiver, receiver_ty);
            }

            let receiver = mono_expr(ctx, &receiver, s);
            let dyn_args = args
                .iter()
                .map(|arg| mono_expr(ctx, arg, s))
                .collect::<Vec<_>>();
            let new_ty = subst_ty(&ty, s);

            if let Ty::TDyn {
                trait_name: dyn_trait_name,
            } = receiver.get_ty()
                && dyn_trait_name == trait_name.0
            {
                return MonoExpr::EDynCall {
                    trait_name,
                    method_name,
                    receiver: Box::new(receiver),
                    args: dyn_args,
                    ty: new_ty,
                };
            }

            let mut all_args = Vec::with_capacity(args.len() + 1);
            all_args.push(receiver);
            all_args.extend(dyn_args);
            let receiver_ty = all_args[0].get_ty();
            let func_name = trait_impl_fn_name(&trait_name, &receiver_ty, &method_name.0);
            let param_tys = all_args.iter().map(|a| a.get_ty()).collect::<Vec<_>>();
            let func_ty = Ty::TFunc {
                params: param_tys.clone(),
                ret_ty: Box::new(new_ty.clone()),
            };

            let resolved_name = if ctx.orig_fns.contains_key(&func_name) {
                func_name.clone()
            } else if let Some(generic_name) =
                ctx.resolve_generic_callee_name(&func_name, &param_tys, &new_ty)
            {
                if let Some(callee) = ctx.orig_fns.get(&generic_name).cloned()
                    && fn_is_generic(&callee)
                {
                    let mut call_subst: Subst = IndexMap::new();
                    for ((_, pt), at) in callee.params.iter().zip(param_tys.iter()) {
                        let _ = unify(pt, at, &mut call_subst);
                    }
                    let _ = unify(&callee.ret_ty, &new_ty, &mut call_subst);
                    if subst_is_fully_concrete(&callee.generics, &call_subst) {
                        ctx.ensure_instance(&generic_name, call_subst)
                    } else {
                        func_name.clone()
                    }
                } else {
                    generic_name
                }
            } else {
                func_name.clone()
            };

            MonoExpr::ECall {
                func: Box::new(MonoExpr::EVar {
                    name: resolved_name,
                    ty: func_ty,
                }),
                args: all_args,
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
    map: IndexMap<(String, Vec<Ty>), TastIdent>,
    enum_base: IndexMap<TastIdent, EnumDef>,
    struct_base: IndexMap<TastIdent, StructDef>,
    fn_renames: IndexMap<String, String>,
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
            fn_renames: IndexMap::new(),
        }
    }

    fn ensure_instance(&mut self, name: &str, args: &[Ty]) -> TastIdent {
        let collapsed_args: Vec<Ty> = args.iter().map(|a| self.collapse_type_apps(a)).collect();
        let key = (name.to_string(), collapsed_args.clone());
        if let Some(u) = self.map.get(&key) {
            return u.clone();
        }
        let suffix = if collapsed_args.is_empty() {
            "".to_string()
        } else {
            format!(
                "__{}",
                collapsed_args
                    .iter()
                    .map(ty_compact)
                    .collect::<Vec<_>>()
                    .join("__")
            )
        };
        let new_name = TastIdent::new(&format!("{}{}", name, suffix));
        self.map.insert(key.clone(), new_name.clone());

        let ident = TastIdent::new(name);

        if let Some(generic_def) = self.enum_base.get(&ident) {
            let mut subst: IndexMap<String, Ty> = IndexMap::new();
            if generic_def.generics.len() != collapsed_args.len() {
                panic!(
                    "enum generic argument length mismatch for {}: expected {}, got {}",
                    name,
                    generic_def.generics.len(),
                    collapsed_args.len()
                );
            }
            for (g, a) in generic_def.generics.iter().zip(collapsed_args.iter()) {
                subst.insert(g.0.clone(), a.clone());
            }

            // Substitute variant field types and also collapse nested enum/struct apps
            let mut new_variants: Vec<(TastIdent, Vec<Ty>)> = Vec::new();
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
            if generic_def.generics.len() != collapsed_args.len() {
                panic!(
                    "struct generic argument length mismatch for {}: expected {}, got {}",
                    name,
                    generic_def.generics.len(),
                    collapsed_args.len()
                );
            }
            for (g, a) in generic_def.generics.iter().zip(collapsed_args.iter()) {
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
                let ident = TastIdent::new(&base_name);
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
            Ty::TSlice { elem } => Ty::TSlice {
                elem: Box::new(self.collapse_type_apps(elem)),
            },
            Ty::TRef { elem } => Ty::TRef {
                elem: Box::new(self.collapse_type_apps(elem)),
            },
            Ty::TVec { elem } => Ty::TVec {
                elem: Box::new(self.collapse_type_apps(elem)),
            },
            Ty::THashMap { key, value } => Ty::THashMap {
                key: Box::new(self.collapse_type_apps(key)),
                value: Box::new(self.collapse_type_apps(value)),
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

fn rewrite_block_types(block: MonoBlock, m: &mut TypeMono<'_>) -> MonoBlock {
    MonoBlock {
        stmts: block
            .stmts
            .into_iter()
            .map(|stmt| MonoLetStmt {
                name: stmt.name,
                value: rewrite_expr_types(stmt.value, m),
                ty: m.collapse_type_apps(&stmt.ty),
            })
            .collect(),
        tail: block
            .tail
            .map(|tail| Box::new(rewrite_expr_types(*tail, m))),
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
            MonoExpr::EPrim { value, ty }
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
        MonoExpr::EBlock { block, ty } => MonoExpr::EBlock {
            block: Box::new(rewrite_block_types(*block, m)),
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
        MonoExpr::EBreak { ty } => MonoExpr::EBreak {
            ty: m.collapse_type_apps(&ty),
        },
        MonoExpr::EContinue { ty } => MonoExpr::EContinue {
            ty: m.collapse_type_apps(&ty),
        },
        MonoExpr::EReturn { expr, ty } => MonoExpr::EReturn {
            expr: expr.map(|expr| Box::new(rewrite_expr_types(*expr, m))),
            ty: m.collapse_type_apps(&ty),
        },
        MonoExpr::EGo { expr, ty } => MonoExpr::EGo {
            expr: Box::new(rewrite_expr_types(*expr, m)),
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
        MonoExpr::EUnary { op, expr, ty } => MonoExpr::EUnary {
            op,
            expr: Box::new(rewrite_expr_types(*expr, m)),
            ty: m.collapse_type_apps(&ty),
        },
        MonoExpr::EBinary { op, lhs, rhs, ty } => MonoExpr::EBinary {
            op,
            lhs: Box::new(rewrite_expr_types(*lhs, m)),
            rhs: Box::new(rewrite_expr_types(*rhs, m)),
            ty: m.collapse_type_apps(&ty),
        },
        MonoExpr::EAssign {
            name,
            value,
            target_ty,
            ty,
        } => MonoExpr::EAssign {
            name,
            value: Box::new(rewrite_expr_types(*value, m)),
            target_ty: m.collapse_type_apps(&target_ty),
            ty: m.collapse_type_apps(&ty),
        },
        MonoExpr::ECall { func, args, ty } => MonoExpr::ECall {
            func: Box::new(rewrite_expr_types(*func, m)),
            args: args.into_iter().map(|a| rewrite_expr_types(a, m)).collect(),
            ty: m.collapse_type_apps(&ty),
        },
        MonoExpr::EToDyn {
            trait_name,
            for_ty,
            expr,
            ty,
        } => MonoExpr::EToDyn {
            trait_name,
            for_ty: m.collapse_type_apps(&for_ty),
            expr: Box::new(rewrite_expr_types(*expr, m)),
            ty: m.collapse_type_apps(&ty),
        },
        MonoExpr::EDynCall {
            trait_name,
            method_name,
            receiver,
            args,
            ty,
        } => MonoExpr::EDynCall {
            trait_name,
            method_name,
            receiver: Box::new(rewrite_expr_types(*receiver, m)),
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

fn rename_block_refs(block: MonoBlock, renames: &IndexMap<String, String>) -> MonoBlock {
    MonoBlock {
        stmts: block
            .stmts
            .into_iter()
            .map(|stmt| MonoLetStmt {
                name: stmt.name,
                value: rename_expr_refs(stmt.value, renames),
                ty: stmt.ty,
            })
            .collect(),
        tail: block
            .tail
            .map(|tail| Box::new(rename_expr_refs(*tail, renames))),
    }
}

fn rename_expr_refs(e: MonoExpr, renames: &IndexMap<String, String>) -> MonoExpr {
    match e {
        MonoExpr::EVar { name, ty } => {
            let new_name = renames.get(&name).cloned().unwrap_or(name);
            MonoExpr::EVar { name: new_name, ty }
        }
        MonoExpr::ECall { func, args, ty } => MonoExpr::ECall {
            func: Box::new(rename_expr_refs(*func, renames)),
            args: args
                .into_iter()
                .map(|a| rename_expr_refs(a, renames))
                .collect(),
            ty,
        },
        MonoExpr::EBlock { block, ty } => MonoExpr::EBlock {
            block: Box::new(rename_block_refs(*block, renames)),
            ty,
        },
        MonoExpr::EMatch {
            expr,
            arms,
            default,
            ty,
        } => MonoExpr::EMatch {
            expr: Box::new(rename_expr_refs(*expr, renames)),
            arms: arms
                .into_iter()
                .map(|arm| MonoArm {
                    lhs: rename_expr_refs(arm.lhs, renames),
                    body: rename_expr_refs(arm.body, renames),
                })
                .collect(),
            default: default.map(|d| Box::new(rename_expr_refs(*d, renames))),
            ty,
        },
        MonoExpr::EIf {
            cond,
            then_branch,
            else_branch,
            ty,
        } => MonoExpr::EIf {
            cond: Box::new(rename_expr_refs(*cond, renames)),
            then_branch: Box::new(rename_expr_refs(*then_branch, renames)),
            else_branch: Box::new(rename_expr_refs(*else_branch, renames)),
            ty,
        },
        MonoExpr::EConstr {
            constructor,
            args,
            ty,
        } => MonoExpr::EConstr {
            constructor,
            args: args
                .into_iter()
                .map(|a| rename_expr_refs(a, renames))
                .collect(),
            ty,
        },
        MonoExpr::ETuple { items, ty } => MonoExpr::ETuple {
            items: items
                .into_iter()
                .map(|a| rename_expr_refs(a, renames))
                .collect(),
            ty,
        },
        MonoExpr::EArray { items, ty } => MonoExpr::EArray {
            items: items
                .into_iter()
                .map(|a| rename_expr_refs(a, renames))
                .collect(),
            ty,
        },
        MonoExpr::EClosure { params, body, ty } => MonoExpr::EClosure {
            params,
            body: Box::new(rename_expr_refs(*body, renames)),
            ty,
        },
        MonoExpr::EWhile { cond, body, ty } => MonoExpr::EWhile {
            cond: Box::new(rename_expr_refs(*cond, renames)),
            body: Box::new(rename_expr_refs(*body, renames)),
            ty,
        },
        MonoExpr::EConstrGet {
            expr,
            constructor,
            field_index,
            ty,
        } => MonoExpr::EConstrGet {
            expr: Box::new(rename_expr_refs(*expr, renames)),
            constructor,
            field_index,
            ty,
        },
        MonoExpr::EUnary { op, expr, ty } => MonoExpr::EUnary {
            op,
            expr: Box::new(rename_expr_refs(*expr, renames)),
            ty,
        },
        MonoExpr::EBinary { op, lhs, rhs, ty } => MonoExpr::EBinary {
            op,
            lhs: Box::new(rename_expr_refs(*lhs, renames)),
            rhs: Box::new(rename_expr_refs(*rhs, renames)),
            ty,
        },
        MonoExpr::EAssign {
            name,
            value,
            target_ty,
            ty,
        } => {
            let new_name = renames.get(&name).cloned().unwrap_or(name);
            MonoExpr::EAssign {
                name: new_name,
                value: Box::new(rename_expr_refs(*value, renames)),
                target_ty,
                ty,
            }
        }
        MonoExpr::EToDyn {
            trait_name,
            for_ty,
            expr,
            ty,
        } => MonoExpr::EToDyn {
            trait_name,
            for_ty,
            expr: Box::new(rename_expr_refs(*expr, renames)),
            ty,
        },
        MonoExpr::EDynCall {
            trait_name,
            method_name,
            receiver,
            args,
            ty,
        } => MonoExpr::EDynCall {
            trait_name,
            method_name,
            receiver: Box::new(rename_expr_refs(*receiver, renames)),
            args: args
                .into_iter()
                .map(|a| rename_expr_refs(a, renames))
                .collect(),
            ty,
        },
        MonoExpr::EProj { tuple, index, ty } => MonoExpr::EProj {
            tuple: Box::new(rename_expr_refs(*tuple, renames)),
            index,
            ty,
        },
        MonoExpr::EGo { expr, ty } => MonoExpr::EGo {
            expr: Box::new(rename_expr_refs(*expr, renames)),
            ty,
        },
        MonoExpr::EReturn { expr, ty } => MonoExpr::EReturn {
            expr: expr.map(|expr| Box::new(rename_expr_refs(*expr, renames))),
            ty,
        },
        MonoExpr::EBreak { .. } | MonoExpr::EContinue { .. } | MonoExpr::EPrim { .. } => e,
    }
}

// Monomorphize Core IR by specializing generic functions per concrete call site.
// Produces a file containing only monomorphic functions reachable from monomorphic roots.
pub fn mono(genv: GlobalTypeEnv, file: core::File) -> Result<(MonoFile, GlobalMonoEnv), String> {
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

        ctx.active_instances.push((orig_name.clone(), s.clone()));
        let new_params = orig_params
            .iter()
            .map(|(n, t)| (n.clone(), subst_ty(t, &s)))
            .collect();
        let new_ret = subst_ty(&orig_ret, &s);
        let new_body = mono_block(&mut ctx, &orig_body, &s);
        ctx.active_instances.pop();
        if let Some(error) = ctx.error.take() {
            return Err(error);
        }

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
        let params: Vec<(String, Ty)> = f
            .params
            .into_iter()
            .map(|(n, t)| (n, m.collapse_type_apps(&t)))
            .collect();
        let ret_ty = m.collapse_type_apps(&f.ret_ty);
        let body = rewrite_block_types(f.body, &mut m);

        let new_name = if let Some((trait_name, _, method_name)) = parse_trait_impl_fn_name(&f.name)
        {
            if let Some(self_param_ty) = params.first().map(|(_, t)| t.clone()) {
                let candidate = trait_impl_fn_name(
                    &TastIdent(trait_name.to_string()),
                    &self_param_ty,
                    method_name,
                );
                if candidate != f.name {
                    m.fn_renames.insert(f.name.clone(), candidate.clone());
                }
                candidate
            } else {
                f.name.clone()
            }
        } else {
            f.name.clone()
        };

        let fn_ty = Ty::TFunc {
            params: params.iter().map(|(_, t)| t.clone()).collect(),
            ret_ty: Box::new(ret_ty.clone()),
        };
        m.monoenv.insert_func(new_name.clone(), fn_ty);

        new_fns.push(MonoFn {
            name: new_name,
            params,
            ret_ty,
            body,
        });
    }

    // Drop all generic enum defs to avoid Go backend panics
    m.monoenv.retain_enums(|_n, def| def.generics.is_empty());
    m.monoenv.retain_structs(|_n, def| def.generics.is_empty());

    // Collapse field types of non-generic structs/enums that reference generic types
    let struct_names: Vec<TastIdent> = m.monoenv.structs().map(|(n, _)| n.clone()).collect();
    for name in struct_names {
        let fields = m.monoenv.struct_def_mut(&name).map(|d| d.fields.clone());
        if let Some(fields) = fields {
            let new_fields: Vec<(TastIdent, Ty)> = fields
                .iter()
                .map(|(fname, fty)| (fname.clone(), m.collapse_type_apps(fty)))
                .collect();
            if let Some(def) = m.monoenv.struct_def_mut(&name) {
                def.fields = new_fields;
            }
        }
    }
    let enum_names: Vec<TastIdent> = m.monoenv.enums().map(|(n, _)| n.clone()).collect();
    for name in enum_names {
        let variants = m.monoenv.get_enum_mut(&name).map(|d| d.variants.clone());
        if let Some(variants) = variants {
            let new_variants: Vec<(TastIdent, Vec<Ty>)> = variants
                .iter()
                .map(|(vname, vtys)| {
                    let new_tys: Vec<Ty> = vtys.iter().map(|t| m.collapse_type_apps(t)).collect();
                    (vname.clone(), new_tys)
                })
                .collect();
            if let Some(def) = m.monoenv.get_enum_mut(&name) {
                def.variants = new_variants;
            }
        }
    }

    let fn_renames = m.fn_renames.clone();
    if !fn_renames.is_empty() {
        for f in new_fns.iter_mut() {
            f.body = rename_block_refs(f.body.clone(), &fn_renames);
        }
    }

    let result = MonoFile { toplevels: new_fns };
    Ok((result, monoenv))
}
