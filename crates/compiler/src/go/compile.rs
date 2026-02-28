use crate::{
    anf::{self, GlobalAnfEnv},
    common::{Constructor, Prim},
    env::{EnumDef, Gensym, GlobalTypeEnv, InherentImplKey, StructDef},
    go::goast::{self, go_type_name_for, tast_ty_to_go_type},
    go::mangle::{encode_ty, go_ident},
    lift::{GlobalLiftEnv, is_closure_env_struct},
    names::{inherent_method_fn_name, parse_trait_impl_fn_name, trait_impl_fn_name, ty_compact},
    package_names::{ENTRY_FUNCTION, ENTRY_WRAPPER_FUNCTION},
    tast::{self, TastIdent},
};

use indexmap::IndexSet;
use std::collections::{HashMap, HashSet};

use super::goty;
use super::runtime;

#[derive(Debug, Clone)]
pub struct GlobalGoEnv {
    pub genv: GlobalTypeEnv,
    pub liftenv: GlobalLiftEnv,
}

impl Default for GlobalGoEnv {
    fn default() -> Self {
        let genv = crate::builtins::builtin_env();
        let monoenv = crate::mono::GlobalMonoEnv::from_genv(genv.clone());
        let liftenv = crate::lift::GlobalLiftEnv::from_monoenv(monoenv);
        Self { genv, liftenv }
    }
}

impl GlobalGoEnv {
    pub fn from_anf_env(anfenv: GlobalAnfEnv) -> Self {
        let liftenv = anfenv.liftenv.clone();
        let genv = liftenv.monoenv.genv.clone();
        Self { genv, liftenv }
    }

    pub fn enums(&self) -> impl Iterator<Item = (&TastIdent, &EnumDef)> {
        self.liftenv.enums()
    }

    pub fn structs(&self) -> impl Iterator<Item = (&TastIdent, &StructDef)> {
        self.liftenv.structs()
    }

    /// Lookup the apply method type for a closure environment struct via inherent_impls.
    pub fn closure_apply_method(&self, closure_ty: &tast::Ty) -> Option<tast::Ty> {
        let tast::Ty::TStruct { name } = closure_ty else {
            return None;
        };
        if !is_closure_env_struct(name) {
            unreachable!(
                "closure_apply_method called on non-closure environment struct {:?}",
                closure_ty
            );
        }
        self.liftenv
            .inherent_impls()
            .find(|(k, _)| matches!(k, InherentImplKey::Exact(ty) if ty == closure_ty))
            .and_then(|(_, impl_def)| impl_def.methods.get("apply"))
            .map(|scheme| scheme.ty.clone())
    }

    pub fn get_enum(&self, name: &TastIdent) -> Option<&EnumDef> {
        self.enums().find(|(k, _)| *k == name).map(|(_, v)| v)
    }

    pub fn get_struct(&self, name: &TastIdent) -> Option<&StructDef> {
        self.liftenv.get_struct(name)
    }
}

fn go_literal_from_primitive(value: &Prim, ty: &tast::Ty) -> goast::Expr {
    if matches!(value, Prim::Unit { .. }) {
        return goast::Expr::Unit {
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(bool_value) = value.as_bool() {
        return goast::Expr::Bool {
            value: bool_value,
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(str_value) = value.as_str() {
        return goast::Expr::String {
            value: str_value.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(ch) = value.as_char() {
        return goast::Expr::Int {
            value: (ch as u32).to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_int8() {
        return goast::Expr::Int {
            value: v.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_int16() {
        return goast::Expr::Int {
            value: v.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_int32() {
        return goast::Expr::Int {
            value: v.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_int64() {
        return goast::Expr::Int {
            value: v.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_uint8() {
        return goast::Expr::Int {
            value: v.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_uint16() {
        return goast::Expr::Int {
            value: v.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_uint32() {
        return goast::Expr::Int {
            value: v.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_uint64() {
        return goast::Expr::Int {
            value: v.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_float32() {
        return goast::Expr::Float {
            value: v as f64,
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_float64() {
        return goast::Expr::Float {
            value: v,
            ty: tast_ty_to_go_type(ty),
        };
    }

    panic!(
        "Unsupported primitive literal {:?} for Go code generation with type {:?}",
        value, ty
    );
}

fn compile_imm(goenv: &GlobalGoEnv, imm: &anf::ImmExpr) -> goast::Expr {
    match imm {
        anf::ImmExpr::Var { id, .. } => goast::Expr::Var {
            name: go_ident(&id.0),
            ty: tast_ty_to_go_type(&imm_ty(imm)),
        },
        anf::ImmExpr::Prim { value, .. } => {
            let ty = imm_ty(imm);
            go_literal_from_primitive(value, &ty)
        }
        anf::ImmExpr::Tag { index, ty } => goast::Expr::StructLiteral {
            fields: vec![],
            ty: variant_ty_by_index(goenv, ty, *index),
        },
    }
}

fn imm_ty(imm: &anf::ImmExpr) -> tast::Ty {
    match imm {
        anf::ImmExpr::Var { ty, .. }
        | anf::ImmExpr::Prim { ty, .. }
        | anf::ImmExpr::Tag { ty, .. } => ty.clone(),
    }
}

#[allow(dead_code)]
fn value_expr_ty(expr: &anf::ValueExpr) -> tast::Ty {
    match expr {
        anf::ValueExpr::Imm(imm) => imm_ty(imm),
        anf::ValueExpr::Constr { ty, .. }
        | anf::ValueExpr::Tuple { ty, .. }
        | anf::ValueExpr::Array { ty, .. }
        | anf::ValueExpr::ConstrGet { ty, .. }
        | anf::ValueExpr::Unary { ty, .. }
        | anf::ValueExpr::Binary { ty, .. }
        | anf::ValueExpr::Call { ty, .. }
        | anf::ValueExpr::ToDyn { ty, .. }
        | anf::ValueExpr::DynCall { ty, .. }
        | anf::ValueExpr::Go { ty, .. }
        | anf::ValueExpr::Proj { ty, .. } => ty.clone(),
    }
}

fn variant_struct_name(goenv: &GlobalGoEnv, enum_name: &str, variant_name: &str) -> String {
    // Count how many enums define a variant with this name.
    let mut count = 0;
    for (_ename, edef) in goenv.enums() {
        if edef
            .variants
            .iter()
            .any(|(v, _)| v.0.as_str() == variant_name)
        {
            count += 1;
            if count > 1 {
                break;
            }
        }
    }
    if count > 1 {
        format!("{}_{}", go_ident(enum_name), go_ident(variant_name))
    } else {
        go_ident(variant_name)
    }
}

fn lookup_variant_name(goenv: &GlobalGoEnv, ty: &tast::Ty, index: usize) -> String {
    let name = ty.get_constr_name_unsafe();
    if let Some(def) = goenv.get_enum(&TastIdent::new(&name)) {
        let (vname, _fields) = &def.variants[index];
        return variant_struct_name(goenv, &name, &vname.0);
    }
    panic!(
        "Cannot resolve variant name for ty {:?} index {}",
        ty, index
    );
}

fn variant_ty_by_index(goenv: &GlobalGoEnv, ty: &tast::Ty, index: usize) -> goty::GoType {
    let vname = lookup_variant_name(goenv, ty, index);
    let ty = tast::Ty::TStruct { name: vname };
    tast_ty_to_go_type(&ty)
}

fn go_package_alias(package_path: &str) -> String {
    let last_segment = package_path.rsplit('/').next().unwrap_or(package_path);
    let mut alias = String::new();
    for ch in last_segment.chars() {
        if ch.is_ascii_alphanumeric() {
            alias.push(ch);
        } else {
            alias.push('_');
        }
    }
    if alias.is_empty() {
        return "pkg".to_string();
    }
    if alias.chars().next().is_some_and(|c| c.is_ascii_digit()) {
        alias.insert(0, '_');
    }
    alias
}

fn substitute_ty_params(ty: &tast::Ty, subst: &HashMap<String, tast::Ty>) -> tast::Ty {
    match ty {
        tast::Ty::TVar(_)
        | tast::Ty::TUnit
        | tast::Ty::TBool
        | tast::Ty::TInt8
        | tast::Ty::TInt16
        | tast::Ty::TInt32
        | tast::Ty::TInt64
        | tast::Ty::TUint8
        | tast::Ty::TUint16
        | tast::Ty::TUint32
        | tast::Ty::TUint64
        | tast::Ty::TFloat32
        | tast::Ty::TFloat64
        | tast::Ty::TString
        | tast::Ty::TChar => ty.clone(),
        tast::Ty::TTuple { typs } => tast::Ty::TTuple {
            typs: typs
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
        },
        tast::Ty::TEnum { name } => tast::Ty::TEnum { name: name.clone() },
        tast::Ty::TStruct { name } => tast::Ty::TStruct { name: name.clone() },
        tast::Ty::TDyn { trait_name } => tast::Ty::TDyn {
            trait_name: trait_name.clone(),
        },
        tast::Ty::TApp { ty, args } => tast::Ty::TApp {
            ty: Box::new(substitute_ty_params(ty, subst)),
            args: args
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
        },
        tast::Ty::TParam { name } => subst
            .get(name)
            .cloned()
            .unwrap_or_else(|| tast::Ty::TParam { name: name.clone() }),
        tast::Ty::TArray { len, elem } => tast::Ty::TArray {
            len: *len,
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        tast::Ty::TSlice { elem } => tast::Ty::TSlice {
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        tast::Ty::TVec { elem } => tast::Ty::TVec {
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        tast::Ty::TRef { elem } => tast::Ty::TRef {
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        tast::Ty::THashMap { key, value } => tast::Ty::THashMap {
            key: Box::new(substitute_ty_params(key, subst)),
            value: Box::new(substitute_ty_params(value, subst)),
        },
        tast::Ty::TFunc { params, ret_ty } => tast::Ty::TFunc {
            params: params
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
            ret_ty: Box::new(substitute_ty_params(ret_ty, subst)),
        },
    }
}

fn instantiate_struct_fields(
    goenv: &GlobalGoEnv,
    type_name: &TastIdent,
    type_args: &[tast::Ty],
) -> Vec<(String, tast::Ty)> {
    let struct_def = goenv
        .get_struct(type_name)
        .unwrap_or_else(|| panic!("Unknown struct {}", type_name.0));

    if struct_def.generics.len() != type_args.len() {
        panic!(
            "Struct {} expects {} type arguments, but got {}",
            struct_def.name.0,
            struct_def.generics.len(),
            type_args.len()
        );
    }

    let mut subst = HashMap::new();
    for (param, arg) in struct_def.generics.iter().zip(type_args.iter()) {
        subst.insert(param.0.clone(), arg.clone());
    }

    struct_def
        .fields
        .iter()
        .map(|(fname, fty)| (fname.0.clone(), substitute_ty_params(fty, &subst)))
        .collect()
}

type RuntimeTypeSets = (
    IndexSet<tast::Ty>,
    IndexSet<tast::Ty>,
    IndexSet<tast::Ty>,
    IndexSet<tast::Ty>,
    IndexSet<tast::Ty>,
);

fn collect_runtime_types(file: &anf::File) -> RuntimeTypeSets {
    struct Collector {
        tuples: IndexSet<tast::Ty>,
        arrays: IndexSet<tast::Ty>,
        refs: IndexSet<tast::Ty>,
        hashmaps: IndexSet<tast::Ty>,
        missings: IndexSet<tast::Ty>,
    }

    impl Collector {
        fn collect_file(mut self, file: &anf::File) -> RuntimeTypeSets {
            for item in &file.toplevels {
                self.collect_fn(item);
            }
            (
                self.tuples,
                self.arrays,
                self.refs,
                self.hashmaps,
                self.missings,
            )
        }

        fn collect_fn(&mut self, item: &anf::Fn) {
            for (_, ty) in &item.params {
                self.collect_type(ty);
            }
            self.collect_type(&item.ret_ty);
            self.collect_block(&item.body);
        }

        fn collect_block(&mut self, block: &anf::Block) {
            for bind in &block.binds {
                self.collect_bind(bind);
            }
            self.collect_term(&block.term);
        }

        fn collect_bind(&mut self, bind: &anf::Bind) {
            match bind {
                anf::Bind::Let(let_bind) => {
                    self.collect_value_expr(&let_bind.value);
                    self.collect_type(&let_bind.ty);
                }
                anf::Bind::Join(join_bind) => {
                    for (_, ty) in &join_bind.params {
                        self.collect_type(ty);
                    }
                    self.collect_type(&join_bind.ret_ty);
                    self.collect_block(&join_bind.body);
                }
                anf::Bind::JoinRec(group) => {
                    for join_bind in group {
                        for (_, ty) in &join_bind.params {
                            self.collect_type(ty);
                        }
                        self.collect_type(&join_bind.ret_ty);
                        self.collect_block(&join_bind.body);
                    }
                }
            }
        }

        fn collect_term(&mut self, term: &anf::Term) {
            match term {
                anf::Term::Return(imm) => self.collect_imm(imm),
                anf::Term::Jump { args, ret_ty, .. } => {
                    for a in args {
                        self.collect_imm(a);
                    }
                    self.collect_type(ret_ty);
                }
                anf::Term::If {
                    cond,
                    then_,
                    else_,
                    ret_ty,
                } => {
                    self.collect_imm(cond);
                    self.collect_block(then_);
                    self.collect_block(else_);
                    self.collect_type(ret_ty);
                }
                anf::Term::Match {
                    scrut,
                    arms,
                    default,
                    ret_ty,
                } => {
                    self.collect_imm(scrut);
                    for arm in arms {
                        self.collect_imm(&arm.lhs);
                        self.collect_block(&arm.body);
                    }
                    if let Some(default) = default {
                        self.collect_block(default);
                    }
                    self.collect_type(ret_ty);
                }
                anf::Term::Unreachable { ty } => self.collect_type(ty),
            }
        }

        fn collect_value_expr(&mut self, expr: &anf::ValueExpr) {
            match expr {
                anf::ValueExpr::Imm(imm) => self.collect_imm(imm),
                anf::ValueExpr::Constr { args, ty, .. }
                | anf::ValueExpr::Tuple { items: args, ty }
                | anf::ValueExpr::Array { items: args, ty } => {
                    for arg in args {
                        self.collect_imm(arg);
                    }
                    self.collect_type(ty);
                }
                anf::ValueExpr::ConstrGet { expr, ty, .. } => {
                    self.collect_imm(expr);
                    self.collect_type(ty);
                }
                anf::ValueExpr::Unary { expr, ty, .. } => {
                    self.collect_imm(expr);
                    self.collect_type(ty);
                }
                anf::ValueExpr::Binary { lhs, rhs, ty, .. } => {
                    self.collect_imm(lhs);
                    self.collect_imm(rhs);
                    self.collect_type(ty);
                }
                anf::ValueExpr::Call { func, args, ty } => {
                    self.collect_imm(func);
                    for arg in args {
                        self.collect_imm(arg);
                    }
                    if let anf::ImmExpr::Var { id, .. } = func
                        && id.0 == "missing"
                    {
                        self.missings.insert(ty.clone());
                    }
                    self.collect_type(ty);
                }
                anf::ValueExpr::ToDyn {
                    for_ty, expr, ty, ..
                } => {
                    self.collect_type(for_ty);
                    self.collect_imm(expr);
                    self.collect_type(ty);
                }
                anf::ValueExpr::DynCall {
                    receiver, args, ty, ..
                } => {
                    self.collect_imm(receiver);
                    for arg in args {
                        self.collect_imm(arg);
                    }
                    self.collect_type(ty);
                }
                anf::ValueExpr::Go { closure, ty } => {
                    self.collect_imm(closure);
                    self.collect_type(ty);
                }
                anf::ValueExpr::Proj { tuple, ty, .. } => {
                    self.collect_imm(tuple);
                    self.collect_type(ty);
                }
            }
        }

        fn collect_imm(&mut self, imm: &anf::ImmExpr) {
            match imm {
                anf::ImmExpr::Var { ty, .. }
                | anf::ImmExpr::Prim { ty, .. }
                | anf::ImmExpr::Tag { ty, .. } => {
                    self.collect_type(ty);
                }
            }
        }

        fn collect_type(&mut self, ty: &tast::Ty) {
            match ty {
                tast::Ty::TTuple { typs } => {
                    if self.tuples.insert(ty.clone()) {
                        for inner in typs {
                            self.collect_type(inner);
                        }
                    }
                }
                tast::Ty::TArray { elem, .. } => {
                    if self.arrays.insert(ty.clone()) {
                        self.collect_type(elem);
                    }
                }
                tast::Ty::TSlice { elem } => {
                    self.collect_type(elem);
                }
                tast::Ty::TRef { elem } => {
                    if self.refs.insert(ty.clone()) {
                        self.collect_type(elem);
                    }
                }
                tast::Ty::THashMap { key, value } => {
                    if self.hashmaps.insert(ty.clone()) {
                        self.collect_type(key);
                        self.collect_type(value);
                    }
                }
                tast::Ty::TStruct { name: _ } => {
                    // Vec types are handled as slices, no special collection needed
                }
                tast::Ty::TApp { ty, args } => {
                    // Vec types are handled as slices, no special collection needed
                    self.collect_type(ty);
                    for arg in args {
                        self.collect_type(arg);
                    }
                }
                tast::Ty::TFunc { params, ret_ty } => {
                    for param in params {
                        self.collect_type(param);
                    }
                    self.collect_type(ret_ty);
                }
                _ => {}
            }
        }
    }

    Collector {
        tuples: IndexSet::new(),
        arrays: IndexSet::new(),
        refs: IndexSet::new(),
        hashmaps: IndexSet::new(),
        missings: IndexSet::new(),
    }
    .collect_file(file)
}

#[derive(Default)]
struct DynRequirements {
    traits: IndexSet<String>,
    vtables: IndexSet<(String, tast::Ty)>,
}

fn any_go_type() -> goty::GoType {
    goty::GoType::TName {
        name: "any".to_string(),
    }
}

fn dyn_struct_go_name(trait_name: &str) -> String {
    go_ident(&format!("dyn__{}", trait_name))
}

fn dyn_vtable_struct_go_name(trait_name: &str) -> String {
    go_ident(&format!("dyn__{}_vtable", trait_name))
}

fn dyn_vtable_ctor_go_name(trait_name: &str, for_ty: &tast::Ty) -> String {
    go_ident(&format!(
        "dyn__{}__vtable__{}",
        trait_name,
        encode_ty(for_ty)
    ))
}

fn dyn_wrap_go_name(trait_name: &str, for_ty: &tast::Ty, method_name: &str) -> String {
    go_ident(&format!(
        "dyn__{}__wrap__{}__{}",
        trait_name,
        encode_ty(for_ty),
        method_name
    ))
}

fn collect_dyn_requirements(file: &anf::File) -> DynRequirements {
    fn collect_ty(req: &mut DynRequirements, ty: &tast::Ty) {
        match ty {
            tast::Ty::TDyn { trait_name } => {
                req.traits.insert(trait_name.clone());
            }
            tast::Ty::TTuple { typs } => {
                for t in typs {
                    collect_ty(req, t);
                }
            }
            tast::Ty::TApp { ty, args } => {
                collect_ty(req, ty);
                for a in args {
                    collect_ty(req, a);
                }
            }
            tast::Ty::TArray { elem, .. } => collect_ty(req, elem),
            tast::Ty::TSlice { elem } => collect_ty(req, elem),
            tast::Ty::TVec { elem } => collect_ty(req, elem),
            tast::Ty::TRef { elem } => collect_ty(req, elem),
            tast::Ty::TFunc { params, ret_ty } => {
                for p in params {
                    collect_ty(req, p);
                }
                collect_ty(req, ret_ty);
            }
            _ => {}
        }
    }

    fn collect_imm(req: &mut DynRequirements, imm: &anf::ImmExpr) {
        match imm {
            anf::ImmExpr::Var { ty, .. }
            | anf::ImmExpr::Prim { ty, .. }
            | anf::ImmExpr::Tag { ty, .. } => collect_ty(req, ty),
        }
    }

    fn collect_value_expr(req: &mut DynRequirements, expr: &anf::ValueExpr) {
        match expr {
            anf::ValueExpr::Imm(imm) => collect_imm(req, imm),
            anf::ValueExpr::Constr { args, ty, .. } => {
                for a in args {
                    collect_imm(req, a);
                }
                collect_ty(req, ty);
            }
            anf::ValueExpr::Tuple { items, ty } | anf::ValueExpr::Array { items, ty } => {
                for a in items {
                    collect_imm(req, a);
                }
                collect_ty(req, ty);
            }
            anf::ValueExpr::ConstrGet { expr, ty, .. } => {
                collect_imm(req, expr);
                collect_ty(req, ty);
            }
            anf::ValueExpr::Unary { expr, ty, .. } => {
                collect_imm(req, expr);
                collect_ty(req, ty);
            }
            anf::ValueExpr::Binary { lhs, rhs, ty, .. } => {
                collect_imm(req, lhs);
                collect_imm(req, rhs);
                collect_ty(req, ty);
            }
            anf::ValueExpr::Call { func, args, ty } => {
                collect_imm(req, func);
                for a in args {
                    collect_imm(req, a);
                }
                collect_ty(req, ty);
            }
            anf::ValueExpr::ToDyn {
                trait_name,
                for_ty,
                expr,
                ty,
            } => {
                req.traits.insert(trait_name.0.clone());
                req.vtables.insert((trait_name.0.clone(), for_ty.clone()));
                collect_ty(req, for_ty);
                collect_imm(req, expr);
                collect_ty(req, ty);
            }
            anf::ValueExpr::DynCall {
                trait_name,
                receiver,
                args,
                ty,
                ..
            } => {
                req.traits.insert(trait_name.0.clone());
                collect_imm(req, receiver);
                for a in args {
                    collect_imm(req, a);
                }
                collect_ty(req, ty);
            }
            anf::ValueExpr::Go { closure, ty } => {
                collect_imm(req, closure);
                collect_ty(req, ty);
            }
            anf::ValueExpr::Proj { tuple, ty, .. } => {
                collect_imm(req, tuple);
                collect_ty(req, ty);
            }
        }
    }

    fn collect_term(req: &mut DynRequirements, term: &anf::Term) {
        match term {
            anf::Term::Return(imm) => collect_imm(req, imm),
            anf::Term::Jump { args, ret_ty, .. } => {
                for a in args {
                    collect_imm(req, a);
                }
                collect_ty(req, ret_ty);
            }
            anf::Term::If {
                cond,
                then_,
                else_,
                ret_ty,
            } => {
                collect_imm(req, cond);
                collect_block(req, then_);
                collect_block(req, else_);
                collect_ty(req, ret_ty);
            }
            anf::Term::Match {
                scrut,
                arms,
                default,
                ret_ty,
            } => {
                collect_imm(req, scrut);
                for arm in arms {
                    collect_imm(req, &arm.lhs);
                    collect_block(req, &arm.body);
                }
                if let Some(default) = default {
                    collect_block(req, default);
                }
                collect_ty(req, ret_ty);
            }
            anf::Term::Unreachable { ty } => collect_ty(req, ty),
        }
    }

    fn collect_bind(req: &mut DynRequirements, bind: &anf::Bind) {
        match bind {
            anf::Bind::Let(let_bind) => {
                collect_value_expr(req, &let_bind.value);
                collect_ty(req, &let_bind.ty);
            }
            anf::Bind::Join(join_bind) => {
                for (_, ty) in &join_bind.params {
                    collect_ty(req, ty);
                }
                collect_ty(req, &join_bind.ret_ty);
                collect_block(req, &join_bind.body);
            }
            anf::Bind::JoinRec(group) => {
                for join_bind in group {
                    for (_, ty) in &join_bind.params {
                        collect_ty(req, ty);
                    }
                    collect_ty(req, &join_bind.ret_ty);
                    collect_block(req, &join_bind.body);
                }
            }
        }
    }

    fn collect_block(req: &mut DynRequirements, block: &anf::Block) {
        for bind in &block.binds {
            collect_bind(req, bind);
        }
        collect_term(req, &block.term);
    }

    let mut req = DynRequirements::default();
    for f in &file.toplevels {
        for (_, ty) in &f.params {
            collect_ty(&mut req, ty);
        }
        collect_ty(&mut req, &f.ret_ty);
        collect_block(&mut req, &f.body);
    }
    req
}

fn trait_method_sigs(
    goenv: &GlobalGoEnv,
    trait_name: &str,
) -> Vec<(String, Vec<tast::Ty>, tast::Ty)> {
    let trait_def = goenv
        .genv
        .trait_env
        .trait_defs
        .get(trait_name)
        .unwrap_or_else(|| panic!("missing trait def {}", trait_name));
    trait_def
        .methods
        .iter()
        .map(|(name, scheme)| {
            let tast::Ty::TFunc { params, ret_ty } = &scheme.ty else {
                panic!("trait {}::{} is not a function", trait_name, name);
            };
            let rest = params.iter().skip(1).cloned().collect::<Vec<_>>();
            (name.clone(), rest, (**ret_ty).clone())
        })
        .collect()
}

fn gen_dyn_type_definitions(goenv: &GlobalGoEnv, req: &DynRequirements) -> Vec<goast::Item> {
    let mut traits: Vec<String> = req.traits.iter().cloned().collect();
    traits.sort();

    let mut items = Vec::new();
    for trait_name in traits {
        let vtable_struct_name = dyn_vtable_struct_go_name(&trait_name);
        let mut vtable_fields = Vec::new();
        for (method_name, params, ret_ty) in trait_method_sigs(goenv, &trait_name) {
            let mut go_params = Vec::with_capacity(params.len() + 1);
            go_params.push(any_go_type());
            go_params.extend(params.iter().map(tast_ty_to_go_type));
            let go_ret = tast_ty_to_go_type(&ret_ty);
            vtable_fields.push(goast::Field {
                name: go_ident(&method_name),
                ty: goty::GoType::TFunc {
                    params: go_params,
                    ret_ty: Box::new(go_ret),
                },
            });
        }
        items.push(goast::Item::Struct(goast::Struct {
            name: vtable_struct_name.clone(),
            fields: vtable_fields,
            methods: vec![],
        }));

        items.push(goast::Item::Struct(goast::Struct {
            name: dyn_struct_go_name(&trait_name),
            fields: vec![
                goast::Field {
                    name: "data".to_string(),
                    ty: any_go_type(),
                },
                goast::Field {
                    name: "vtable".to_string(),
                    ty: goty::GoType::TPointer {
                        elem: Box::new(goty::GoType::TName {
                            name: vtable_struct_name,
                        }),
                    },
                },
            ],
            methods: vec![],
        }));
    }
    items
}

fn build_trait_impl_name_map(
    toplevels: &[anf::Fn],
) -> std::collections::HashMap<(String, String, String), String> {
    let mut map = std::collections::HashMap::new();
    for tl in toplevels {
        if let Some((trait_name, for_ty_str, method_name)) =
            parse_trait_impl_fn_name(&tl.name)
        {
            let base_method = method_name.split("__").next().unwrap_or(method_name);
            let go_name = go_ident(&tl.name);
            map.insert(
                (
                    trait_name.to_string(),
                    for_ty_str.to_string(),
                    base_method.to_string(),
                ),
                go_name,
            );
        }
    }
    map
}

fn gen_dyn_helper_fns(
    goenv: &GlobalGoEnv,
    req: &DynRequirements,
    impl_name_map: &std::collections::HashMap<(String, String, String), String>,
) -> Vec<goast::Item> {
    let mut vtables: Vec<(String, tast::Ty)> = req.vtables.iter().cloned().collect();
    vtables.sort_by(|(t1, ty1), (t2, ty2)| {
        let k1 = (t1.clone(), encode_ty(ty1));
        let k2 = (t2.clone(), encode_ty(ty2));
        k1.cmp(&k2)
    });

    let mut items = Vec::new();
    for (trait_name, for_ty) in vtables {
        let methods = trait_method_sigs(goenv, &trait_name);

        for (method_name, params, ret_ty) in &methods {
            items.push(goast::Item::Fn(gen_dyn_wrap_fn(
                &trait_name,
                &for_ty,
                method_name,
                params,
                ret_ty,
                impl_name_map,
            )));
        }

        items.push(goast::Item::Fn(gen_dyn_vtable_ctor_fn(
            &trait_name,
            &for_ty,
            &methods,
        )));
    }
    items
}

fn gen_dyn_wrap_fn(
    trait_name: &str,
    for_ty: &tast::Ty,
    method_name: &str,
    params: &[tast::Ty],
    ret_ty: &tast::Ty,
    impl_name_map: &std::collections::HashMap<(String, String, String), String>,
) -> goast::Fn {
    let fn_name = dyn_wrap_go_name(trait_name, for_ty, method_name);

    let mut go_params = Vec::with_capacity(params.len() + 1);
    go_params.push(("self".to_string(), any_go_type()));
    for (i, pty) in params.iter().enumerate() {
        go_params.push((format!("p{}", i), tast_ty_to_go_type(pty)));
    }

    let trait_ident = TastIdent(trait_name.to_string());
    let for_ty_compact = ty_compact(for_ty);
    let impl_go_name = impl_name_map
        .get(&(
            trait_name.to_string(),
            for_ty_compact,
            method_name.to_string(),
        ))
        .cloned()
        .unwrap_or_else(|| {
            let impl_name = trait_impl_fn_name(&trait_ident, for_ty, method_name);
            go_ident(&impl_name)
        });

    let receiver_go_ty = tast_ty_to_go_type(for_ty);
    let ret_go_ty = tast_ty_to_go_type(ret_ty);
    let mut impl_param_tys = Vec::with_capacity(params.len() + 1);
    impl_param_tys.push(receiver_go_ty.clone());
    impl_param_tys.extend(params.iter().map(tast_ty_to_go_type));

    let asserted_self = goast::Expr::Cast {
        expr: Box::new(goast::Expr::Var {
            name: "self".to_string(),
            ty: any_go_type(),
        }),
        ty: receiver_go_ty.clone(),
    };

    let mut args = Vec::with_capacity(params.len() + 1);
    args.push(asserted_self);
    for (i, pty) in params.iter().enumerate() {
        args.push(goast::Expr::Var {
            name: format!("p{}", i),
            ty: tast_ty_to_go_type(pty),
        });
    }

    let call = goast::Expr::Call {
        func: Box::new(goast::Expr::Var {
            name: impl_go_name,
            ty: goty::GoType::TFunc {
                params: impl_param_tys,
                ret_ty: Box::new(ret_go_ty.clone()),
            },
        }),
        args,
        ty: ret_go_ty.clone(),
    };

    goast::Fn {
        name: fn_name,
        params: go_params,
        ret_ty: Some(ret_go_ty),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return { expr: Some(call) }],
        },
    }
}

fn gen_dyn_vtable_ctor_fn(
    trait_name: &str,
    for_ty: &tast::Ty,
    methods: &[(String, Vec<tast::Ty>, tast::Ty)],
) -> goast::Fn {
    let vtable_struct_name = dyn_vtable_struct_go_name(trait_name);
    let vtable_ptr_ty = goty::GoType::TPointer {
        elem: Box::new(goty::GoType::TName {
            name: vtable_struct_name.clone(),
        }),
    };

    let mut fields = Vec::new();
    for (method_name, params, ret_ty) in methods {
        let mut go_params = Vec::with_capacity(params.len() + 1);
        go_params.push(any_go_type());
        go_params.extend(params.iter().map(tast_ty_to_go_type));
        let field_ty = goty::GoType::TFunc {
            params: go_params,
            ret_ty: Box::new(tast_ty_to_go_type(ret_ty)),
        };
        fields.push((
            go_ident(method_name),
            goast::Expr::Var {
                name: dyn_wrap_go_name(trait_name, for_ty, method_name),
                ty: field_ty,
            },
        ));
    }

    let lit = goast::Expr::StructLiteral {
        fields,
        ty: goty::GoType::TName {
            name: vtable_struct_name,
        },
    };

    let addr = goast::Expr::UnaryOp {
        op: goast::GoUnaryOp::AddrOf,
        expr: Box::new(lit),
        ty: vtable_ptr_ty.clone(),
    };

    goast::Fn {
        name: dyn_vtable_ctor_go_name(trait_name, for_ty),
        params: vec![],
        ret_ty: Some(vtable_ptr_ty),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return { expr: Some(addr) }],
        },
    }
}

fn tuple_to_go_struct_type(ty: &tast::Ty) -> goty::GoType {
    if let tast::Ty::TTuple { typs } = ty {
        let name = go_type_name_for(ty);
        goty::GoType::TStruct {
            name,
            fields: typs
                .iter()
                .enumerate()
                .map(|(i, t)| (format!("_{}", i), tast_ty_to_go_type(t)))
                .collect(),
        }
    } else {
        panic!("expected tuple type, got {:?}", ty);
    }
}

#[cfg(any())]
mod legacy_anf_codegen {
    use super::*;

    fn compile_cexpr(goenv: &GlobalGoEnv, e: &anf::CExpr) -> goast::Expr {
        match e {
            anf::CExpr::CImm { imm } => compile_imm(goenv, imm),
            anf::CExpr::EConstr {
                constructor,
                args,
                ty,
            } => match constructor {
                Constructor::Enum(enum_constructor) => {
                    let variant_ty = variant_ty_by_index(goenv, ty, enum_constructor.index);
                    let variant_field_types = goenv
                        .get_enum(&enum_constructor.type_name)
                        .and_then(|def| def.variants.get(enum_constructor.index))
                        .map(|(_, fields)| fields.as_slice());
                    let fields = args
                        .iter()
                        .enumerate()
                        .map(|(i, a)| {
                            let field_ty = variant_field_types.and_then(|f| f.get(i));
                            let arg_ty = imm_ty(a);
                            let val = if let Some(ft) = field_ty
                                && needs_closure_to_func_wrap(&arg_ty, ft)
                            {
                                closure_to_func_lit(goenv, a, ft)
                            } else {
                                compile_imm(goenv, a)
                            };
                            (format!("_{}", i), val)
                        })
                        .collect();
                    goast::Expr::StructLiteral {
                        ty: variant_ty,
                        fields,
                    }
                }
                Constructor::Struct(struct_constructor) => {
                    let go_ty = tast_ty_to_go_type(ty);
                    let struct_def = goenv
                        .get_struct(&struct_constructor.type_name)
                        .unwrap_or_else(|| {
                            panic!("unknown struct {}", struct_constructor.type_name.0)
                        });
                    if struct_def.fields.len() != args.len() {
                        panic!(
                            "struct constructor {} expects {} args, got {}",
                            struct_constructor.type_name.0,
                            struct_def.fields.len(),
                            args.len()
                        );
                    }
                    let fields = struct_def
                        .fields
                        .iter()
                        .zip(args.iter())
                        .map(|((fname, field_ty), arg)| {
                            let arg_ty = imm_ty(arg);
                            let val = if needs_closure_to_func_wrap(&arg_ty, field_ty) {
                                closure_to_func_lit(goenv, arg, field_ty)
                            } else {
                                compile_imm(goenv, arg)
                            };
                            (go_ident(&fname.0), val)
                        })
                        .collect();
                    goast::Expr::StructLiteral { ty: go_ty, fields }
                }
            },
            anf::CExpr::ETuple { items, ty } => {
                let fields = items
                    .iter()
                    .enumerate()
                    .map(|(i, a)| (format!("_{}", i), compile_imm(goenv, a)))
                    .collect();
                goast::Expr::StructLiteral {
                    ty: tuple_to_go_struct_type(ty),
                    fields,
                }
            }
            anf::CExpr::EArray { items, ty } => {
                let elem_ty = match ty {
                    tast::Ty::TArray { elem, .. } => elem.as_ref(),
                    _ => ty,
                };
                let elems = items
                    .iter()
                    .map(|item| {
                        let arg_ty = imm_ty(item);
                        if needs_closure_to_func_wrap(&arg_ty, elem_ty) {
                            closure_to_func_lit(goenv, item, elem_ty)
                        } else {
                            compile_imm(goenv, item)
                        }
                    })
                    .collect();
                goast::Expr::ArrayLiteral {
                    elems,
                    ty: tast_ty_to_go_type(ty),
                }
            }
            anf::CExpr::EMatch { expr, .. } => match imm_ty(expr) {
                // Boolean matches are handled as statements (not expressions) in Go.
                tast::Ty::TBool => {
                    panic!("boolean match should be lowered to  goast::Stmt::If in compile_aexpr")
                }
                _ => {
                    panic!("EMatch should be lowered in compile_aexpr/compile_aexpr_assign")
                }
            },
            anf::CExpr::EIf {
                cond: _,
                then: _,
                else_: _,
                ty: _,
            } => panic!("EIf should be lowered to  goast::Stmt::If in compile_aexpr"),
            anf::CExpr::EWhile { .. } => {
                panic!("EWhile should be lowered to goast::Stmt::Loop in compile_aexpr")
            }
            anf::CExpr::EGo { .. } => {
                panic!("EGo should be handled as a statement, not as an expression")
            }
            anf::CExpr::EConstrGet {
                expr,
                constructor,
                field_index,
                ty: _,
            } => {
                let obj = compile_imm(goenv, expr);
                match constructor {
                    Constructor::Enum(enum_constructor) => {
                        let def = goenv
                            .get_enum(&enum_constructor.type_name)
                            .expect("unknown enum in EConstrGet");
                        let field_ty = def.variants[enum_constructor.index].1[*field_index].clone();
                        goast::Expr::FieldAccess {
                            obj: Box::new(obj),
                            field: format!("_{}", field_index),
                            ty: tast_ty_to_go_type(&field_ty),
                        }
                    }
                    Constructor::Struct(struct_constructor) => {
                        let scrut_ty = imm_ty(expr);
                        let (ty_name, type_args) = match scrut_ty {
                            tast::Ty::TStruct { name } => (name, Vec::new()),
                            tast::Ty::TApp { ty, args } => {
                                let base_name = ty.get_constr_name_unsafe();
                                (base_name, args)
                            }
                            other => panic!(
                                "EConstrGet on non-struct type {:?} for constructor {}",
                                other, struct_constructor.type_name.0
                            ),
                        };
                        let struct_name = &struct_constructor.type_name.0;
                        assert_eq!(
                            ty_name, *struct_name,
                            "struct constructor type mismatch: expected {}, got {}",
                            struct_name, ty_name
                        );
                        let fields = instantiate_struct_fields(
                            goenv,
                            &struct_constructor.type_name,
                            &type_args,
                        );
                        let (field_name, field_ty) = &fields[*field_index];
                        goast::Expr::FieldAccess {
                            obj: Box::new(obj),
                            field: go_ident(field_name),
                            ty: tast_ty_to_go_type(field_ty),
                        }
                    }
                }
            }
            anf::CExpr::EUnary { op, expr, ty } => {
                let go_op = match op {
                    common_defs::UnaryOp::Neg => goast::GoUnaryOp::Neg,
                    common_defs::UnaryOp::Not => goast::GoUnaryOp::Not,
                };
                goast::Expr::UnaryOp {
                    op: go_op,
                    expr: Box::new(compile_imm(goenv, expr)),
                    ty: tast_ty_to_go_type(ty),
                }
            }
            anf::CExpr::EBinary { op, lhs, rhs, ty } => {
                let go_op = match op {
                    common_defs::BinaryOp::Add => goast::GoBinaryOp::Add,
                    common_defs::BinaryOp::Sub => goast::GoBinaryOp::Sub,
                    common_defs::BinaryOp::Mul => goast::GoBinaryOp::Mul,
                    common_defs::BinaryOp::Div => goast::GoBinaryOp::Div,
                    common_defs::BinaryOp::And => goast::GoBinaryOp::And,
                    common_defs::BinaryOp::Or => goast::GoBinaryOp::Or,
                    common_defs::BinaryOp::Less => goast::GoBinaryOp::Less,
                    common_defs::BinaryOp::Greater => goast::GoBinaryOp::Greater,
                    common_defs::BinaryOp::LessEq => goast::GoBinaryOp::LessEq,
                    common_defs::BinaryOp::GreaterEq => goast::GoBinaryOp::GreaterEq,
                    common_defs::BinaryOp::Eq => goast::GoBinaryOp::Eq,
                    common_defs::BinaryOp::NotEq => goast::GoBinaryOp::NotEq,
                };
                goast::Expr::BinaryOp {
                    op: go_op,
                    lhs: Box::new(compile_imm(goenv, lhs)),
                    rhs: Box::new(compile_imm(goenv, rhs)),
                    ty: tast_ty_to_go_type(ty),
                }
            }
            anf::CExpr::EToDyn {
                trait_name,
                for_ty,
                expr,
                ty,
            } => {
                let dyn_struct_ty = tast_ty_to_go_type(ty);
                let vtable_struct_name = dyn_vtable_struct_go_name(&trait_name.0);
                let vtable_ptr_ty = goty::GoType::TPointer {
                    elem: Box::new(goty::GoType::TName {
                        name: vtable_struct_name,
                    }),
                };

                let ctor_name = dyn_vtable_ctor_go_name(&trait_name.0, for_ty);
                let ctor_ty = goty::GoType::TFunc {
                    params: vec![],
                    ret_ty: Box::new(vtable_ptr_ty.clone()),
                };
                let vtable_expr = goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: ctor_name,
                        ty: ctor_ty,
                    }),
                    args: vec![],
                    ty: vtable_ptr_ty,
                };

                goast::Expr::StructLiteral {
                    fields: vec![
                        ("data".to_string(), compile_imm(goenv, expr)),
                        ("vtable".to_string(), vtable_expr),
                    ],
                    ty: dyn_struct_ty,
                }
            }
            anf::CExpr::EDynCall {
                trait_name,
                method_name,
                receiver,
                args,
                ty,
            } => {
                let receiver_expr_for_vtable = compile_imm(goenv, receiver);
                let receiver_expr_for_data = compile_imm(goenv, receiver);

                let (method_params, method_ret) = trait_method_sigs(goenv, &trait_name.0)
                    .into_iter()
                    .find(|(name, _, _)| name == &method_name.0)
                    .map(|(_name, params, ret)| (params, ret))
                    .unwrap_or_else(|| {
                        panic!("missing trait method {}::{}", trait_name.0, method_name.0)
                    });

                let mut fn_params = Vec::with_capacity(method_params.len() + 1);
                fn_params.push(any_go_type());
                fn_params.extend(method_params.iter().map(tast_ty_to_go_type));
                let fn_ret = tast_ty_to_go_type(&method_ret);

                let vtable_ptr_expr = goast::Expr::FieldAccess {
                    obj: Box::new(receiver_expr_for_vtable),
                    field: "vtable".to_string(),
                    ty: goty::GoType::TPointer {
                        elem: Box::new(goty::GoType::TName {
                            name: dyn_vtable_struct_go_name(&trait_name.0),
                        }),
                    },
                };
                let method_expr = goast::Expr::FieldAccess {
                    obj: Box::new(vtable_ptr_expr),
                    field: go_ident(&method_name.0),
                    ty: goty::GoType::TFunc {
                        params: fn_params,
                        ret_ty: Box::new(fn_ret.clone()),
                    },
                };

                let mut call_args = Vec::with_capacity(args.len() + 1);
                call_args.push(goast::Expr::FieldAccess {
                    obj: Box::new(receiver_expr_for_data),
                    field: "data".to_string(),
                    ty: any_go_type(),
                });
                call_args.extend(args.iter().map(|arg| compile_imm(goenv, arg)));

                goast::Expr::Call {
                    func: Box::new(method_expr),
                    args: call_args,
                    ty: tast_ty_to_go_type(ty),
                }
            }
            anf::CExpr::ECall { func, args, ty } => {
                let compiled_args = args.iter().map(|arg| compile_imm(goenv, arg)).collect();
                let func_ty = tast_ty_to_go_type(&imm_ty(func));

                if let anf::ImmExpr::ImmVar { name, .. } = &func
                    && *name == "missing"
                {
                    let helper_name = runtime::missing_helper_fn_name(ty);
                    goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: helper_name,
                            ty: goty::GoType::TFunc {
                                params: vec![goty::GoType::TString],
                                ret_ty: Box::new(tast_ty_to_go_type(ty)),
                            },
                        }),
                        args: compiled_args,
                        ty: tast_ty_to_go_type(ty),
                    }
                } else if let anf::ImmExpr::ImmVar { name, .. } = &func
                    && (*name == "array_get" || *name == "array_set")
                {
                    let helper = runtime::array_helper_fn_name(name, &imm_ty(&args[0]));
                    goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: helper,
                            ty: func_ty,
                        }),
                        args: compiled_args,
                        ty: tast_ty_to_go_type(ty),
                    }
                } else if let anf::ImmExpr::ImmVar { name, .. } = &func
                    && (*name == "ref"
                        || *name == "ref_get"
                        || *name == "ref_set"
                        || *name == "ptr_eq")
                {
                    let (helper, helper_ty) = if name == "ref" {
                        let tast::Ty::TRef { elem } = ty else {
                            panic!("ref return type must be reference, got {:?}", ty);
                        };
                        let elem_go_ty = tast_ty_to_go_type(elem);
                        let ref_go_ty = tast_ty_to_go_type(ty);
                        (
                            runtime::ref_helper_fn_name("ref", ty),
                            goty::GoType::TFunc {
                                params: vec![elem_go_ty],
                                ret_ty: Box::new(ref_go_ty),
                            },
                        )
                    } else if name == "ptr_eq" {
                        let ref_ty = imm_ty(&args[0]);
                        let tast::Ty::TRef { .. } = &ref_ty else {
                            panic!("ptr_eq expects reference arguments, got {:?}", ref_ty);
                        };
                        let ref_go_ty = tast_ty_to_go_type(&ref_ty);
                        (
                            runtime::ref_helper_fn_name("ptr_eq", &ref_ty),
                            goty::GoType::TFunc {
                                params: vec![ref_go_ty.clone(), ref_go_ty.clone()],
                                ret_ty: Box::new(goty::GoType::TBool),
                            },
                        )
                    } else {
                        let ref_ty = imm_ty(&args[0]);
                        let tast::Ty::TRef { elem } = &ref_ty else {
                            panic!("{} expects reference argument, got {:?}", name, ref_ty);
                        };
                        let ref_go_ty = tast_ty_to_go_type(&ref_ty);
                        let elem_go_ty = tast_ty_to_go_type(elem);
                        let ret_ty = if name == "ref_get" {
                            elem_go_ty.clone()
                        } else {
                            goty::GoType::TUnit
                        };
                        (
                            runtime::ref_helper_fn_name(name, &ref_ty),
                            goty::GoType::TFunc {
                                params: if name == "ref_get" {
                                    vec![ref_go_ty.clone()]
                                } else {
                                    vec![ref_go_ty.clone(), elem_go_ty.clone()]
                                },
                                ret_ty: Box::new(ret_ty),
                            },
                        )
                    };
                    goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: helper,
                            ty: helper_ty,
                        }),
                        args: compiled_args,
                        ty: tast_ty_to_go_type(ty),
                    }
                } else if let anf::ImmExpr::ImmVar { name, .. } = &func
                    && (*name == "hashmap_new"
                        || *name == "hashmap_get"
                        || *name == "hashmap_set"
                        || *name == "hashmap_remove"
                        || *name == "hashmap_len"
                        || *name == "hashmap_contains")
                {
                    let map_ty = if name == "hashmap_new" {
                        ty.clone()
                    } else {
                        imm_ty(&args[0])
                    };
                    let tast::Ty::THashMap { key, value } = &map_ty else {
                        panic!("{} expects HashMap type, got {:?}", name, map_ty);
                    };

                    let map_go_ty = tast_ty_to_go_type(&map_ty);
                    let key_go_ty = tast_ty_to_go_type(key);
                    let value_go_ty = tast_ty_to_go_type(value);

                    let helper = runtime::hashmap_helper_fn_name(name, &map_ty);
                    let helper_ty = match name.as_str() {
                        "hashmap_new" => goty::GoType::TFunc {
                            params: vec![],
                            ret_ty: Box::new(map_go_ty.clone()),
                        },
                        "hashmap_get" => goty::GoType::TFunc {
                            params: vec![map_go_ty.clone(), key_go_ty.clone()],
                            ret_ty: Box::new(tast_ty_to_go_type(ty)),
                        },
                        "hashmap_set" => goty::GoType::TFunc {
                            params: vec![map_go_ty.clone(), key_go_ty.clone(), value_go_ty.clone()],
                            ret_ty: Box::new(goty::GoType::TUnit),
                        },
                        "hashmap_remove" => goty::GoType::TFunc {
                            params: vec![map_go_ty.clone(), key_go_ty.clone()],
                            ret_ty: Box::new(goty::GoType::TUnit),
                        },
                        "hashmap_len" => goty::GoType::TFunc {
                            params: vec![map_go_ty.clone()],
                            ret_ty: Box::new(goty::GoType::TInt32),
                        },
                        "hashmap_contains" => goty::GoType::TFunc {
                            params: vec![map_go_ty.clone(), key_go_ty.clone()],
                            ret_ty: Box::new(goty::GoType::TBool),
                        },
                        _ => unreachable!(),
                    };

                    goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: helper,
                            ty: helper_ty,
                        }),
                        args: compiled_args,
                        ty: tast_ty_to_go_type(ty),
                    }
                } else if let anf::ImmExpr::ImmVar { name, .. } = &func
                    && (*name == "slice"
                        || *name == "slice_get"
                        || *name == "slice_len"
                        || *name == "slice_sub")
                {
                    match name.as_str() {
                        "slice" | "slice_sub" => {
                            let mut args_iter = compiled_args.into_iter();
                            let array_arg = args_iter.next().unwrap();
                            let start_arg = args_iter.next().unwrap();
                            let end_arg = args_iter.next().unwrap();
                            goast::Expr::Slice {
                                array: Box::new(array_arg),
                                start: Box::new(start_arg),
                                end: Box::new(end_arg),
                                ty: tast_ty_to_go_type(ty),
                            }
                        }
                        "slice_get" => {
                            let mut args_iter = compiled_args.into_iter();
                            let slice_arg = args_iter.next().unwrap();
                            let index_arg = args_iter.next().unwrap();
                            goast::Expr::Index {
                                array: Box::new(slice_arg),
                                index: Box::new(index_arg),
                                ty: tast_ty_to_go_type(ty),
                            }
                        }
                        "slice_len" => {
                            let mut args_iter = compiled_args.into_iter();
                            let slice_arg = args_iter.next().unwrap();
                            goast::Expr::Call {
                                func: Box::new(goast::Expr::Var {
                                    name: "int32".to_string(),
                                    ty: goty::GoType::TFunc {
                                        params: vec![goty::GoType::TInt32],
                                        ret_ty: Box::new(goty::GoType::TInt32),
                                    },
                                }),
                                args: vec![goast::Expr::Call {
                                    func: Box::new(goast::Expr::Var {
                                        name: "len".to_string(),
                                        ty: goty::GoType::TFunc {
                                            params: vec![tast_ty_to_go_type(&imm_ty(&args[0]))],
                                            ret_ty: Box::new(goty::GoType::TInt32),
                                        },
                                    }),
                                    args: vec![slice_arg],
                                    ty: goty::GoType::TInt32,
                                }],
                                ty: tast_ty_to_go_type(ty),
                            }
                        }
                        _ => unreachable!(),
                    }
                } else if let anf::ImmExpr::ImmVar { name, .. } = &func
                    && (*name == "vec_new"
                        || *name == "vec_push"
                        || *name == "vec_get"
                        || *name == "vec_len")
                {
                    // Use Go's native slice operations directly
                    match name.as_str() {
                        "vec_new" => {
                            // vec_new() -> nil (empty slice)
                            goast::Expr::Nil {
                                ty: tast_ty_to_go_type(ty),
                            }
                        }
                        "vec_push" => {
                            // vec_push(v, elem) -> append(v, elem)
                            goast::Expr::Call {
                                func: Box::new(goast::Expr::Var {
                                    name: "append".to_string(),
                                    ty: func_ty,
                                }),
                                args: compiled_args,
                                ty: tast_ty_to_go_type(ty),
                            }
                        }
                        "vec_get" => {
                            // vec_get(v, index) -> v[index]
                            let mut args_iter = compiled_args.into_iter();
                            let v_arg = args_iter.next().unwrap();
                            let index_arg = args_iter.next().unwrap();
                            goast::Expr::Index {
                                array: Box::new(v_arg),
                                index: Box::new(index_arg),
                                ty: tast_ty_to_go_type(ty),
                            }
                        }
                        "vec_len" => {
                            // vec_len(v) -> int32(len(v))
                            let mut args_iter = compiled_args.into_iter();
                            let v_arg = args_iter.next().unwrap();
                            goast::Expr::Call {
                                func: Box::new(goast::Expr::Var {
                                    name: "int32".to_string(),
                                    ty: goty::GoType::TFunc {
                                        params: vec![goty::GoType::TInt32],
                                        ret_ty: Box::new(goty::GoType::TInt32),
                                    },
                                }),
                                args: vec![goast::Expr::Call {
                                    func: Box::new(goast::Expr::Var {
                                        name: "len".to_string(),
                                        ty: goty::GoType::TFunc {
                                            params: vec![tast_ty_to_go_type(&imm_ty(&args[0]))],
                                            ret_ty: Box::new(goty::GoType::TInt32),
                                        },
                                    }),
                                    args: vec![v_arg],
                                    ty: goty::GoType::TInt32,
                                }],
                                ty: tast_ty_to_go_type(ty),
                            }
                        }
                        _ => unreachable!(),
                    }
                } else if let anf::ImmExpr::ImmVar { name, .. } = &func
                    && let Some(extern_fn) = goenv.genv.value_env.extern_funcs.get(name)
                {
                    let alias = go_package_alias(&extern_fn.package_path);
                    goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: format!("{}.{}", alias, extern_fn.go_name),
                            ty: func_ty,
                        }),
                        args: compiled_args,
                        ty: tast_ty_to_go_type(ty),
                    }
                } else {
                    goast::Expr::Call {
                        func: Box::new(compile_imm(goenv, func)),
                        args: compiled_args,
                        ty: tast_ty_to_go_type(ty),
                    }
                }
            }
            anf::CExpr::EProj { tuple, index, ty } => {
                let obj = compile_imm(goenv, tuple);
                goast::Expr::FieldAccess {
                    obj: Box::new(obj),
                    field: format!("_{}", index),
                    ty: tast_ty_to_go_type(ty),
                }
            }
        }
    }

    fn compile_int_match_branch<F, E>(
        goenv: &GlobalGoEnv,
        scrutinee: &anf::ImmExpr,
        arms: &[anf::Arm],
        default: &Option<Box<anf::Block>>,
        build_branch: &mut F,
        extract: E,
    ) -> Vec<goast::Stmt>
    where
        F: FnMut(anf::AExpr) -> Vec<goast::Stmt>,
        E: Fn(&Prim) -> Option<String>,
    {
        let mut cases = Vec::new();
        for arm in arms {
            if let anf::ImmExpr::ImmPrim { value, .. } = &arm.lhs {
                if let Some(v) = extract(value) {
                    cases.push((
                        goast::Expr::Int {
                            value: v,
                            ty: tast_ty_to_go_type(&imm_ty(&arm.lhs)),
                        },
                        goast::Block {
                            stmts: build_branch(block_to_aexpr(arm.body.clone())),
                        },
                    ));
                } else {
                    panic!("expected integer primitive in match arm");
                }
            } else {
                panic!("expected primitive literal in integer match arm");
            }
        }
        let default_block = default.as_ref().map(|d| goast::Block {
            stmts: build_branch(block_to_aexpr((**d).clone())),
        });
        vec![goast::Stmt::SwitchExpr {
            expr: compile_imm(goenv, scrutinee),
            cases,
            default: default_block,
        }]
    }

    fn compile_float_match_branch<F, E>(
        goenv: &GlobalGoEnv,
        scrutinee: &anf::ImmExpr,
        arms: &[anf::Arm],
        default: &Option<Box<anf::Block>>,
        build_branch: &mut F,
        extract: E,
    ) -> Vec<goast::Stmt>
    where
        F: FnMut(anf::AExpr) -> Vec<goast::Stmt>,
        E: Fn(&Prim) -> Option<f64>,
    {
        let mut cases = Vec::new();
        for arm in arms {
            if let anf::ImmExpr::ImmPrim { value, .. } = &arm.lhs {
                if let Some(v) = extract(value) {
                    cases.push((
                        goast::Expr::Float {
                            value: v,
                            ty: tast_ty_to_go_type(&imm_ty(&arm.lhs)),
                        },
                        goast::Block {
                            stmts: build_branch(block_to_aexpr(arm.body.clone())),
                        },
                    ));
                } else {
                    panic!("expected float primitive in match arm");
                }
            } else {
                panic!("expected primitive literal in float match arm");
            }
        }
        let default_block = default.as_ref().map(|d| goast::Block {
            stmts: build_branch(block_to_aexpr((**d).clone())),
        });
        vec![goast::Stmt::SwitchExpr {
            expr: compile_imm(goenv, scrutinee),
            cases,
            default: default_block,
        }]
    }

    fn compile_match_branches<F>(
        goenv: &GlobalGoEnv,
        scrutinee: &anf::ImmExpr,
        arms: &[anf::Arm],
        default: &Option<Box<anf::Block>>,
        mut build_branch: F,
    ) -> Vec<goast::Stmt>
    where
        F: FnMut(anf::AExpr) -> Vec<goast::Stmt>,
    {
        match imm_ty(scrutinee) {
            tast::Ty::TUnit => {
                if let Some(first) = arms.first() {
                    return build_branch(block_to_aexpr(first.body.clone()));
                }
                if let Some(default_arm) = default.as_ref() {
                    return build_branch(block_to_aexpr((**default_arm).clone()));
                }
                Vec::new()
            }
            tast::Ty::TBool => {
                let mut cases = Vec::new();
                for arm in arms {
                    if let anf::ImmExpr::ImmPrim { value, .. } = &arm.lhs {
                        if let Some(bool_value) = value.as_bool() {
                            cases.push((
                                goast::Expr::Bool {
                                    value: bool_value,
                                    ty: tast_ty_to_go_type(&imm_ty(&arm.lhs)),
                                },
                                goast::Block {
                                    stmts: build_branch(block_to_aexpr(arm.body.clone())),
                                },
                            ));
                        } else {
                            panic!("expected boolean primitive in boolean match arm");
                        }
                    } else {
                        panic!("expected primitive literal in boolean match arm");
                    }
                }
                let default_block = default.as_ref().map(|d| goast::Block {
                    stmts: build_branch(block_to_aexpr((**d).clone())),
                });
                vec![goast::Stmt::SwitchExpr {
                    expr: compile_imm(goenv, scrutinee),
                    cases,
                    default: default_block,
                }]
            }
            tast::Ty::TInt8 => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_int8().map(|x| x.to_string())
                })
            }
            tast::Ty::TInt16 => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_int16().map(|x| x.to_string())
                })
            }
            tast::Ty::TInt32 => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_int32().map(|x| x.to_string())
                })
            }
            tast::Ty::TInt64 => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_int64().map(|x| x.to_string())
                })
            }
            tast::Ty::TUint8 => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_uint8().map(|x| x.to_string())
                })
            }
            tast::Ty::TUint16 => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_uint16().map(|x| x.to_string())
                })
            }
            tast::Ty::TUint32 => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_uint32().map(|x| x.to_string())
                })
            }
            tast::Ty::TUint64 => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_uint64().map(|x| x.to_string())
                })
            }
            tast::Ty::TChar => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_char().map(|ch| (ch as u32).to_string())
                })
            }
            tast::Ty::TFloat32 => compile_float_match_branch(
                goenv,
                scrutinee,
                arms,
                default,
                &mut build_branch,
                |v| v.as_float32().map(|x| x as f64),
            ),
            tast::Ty::TFloat64 => compile_float_match_branch(
                goenv,
                scrutinee,
                arms,
                default,
                &mut build_branch,
                |v| v.as_float64(),
            ),
            tast::Ty::TString => {
                let mut cases = Vec::new();
                for arm in arms {
                    if let anf::ImmExpr::ImmPrim { value, .. } = &arm.lhs {
                        if let Some(str_value) = value.as_str() {
                            cases.push((
                                goast::Expr::String {
                                    value: str_value.to_string(),
                                    ty: tast_ty_to_go_type(&imm_ty(&arm.lhs)),
                                },
                                goast::Block {
                                    stmts: build_branch(block_to_aexpr(arm.body.clone())),
                                },
                            ));
                        } else {
                            panic!("expected string primitive in string match arm");
                        }
                    } else {
                        panic!("expected primitive literal in string match arm");
                    }
                }
                let default_block = default.as_ref().map(|d| goast::Block {
                    stmts: build_branch(block_to_aexpr((**d).clone())),
                });
                vec![goast::Stmt::SwitchExpr {
                    expr: compile_imm(goenv, scrutinee),
                    cases,
                    default: default_block,
                }]
            }
            tast::Ty::TEnum { .. } => {
                let scrutinee_name = match scrutinee {
                    anf::ImmExpr::ImmVar { name, .. } => name.clone(),
                    _ => {
                        unreachable!("expected scrutinee to be a variable after ANF lowering")
                    }
                };
                let mut cases = Vec::new();
                for arm in arms {
                    if let anf::ImmExpr::ImmTag { index, ty } = &arm.lhs {
                        let vty = variant_ty_by_index(goenv, ty, *index);
                        cases.push((
                            vty,
                            goast::Block {
                                stmts: build_branch(block_to_aexpr(arm.body.clone())),
                            },
                        ));
                    } else {
                        panic!("expected ImmTag in enum match arm");
                    }
                }
                let default_block = default.as_ref().map(|d| goast::Block {
                    stmts: build_branch(block_to_aexpr((**d).clone())),
                });
                vec![goast::Stmt::SwitchType {
                    bind: Some(scrutinee_name),
                    expr: compile_imm(goenv, scrutinee),
                    cases,
                    default: default_block,
                }]
            }
            tast::Ty::TStruct { .. } => {
                panic!("struct matches are not supported in Go backend")
            }
            tast::Ty::TApp { ty: base, .. } => match base.as_ref() {
                tast::Ty::TEnum { .. } => {
                    let scrutinee_name = match scrutinee {
                        anf::ImmExpr::ImmVar { name, .. } => name.clone(),
                        _ => {
                            unreachable!("expected scrutinee to be a variable after ANF lowering")
                        }
                    };
                    let mut cases = Vec::new();
                    for arm in arms {
                        if let anf::ImmExpr::ImmTag { index, ty } = &arm.lhs {
                            let vty = variant_ty_by_index(goenv, ty, *index);
                            cases.push((
                                vty,
                                goast::Block {
                                    stmts: build_branch(block_to_aexpr(arm.body.clone())),
                                },
                            ));
                        } else {
                            panic!("expected ImmTag in enum match arm");
                        }
                    }
                    let default_block = default.as_ref().map(|d| goast::Block {
                        stmts: build_branch(block_to_aexpr((**d).clone())),
                    });
                    vec![goast::Stmt::SwitchType {
                        bind: Some(scrutinee_name),
                        expr: compile_imm(goenv, scrutinee),
                        cases,
                        default: default_block,
                    }]
                }
                _ => panic!(
                    "unsupported scrutinee type TApp({:?}, ..) for match in Go backend",
                    base
                ),
            },
            _ => panic!("unsupported scrutinee type for match in Go backend"),
        }
    }

    fn compile_cexpr_effect(goenv: &GlobalGoEnv, expr: &anf::CExpr) -> Vec<goast::Stmt> {
        match expr {
            anf::CExpr::CImm { .. }
            | anf::CExpr::EConstr { .. }
            | anf::CExpr::ETuple { .. }
            | anf::CExpr::EArray { .. }
            | anf::CExpr::EConstrGet { .. }
            | anf::CExpr::EUnary { .. }
            | anf::CExpr::EBinary { .. }
            | anf::CExpr::EToDyn { .. }
            | anf::CExpr::EProj { .. } => Vec::new(),
            anf::CExpr::ECall { .. } | anf::CExpr::EDynCall { .. } => {
                vec![goast::Stmt::Expr(compile_cexpr(goenv, expr))]
            }
            anf::CExpr::EGo { closure, .. } => {
                vec![compile_go(goenv, closure)]
            }
            anf::CExpr::EMatch { .. } | anf::CExpr::EIf { .. } | anf::CExpr::EWhile { .. } => {
                panic!("control-flow expressions should be handled before compile_cexpr_effect")
            }
        }
    }

    struct ClosureApplyFn {
        name: String,
        ty: tast::Ty,
        ret_ty: tast::Ty,
    }

    fn compile_go(goenv: &GlobalGoEnv, closure: &anf::ImmExpr) -> goast::Stmt {
        let closure_ty = imm_ty(closure);
        let apply = find_closure_apply_fn(goenv, &closure_ty)
            .expect("go statement closure must have an apply method");

        let apply_call = anf::CExpr::ECall {
            func: anf::ImmExpr::ImmVar {
                name: apply.name.clone(),
                ty: apply.ty.clone(),
            },
            args: vec![closure.clone()],
            ty: apply.ret_ty.clone(),
        };

        let call_expr = compile_cexpr(goenv, &apply_call);
        goast::Stmt::Go { call: call_expr }
    }

    fn find_closure_apply_fn(goenv: &GlobalGoEnv, closure_ty: &tast::Ty) -> Option<ClosureApplyFn> {
        // Look up the apply method via inherent_impls
        let fn_ty = goenv.closure_apply_method(closure_ty)?;

        let tast::Ty::TFunc { params, ret_ty } = &fn_ty else {
            return None;
        };

        if params.first()? != closure_ty {
            return None;
        }

        let apply_name = inherent_method_fn_name(closure_ty, "apply");

        Some(ClosureApplyFn {
            name: apply_name,
            ty: fn_ty.clone(),
            ret_ty: (**ret_ty).clone(),
        })
    }

    fn block_to_aexpr(block: anf::Block) -> anf::AExpr {
        let mut expr = anf::AExpr::ACExpr { expr: block.tail };
        for stmt in block.stmts.into_iter().rev() {
            let body_ty = expr.get_ty();
            expr = anf::AExpr::ALet {
                name: stmt.name,
                value: Box::new(stmt.value),
                body: Box::new(expr),
                ty: body_ty,
            };
        }
        expr
    }

    fn compile_aexpr_effect(
        goenv: &GlobalGoEnv,
        gensym: &Gensym,
        e: anf::AExpr,
    ) -> Vec<goast::Stmt> {
        match e {
            AExpr::ACExpr { expr } => match expr {
                anf::CExpr::EIf {
                    cond, then, else_, ..
                } => {
                    let cond_e = compile_imm(goenv, &cond);
                    let then_block = goast::Block {
                        stmts: compile_aexpr_effect(goenv, gensym, block_to_aexpr(*then)),
                    };
                    let else_block = goast::Block {
                        stmts: compile_aexpr_effect(goenv, gensym, block_to_aexpr(*else_)),
                    };
                    vec![goast::Stmt::If {
                        cond: cond_e,
                        then: then_block,
                        else_: Some(else_block),
                    }]
                }
                anf::CExpr::EMatch {
                    expr: scrutinee,
                    arms,
                    default,
                    ty: _,
                } => compile_match_branches(goenv, scrutinee.as_ref(), &arms, &default, |branch| {
                    compile_aexpr_effect(goenv, gensym, branch)
                }),
                anf::CExpr::EWhile { cond, body, .. } => compile_while(goenv, gensym, *cond, *body),
                other => compile_cexpr_effect(goenv, &other),
            },
            AExpr::ALet {
                name,
                value,
                body,
                ty: _,
            } => {
                let mut out = Vec::new();
                let value_expr = *value;

                match value_expr {
                    complex @ (anf::CExpr::EIf { .. }
                    | anf::CExpr::EMatch { .. }
                    | anf::CExpr::EWhile { .. }) => {
                        out.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &complex),
                            value: None,
                        });
                        out.extend(compile_aexpr_assign(
                            goenv,
                            gensym,
                            &name,
                            AExpr::ACExpr { expr: complex },
                        ));
                    }
                    anf::CExpr::EGo { ref closure, .. } => {
                        out.push(compile_go(goenv, closure));
                        out.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: goty::GoType::TUnit,
                            value: Some(goast::Expr::Unit {
                                ty: goty::GoType::TUnit,
                            }),
                        });
                        out.extend(compile_aexpr_effect(goenv, gensym, *body));
                        return out;
                    }
                    simple @ anf::CExpr::ECall { .. } => {
                        out.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &simple),
                            value: Some(compile_cexpr(goenv, &simple)),
                        });
                    }
                    simple => {
                        out.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &simple),
                            value: Some(compile_cexpr(goenv, &simple)),
                        });
                    }
                }
                out.extend(compile_aexpr_effect(goenv, gensym, *body));
                out
            }
        }
    }

    fn compile_while(
        goenv: &GlobalGoEnv,
        gensym: &Gensym,
        cond: anf::Block,
        body: anf::Block,
    ) -> Vec<goast::Stmt> {
        let cond_ty = cond.get_ty();
        if cond_ty != tast::Ty::TBool {
            panic!("while condition must have type bool, got {:?}", cond_ty);
        }

        let cond_var = gensym.gensym("cond");
        let mut stmts = Vec::new();
        stmts.push(goast::Stmt::VarDecl {
            name: go_ident(&cond_var),
            ty: goty::GoType::TBool,
            value: None,
        });

        let mut loop_body = compile_aexpr_assign(goenv, gensym, &cond_var, block_to_aexpr(cond));
        let not_cond = goast::Expr::UnaryOp {
            op: goast::GoUnaryOp::Not,
            expr: Box::new(goast::Expr::Var {
                name: go_ident(&cond_var),
                ty: goty::GoType::TBool,
            }),
            ty: goty::GoType::TBool,
        };
        loop_body.push(goast::Stmt::If {
            cond: not_cond,
            then: goast::Block {
                stmts: vec![goast::Stmt::Break],
            },
            else_: None,
        });
        loop_body.extend(compile_aexpr_effect(goenv, gensym, block_to_aexpr(body)));

        stmts.push(goast::Stmt::Loop {
            body: goast::Block { stmts: loop_body },
        });
        stmts
    }

    fn compile_aexpr_assign(
        goenv: &GlobalGoEnv,
        gensym: &Gensym,
        target: &str,
        e: anf::AExpr,
    ) -> Vec<goast::Stmt> {
        match e {
            AExpr::ACExpr { expr } => match expr {
                anf::CExpr::EIf {
                    cond, then, else_, ..
                } => {
                    let cond_e = compile_imm(goenv, &cond);
                    let then_stmts =
                        compile_aexpr_assign(goenv, gensym, target, block_to_aexpr(*then));
                    let else_stmts =
                        compile_aexpr_assign(goenv, gensym, target, block_to_aexpr(*else_));
                    vec![goast::Stmt::If {
                        cond: cond_e,
                        then: goast::Block { stmts: then_stmts },
                        else_: Some(goast::Block { stmts: else_stmts }),
                    }]
                }
                anf::CExpr::EMatch {
                    expr: scrutinee,
                    arms,
                    default,
                    ty: _,
                } => compile_match_branches(goenv, scrutinee.as_ref(), &arms, &default, |branch| {
                    compile_aexpr_assign(goenv, gensym, target, branch)
                }),
                anf::CExpr::EWhile { cond, body, .. } => {
                    let mut stmts = compile_while(goenv, gensym, *cond, *body);
                    stmts.push(goast::Stmt::Assignment {
                        name: go_ident(target),
                        value: goast::Expr::Unit {
                            ty: goty::GoType::TUnit,
                        },
                    });
                    stmts
                }
                other @ (anf::CExpr::CImm { .. }
                | anf::CExpr::EConstr { .. }
                | anf::CExpr::EConstrGet { .. }
                | anf::CExpr::EUnary { .. }
                | anf::CExpr::EBinary { .. }
                | anf::CExpr::EToDyn { .. }
                | anf::CExpr::EProj { .. }
                | anf::CExpr::ETuple { .. }
                | anf::CExpr::EArray { .. }) => vec![goast::Stmt::Assignment {
                    name: go_ident(target),
                    value: compile_cexpr(goenv, &other),
                }],
                anf::CExpr::ECall { func, args, ty } => {
                    vec![goast::Stmt::Assignment {
                        name: go_ident(target),
                        value: compile_cexpr(goenv, &anf::CExpr::ECall { func, args, ty }),
                    }]
                }
                anf::CExpr::EDynCall {
                    trait_name,
                    method_name,
                    receiver,
                    args,
                    ty,
                } => {
                    vec![goast::Stmt::Assignment {
                        name: go_ident(target),
                        value: compile_cexpr(
                            goenv,
                            &anf::CExpr::EDynCall {
                                trait_name,
                                method_name,
                                receiver,
                                args,
                                ty,
                            },
                        ),
                    }]
                }
                anf::CExpr::EGo { closure, .. } => {
                    vec![
                        compile_go(goenv, &closure),
                        goast::Stmt::Assignment {
                            name: go_ident(target),
                            value: goast::Expr::Unit {
                                ty: goty::GoType::TUnit,
                            },
                        },
                    ]
                }
            },
            AExpr::ALet {
                name,
                value,
                body,
                ty: _,
            } => {
                let mut out = Vec::new();
                let value_expr = *value;

                match value_expr {
                    complex @ (anf::CExpr::EIf { .. }
                    | anf::CExpr::EMatch { .. }
                    | anf::CExpr::EWhile { .. }) => {
                        out.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &complex),
                            value: None,
                        });
                        out.extend(compile_aexpr_assign(
                            goenv,
                            gensym,
                            &name,
                            AExpr::ACExpr { expr: complex },
                        ));
                    }
                    anf::CExpr::EGo { ref closure, .. } => {
                        out.push(compile_go(goenv, closure));
                        out.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: goty::GoType::TUnit,
                            value: Some(goast::Expr::Unit {
                                ty: goty::GoType::TUnit,
                            }),
                        });
                        out.extend(compile_aexpr_assign(goenv, gensym, target, *body));
                        return out;
                    }
                    simple @ anf::CExpr::ECall { .. } => {
                        out.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &simple),
                            value: Some(compile_cexpr(goenv, &simple)),
                        });
                    }
                    simple => {
                        out.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &simple),
                            value: Some(compile_cexpr(goenv, &simple)),
                        });
                    }
                }

                out.extend(compile_aexpr_assign(goenv, gensym, target, *body));
                out
            }
        }
    }

    fn compile_aexpr(goenv: &GlobalGoEnv, gensym: &Gensym, e: anf::AExpr) -> Vec<goast::Stmt> {
        let mut stmts = Vec::new();
        match e {
            AExpr::ACExpr { expr } => match expr {
                // Lower conditional expressions to if-statements with returns in branches
                anf::CExpr::EIf {
                    cond, then, else_, ..
                } => {
                    let cond_e = compile_imm(goenv, &cond);
                    let then_block = goast::Block {
                        stmts: compile_aexpr(goenv, gensym, block_to_aexpr(*then)),
                    };
                    let else_block = goast::Block {
                        stmts: compile_aexpr(goenv, gensym, block_to_aexpr(*else_)),
                    };
                    stmts.push(goast::Stmt::If {
                        cond: cond_e,
                        then: then_block,
                        else_: Some(else_block),
                    });
                }
                anf::CExpr::EMatch {
                    expr: scrutinee,
                    arms,
                    default,
                    ty: _,
                } => {
                    stmts.extend(compile_match_branches(
                        goenv,
                        scrutinee.as_ref(),
                        &arms,
                        &default,
                        |branch| compile_aexpr(goenv, gensym, branch),
                    ));
                }
                anf::CExpr::EWhile { cond, body, .. } => {
                    stmts.extend(compile_while(goenv, gensym, *cond, *body));
                    stmts.push(goast::Stmt::Return {
                        expr: Some(goast::Expr::Unit {
                            ty: goty::GoType::TUnit,
                        }),
                    });
                }
                _ => {
                    let e = compile_cexpr(goenv, &expr);
                    match e.get_ty() {
                        goty::GoType::TVoid => {}
                        _ => {
                            stmts.push(goast::Stmt::Return { expr: Some(e) });
                        }
                    }
                }
            },
            AExpr::ALet {
                name,
                value,
                body,
                ty: _,
            } => {
                let value_expr = *value;

                match value_expr {
                    // If RHS needs statements, declare then fill via if-lowering
                    complex @ (anf::CExpr::EIf { .. }
                    | anf::CExpr::EMatch { .. }
                    | anf::CExpr::EWhile { .. }) => {
                        stmts.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &complex),
                            value: None,
                        });
                        stmts.extend(compile_aexpr_assign(
                            goenv,
                            gensym,
                            &name,
                            AExpr::ACExpr { expr: complex },
                        ));
                    }
                    anf::CExpr::EGo { ref closure, .. } => {
                        stmts.push(compile_go(goenv, closure));
                        stmts.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: goty::GoType::TUnit,
                            value: Some(goast::Expr::Unit {
                                ty: goty::GoType::TUnit,
                            }),
                        });
                        stmts.extend(compile_aexpr(goenv, gensym, *body));
                        return stmts;
                    }
                    simple @ anf::CExpr::ECall { .. } => {
                        stmts.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &simple),
                            value: Some(compile_cexpr(goenv, &simple)),
                        });
                    }
                    simple => {
                        stmts.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &simple),
                            value: Some(compile_cexpr(goenv, &simple)),
                        });
                    }
                }
                stmts.extend(compile_aexpr(goenv, gensym, *body));
            }
        }
        stmts
    }

    fn compile_fn(goenv: &GlobalGoEnv, gensym: &Gensym, f: anf::Fn) -> goast::Fn {
        let mut params = Vec::new();
        for (name, ty) in f.params {
            params.push((go_ident(&name), tast_ty_to_go_type(&ty)));
        }

        let go_ret_ty = tast_ty_to_go_type(&f.ret_ty);

        let is_entry =
            f.name == ENTRY_FUNCTION || f.name.ends_with(&format!("::{}", ENTRY_FUNCTION));
        let patched_name = if is_entry {
            ENTRY_WRAPPER_FUNCTION.to_string()
        } else {
            go_ident(&f.name)
        };

        let body = block_to_aexpr(f.body);

        let (ret_ty, body_stmts) = match go_ret_ty {
            goty::GoType::TVoid => (None, compile_aexpr(goenv, gensym, body)),
            _ => {
                let ret_name = gensym.gensym("ret");
                let mut stmts = Vec::new();

                stmts.push(goast::Stmt::VarDecl {
                    name: go_ident(&ret_name),
                    ty: go_ret_ty.clone(),
                    value: None,
                });

                stmts.extend(compile_aexpr_assign(goenv, gensym, &ret_name, body));

                stmts.push(goast::Stmt::Return {
                    expr: Some(goast::Expr::Var {
                        name: go_ident(&ret_name),
                        ty: go_ret_ty.clone(),
                    }),
                });

                (Some(go_ret_ty), stmts)
            }
        };

        goast::Fn {
            name: patched_name,
            params,
            ret_ty,
            body: goast::Block { stmts: body_stmts },
        }
    }
}

#[derive(Debug, Clone)]
struct ClosureApplyFn {
    name: String,
    ty: tast::Ty,
    ret_ty: tast::Ty,
}

fn find_closure_apply_fn(goenv: &GlobalGoEnv, closure_ty: &tast::Ty) -> Option<ClosureApplyFn> {
    let fn_ty = goenv.closure_apply_method(closure_ty)?;
    let tast::Ty::TFunc { params, ret_ty } = &fn_ty else {
        return None;
    };
    if params.first()? != closure_ty {
        return None;
    }

    let apply_name = inherent_method_fn_name(closure_ty, "apply");
    Some(ClosureApplyFn {
        name: apply_name,
        ty: fn_ty.clone(),
        ret_ty: (**ret_ty).clone(),
    })
}

fn compile_go(goenv: &GlobalGoEnv, closure: &anf::ImmExpr) -> goast::Stmt {
    let closure_ty = imm_ty(closure);
    let apply = find_closure_apply_fn(goenv, &closure_ty)
        .expect("go statement closure must have an apply method");

    let apply_expr = goast::Expr::Var {
        name: go_ident(&apply.name),
        ty: tast_ty_to_go_type(&apply.ty),
    };

    let call_expr = goast::Expr::Call {
        func: Box::new(apply_expr),
        args: vec![compile_imm(goenv, closure)],
        ty: tast_ty_to_go_type(&apply.ret_ty),
    };

    goast::Stmt::Go { call: call_expr }
}

#[derive(Debug)]
struct CompiledValue {
    stmts: Vec<goast::Stmt>,
    expr: goast::Expr,
}

fn compile_call(
    goenv: &GlobalGoEnv,
    func: &anf::ImmExpr,
    args: &[anf::ImmExpr],
    ty: &tast::Ty,
) -> goast::Expr {
    let func_tast_ty = imm_ty(func);
    let param_types: Vec<tast::Ty> = match &func_tast_ty {
        tast::Ty::TFunc { params, .. } => params.clone(),
        _ => Vec::new(),
    };
    let compiled_args = args
        .iter()
        .enumerate()
        .map(|(i, arg)| {
            let arg_ty = imm_ty(arg);
            if let Some(param_ty) = param_types.get(i)
                && needs_closure_to_func_wrap(&arg_ty, param_ty)
            {
                return closure_to_func_lit(goenv, arg, param_ty);
            }
            compile_imm(goenv, arg)
        })
        .collect::<Vec<_>>();
    let func_ty = tast_ty_to_go_type(&func_tast_ty);

    if let anf::ImmExpr::Var { id, .. } = func
        && id.0 == "missing"
    {
        let helper_name = runtime::missing_helper_fn_name(ty);
        return goast::Expr::Call {
            func: Box::new(goast::Expr::Var {
                name: helper_name,
                ty: goty::GoType::TFunc {
                    params: vec![goty::GoType::TString],
                    ret_ty: Box::new(tast_ty_to_go_type(ty)),
                },
            }),
            args: compiled_args,
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let anf::ImmExpr::Var { id, .. } = func
        && (id.0 == "array_get" || id.0 == "array_set")
    {
        let array_ty = imm_ty(&args[0]);
        let helper = runtime::array_helper_fn_name(&id.0, &array_ty);
        return goast::Expr::Call {
            func: Box::new(goast::Expr::Var {
                name: helper,
                ty: func_ty,
            }),
            args: compiled_args,
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let anf::ImmExpr::Var { id, .. } = func
        && (id.0 == "ref" || id.0 == "ref_get" || id.0 == "ref_set" || id.0 == "ptr_eq")
    {
        let (helper, helper_ty) = if id.0 == "ref" {
            let tast::Ty::TRef { elem } = ty else {
                panic!("ref return type must be reference, got {:?}", ty);
            };
            let elem_go_ty = tast_ty_to_go_type(elem);
            let ref_go_ty = tast_ty_to_go_type(ty);
            (
                runtime::ref_helper_fn_name("ref", ty),
                goty::GoType::TFunc {
                    params: vec![elem_go_ty],
                    ret_ty: Box::new(ref_go_ty),
                },
            )
        } else if id.0 == "ptr_eq" {
            let ref_ty = imm_ty(&args[0]);
            let tast::Ty::TRef { .. } = &ref_ty else {
                panic!("ptr_eq expects reference arguments, got {:?}", ref_ty);
            };
            let ref_go_ty = tast_ty_to_go_type(&ref_ty);
            (
                runtime::ref_helper_fn_name("ptr_eq", &ref_ty),
                goty::GoType::TFunc {
                    params: vec![ref_go_ty.clone(), ref_go_ty.clone()],
                    ret_ty: Box::new(goty::GoType::TBool),
                },
            )
        } else {
            let ref_ty = imm_ty(&args[0]);
            let tast::Ty::TRef { elem } = &ref_ty else {
                panic!("{} expects reference argument, got {:?}", id.0, ref_ty);
            };
            let ref_go_ty = tast_ty_to_go_type(&ref_ty);
            let elem_go_ty = tast_ty_to_go_type(elem);
            let ret_ty = if id.0 == "ref_get" {
                elem_go_ty.clone()
            } else {
                goty::GoType::TUnit
            };
            (
                runtime::ref_helper_fn_name(&id.0, &ref_ty),
                goty::GoType::TFunc {
                    params: if id.0 == "ref_get" {
                        vec![ref_go_ty.clone()]
                    } else {
                        vec![ref_go_ty.clone(), elem_go_ty.clone()]
                    },
                    ret_ty: Box::new(ret_ty),
                },
            )
        };

        return goast::Expr::Call {
            func: Box::new(goast::Expr::Var {
                name: helper,
                ty: helper_ty,
            }),
            args: compiled_args,
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let anf::ImmExpr::Var { id, .. } = func
        && (id.0 == "hashmap_new"
            || id.0 == "hashmap_get"
            || id.0 == "hashmap_set"
            || id.0 == "hashmap_remove"
            || id.0 == "hashmap_len"
            || id.0 == "hashmap_contains")
    {
        let map_ty = if id.0 == "hashmap_new" {
            ty.clone()
        } else {
            imm_ty(&args[0])
        };
        let tast::Ty::THashMap { key, value } = &map_ty else {
            panic!("{} expects HashMap type, got {:?}", id.0, map_ty);
        };

        let map_go_ty = tast_ty_to_go_type(&map_ty);
        let key_go_ty = tast_ty_to_go_type(key);
        let value_go_ty = tast_ty_to_go_type(value);

        let helper = runtime::hashmap_helper_fn_name(&id.0, &map_ty);
        let helper_ty = match id.0.as_str() {
            "hashmap_new" => goty::GoType::TFunc {
                params: vec![],
                ret_ty: Box::new(map_go_ty.clone()),
            },
            "hashmap_get" => goty::GoType::TFunc {
                params: vec![map_go_ty.clone(), key_go_ty.clone()],
                ret_ty: Box::new(tast_ty_to_go_type(ty)),
            },
            "hashmap_set" => goty::GoType::TFunc {
                params: vec![map_go_ty.clone(), key_go_ty.clone(), value_go_ty.clone()],
                ret_ty: Box::new(goty::GoType::TUnit),
            },
            "hashmap_remove" => goty::GoType::TFunc {
                params: vec![map_go_ty.clone(), key_go_ty.clone()],
                ret_ty: Box::new(goty::GoType::TUnit),
            },
            "hashmap_len" => goty::GoType::TFunc {
                params: vec![map_go_ty.clone()],
                ret_ty: Box::new(goty::GoType::TInt32),
            },
            "hashmap_contains" => goty::GoType::TFunc {
                params: vec![map_go_ty.clone(), key_go_ty.clone()],
                ret_ty: Box::new(goty::GoType::TBool),
            },
            _ => unreachable!(),
        };

        return goast::Expr::Call {
            func: Box::new(goast::Expr::Var {
                name: helper,
                ty: helper_ty,
            }),
            args: compiled_args,
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let anf::ImmExpr::Var { id, .. } = func
        && (id.0 == "slice" || id.0 == "slice_get" || id.0 == "slice_len" || id.0 == "slice_sub")
    {
        let arg0_ty = imm_ty(&args[0]);
        return match id.0.as_str() {
            "slice" | "slice_sub" => {
                let mut args_iter = compiled_args.into_iter();
                let array_arg = args_iter.next().unwrap();
                let start_arg = args_iter.next().unwrap();
                let end_arg = args_iter.next().unwrap();
                goast::Expr::Slice {
                    array: Box::new(array_arg),
                    start: Box::new(start_arg),
                    end: Box::new(end_arg),
                    ty: tast_ty_to_go_type(ty),
                }
            }
            "slice_get" => {
                let mut args_iter = compiled_args.into_iter();
                let slice_arg = args_iter.next().unwrap();
                let index_arg = args_iter.next().unwrap();
                goast::Expr::Index {
                    array: Box::new(slice_arg),
                    index: Box::new(index_arg),
                    ty: tast_ty_to_go_type(ty),
                }
            }
            "slice_len" => {
                let mut args_iter = compiled_args.into_iter();
                let slice_arg = args_iter.next().unwrap();
                goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "int32".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TInt32],
                            ret_ty: Box::new(goty::GoType::TInt32),
                        },
                    }),
                    args: vec![goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: "len".to_string(),
                            ty: goty::GoType::TFunc {
                                params: vec![tast_ty_to_go_type(&arg0_ty)],
                                ret_ty: Box::new(goty::GoType::TInt32),
                            },
                        }),
                        args: vec![slice_arg],
                        ty: goty::GoType::TInt32,
                    }],
                    ty: tast_ty_to_go_type(ty),
                }
            }
            _ => unreachable!(),
        };
    }

    if let anf::ImmExpr::Var { id, .. } = func
        && (id.0 == "vec_new" || id.0 == "vec_push" || id.0 == "vec_get" || id.0 == "vec_len")
    {
        return match id.0.as_str() {
            "vec_new" => goast::Expr::Nil {
                ty: tast_ty_to_go_type(ty),
            },
            "vec_push" => goast::Expr::Call {
                func: Box::new(goast::Expr::Var {
                    name: "append".to_string(),
                    ty: func_ty,
                }),
                args: compiled_args,
                ty: tast_ty_to_go_type(ty),
            },
            "vec_get" => {
                let mut args_iter = compiled_args.into_iter();
                let v_arg = args_iter.next().unwrap();
                let index_arg = args_iter.next().unwrap();
                goast::Expr::Index {
                    array: Box::new(v_arg),
                    index: Box::new(index_arg),
                    ty: tast_ty_to_go_type(ty),
                }
            }
            "vec_len" => {
                let arg0_ty = imm_ty(args.first().unwrap());
                let mut args_iter = compiled_args.into_iter();
                let v_arg = args_iter.next().unwrap();
                goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "int32".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TInt32],
                            ret_ty: Box::new(goty::GoType::TInt32),
                        },
                    }),
                    args: vec![goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: "len".to_string(),
                            ty: goty::GoType::TFunc {
                                params: vec![tast_ty_to_go_type(&arg0_ty)],
                                ret_ty: Box::new(goty::GoType::TInt32),
                            },
                        }),
                        args: vec![v_arg],
                        ty: goty::GoType::TInt32,
                    }],
                    ty: tast_ty_to_go_type(ty),
                }
            }
            _ => unreachable!(),
        };
    }

    if let anf::ImmExpr::Var { id, .. } = func
        && let Some(extern_fn) = goenv.genv.value_env.extern_funcs.get(&id.0)
    {
        let alias = go_package_alias(&extern_fn.package_path);
        return goast::Expr::Call {
            func: Box::new(goast::Expr::Var {
                name: format!("{}.{}", alias, extern_fn.go_name),
                ty: func_ty,
            }),
            args: compiled_args,
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let tast::Ty::TStruct { name } = &func_tast_ty
        && is_closure_env_struct(name)
    {
        if let Some(apply) = find_closure_apply_fn(goenv, &func_tast_ty) {
            let apply_expr = goast::Expr::Var {
                name: go_ident(&apply.name),
                ty: tast_ty_to_go_type(&apply.ty),
            };
            let mut call_args = vec![compile_imm(goenv, func)];
            call_args.extend(compiled_args);
            return goast::Expr::Call {
                func: Box::new(apply_expr),
                args: call_args,
                ty: tast_ty_to_go_type(&apply.ret_ty),
            };
        }
    }

    goast::Expr::Call {
        func: Box::new(compile_imm(goenv, func)),
        args: compiled_args,
        ty: tast_ty_to_go_type(ty),
    }
}

fn compile_value_expr(goenv: &GlobalGoEnv, expr: &anf::ValueExpr) -> CompiledValue {
    match expr {
        anf::ValueExpr::Imm(imm) => CompiledValue {
            stmts: Vec::new(),
            expr: compile_imm(goenv, imm),
        },
        anf::ValueExpr::Constr {
            constructor,
            args,
            ty,
        } => match constructor {
            Constructor::Enum(enum_constructor) => {
                let variant_ty = variant_ty_by_index(goenv, ty, enum_constructor.index);
                let variant_field_types = goenv
                    .get_enum(&enum_constructor.type_name)
                    .and_then(|def| def.variants.get(enum_constructor.index))
                    .map(|(_, fields)| fields.as_slice());
                let fields = args
                    .iter()
                    .enumerate()
                    .map(|(i, a)| {
                        let field_ty = variant_field_types.and_then(|f| f.get(i));
                        let arg_ty = imm_ty(a);
                        let val = if let Some(ft) = field_ty
                            && needs_closure_to_func_wrap(&arg_ty, ft)
                        {
                            closure_to_func_lit(goenv, a, ft)
                        } else {
                            compile_imm(goenv, a)
                        };
                        (format!("_{}", i), val)
                    })
                    .collect();
                CompiledValue {
                    stmts: Vec::new(),
                    expr: goast::Expr::StructLiteral {
                        ty: variant_ty,
                        fields,
                    },
                }
            }
            Constructor::Struct(struct_constructor) => {
                let go_ty = tast_ty_to_go_type(ty);
                let struct_def = goenv
                    .get_struct(&struct_constructor.type_name)
                    .unwrap_or_else(|| panic!("unknown struct {}", struct_constructor.type_name.0));
                if struct_def.fields.len() != args.len() {
                    panic!(
                        "struct constructor {} expects {} args, got {}",
                        struct_constructor.type_name.0,
                        struct_def.fields.len(),
                        args.len()
                    );
                }
                let fields = struct_def
                    .fields
                    .iter()
                    .zip(args.iter())
                    .map(|((fname, field_ty), arg)| {
                        let arg_ty = imm_ty(arg);
                        let val = if needs_closure_to_func_wrap(&arg_ty, field_ty) {
                            closure_to_func_lit(goenv, arg, field_ty)
                        } else {
                            compile_imm(goenv, arg)
                        };
                        (go_ident(&fname.0), val)
                    })
                    .collect();
                CompiledValue {
                    stmts: Vec::new(),
                    expr: goast::Expr::StructLiteral { ty: go_ty, fields },
                }
            }
        },
        anf::ValueExpr::Tuple { items, ty } => {
            let fields = items
                .iter()
                .enumerate()
                .map(|(i, a)| (format!("_{}", i), compile_imm(goenv, a)))
                .collect();
            CompiledValue {
                stmts: Vec::new(),
                expr: goast::Expr::StructLiteral {
                    ty: tuple_to_go_struct_type(ty),
                    fields,
                },
            }
        }
        anf::ValueExpr::Array { items, ty } => {
            let elem_ty = match ty {
                tast::Ty::TArray { elem, .. } => elem.as_ref(),
                _ => ty,
            };
            let elems = items
                .iter()
                .map(|item| {
                    let arg_ty = imm_ty(item);
                    if needs_closure_to_func_wrap(&arg_ty, elem_ty) {
                        closure_to_func_lit(goenv, item, elem_ty)
                    } else {
                        compile_imm(goenv, item)
                    }
                })
                .collect();
            CompiledValue {
                stmts: Vec::new(),
                expr: goast::Expr::ArrayLiteral {
                    elems,
                    ty: tast_ty_to_go_type(ty),
                },
            }
        }
        anf::ValueExpr::ConstrGet {
            expr,
            constructor,
            field_index,
            ty: _,
        } => {
            let obj = compile_imm(goenv, expr);
            match constructor {
                Constructor::Enum(enum_constructor) => {
                    let def = goenv
                        .get_enum(&enum_constructor.type_name)
                        .expect("unknown enum in ConstrGet");
                    let field_ty = def.variants[enum_constructor.index].1[*field_index].clone();
                    let scrut_ty = imm_ty(expr);
                    let variant_ty = variant_ty_by_index(goenv, &scrut_ty, enum_constructor.index);
                    let cast = goast::Expr::Cast {
                        expr: Box::new(obj),
                        ty: variant_ty,
                    };
                    CompiledValue {
                        stmts: Vec::new(),
                        expr: goast::Expr::FieldAccess {
                            obj: Box::new(cast),
                            field: format!("_{}", field_index),
                            ty: tast_ty_to_go_type(&field_ty),
                        },
                    }
                }
                Constructor::Struct(struct_constructor) => {
                    let scrut_ty = imm_ty(expr);
                    let (ty_name, type_args) = match scrut_ty {
                        tast::Ty::TStruct { name } => (name, Vec::new()),
                        tast::Ty::TApp { ty, args } => {
                            let base_name = ty.get_constr_name_unsafe();
                            (base_name, args)
                        }
                        other => panic!(
                            "ConstrGet on non-struct type {:?} for constructor {}",
                            other, struct_constructor.type_name.0
                        ),
                    };
                    let struct_name = &struct_constructor.type_name.0;
                    assert_eq!(
                        ty_name, *struct_name,
                        "struct constructor type mismatch: expected {}, got {}",
                        struct_name, ty_name
                    );
                    let fields =
                        instantiate_struct_fields(goenv, &struct_constructor.type_name, &type_args);
                    let (field_name, field_ty) = &fields[*field_index];
                    CompiledValue {
                        stmts: Vec::new(),
                        expr: goast::Expr::FieldAccess {
                            obj: Box::new(obj),
                            field: go_ident(field_name),
                            ty: tast_ty_to_go_type(field_ty),
                        },
                    }
                }
            }
        }
        anf::ValueExpr::Unary { op, expr, ty } => {
            let go_op = match op {
                common_defs::UnaryOp::Neg => goast::GoUnaryOp::Neg,
                common_defs::UnaryOp::Not => goast::GoUnaryOp::Not,
            };
            CompiledValue {
                stmts: Vec::new(),
                expr: goast::Expr::UnaryOp {
                    op: go_op,
                    expr: Box::new(compile_imm(goenv, expr)),
                    ty: tast_ty_to_go_type(ty),
                },
            }
        }
        anf::ValueExpr::Binary { op, lhs, rhs, ty } => {
            let go_op = match op {
                common_defs::BinaryOp::Add => goast::GoBinaryOp::Add,
                common_defs::BinaryOp::Sub => goast::GoBinaryOp::Sub,
                common_defs::BinaryOp::Mul => goast::GoBinaryOp::Mul,
                common_defs::BinaryOp::Div => goast::GoBinaryOp::Div,
                common_defs::BinaryOp::And => goast::GoBinaryOp::And,
                common_defs::BinaryOp::Or => goast::GoBinaryOp::Or,
                common_defs::BinaryOp::Less => goast::GoBinaryOp::Less,
                common_defs::BinaryOp::Greater => goast::GoBinaryOp::Greater,
                common_defs::BinaryOp::LessEq => goast::GoBinaryOp::LessEq,
                common_defs::BinaryOp::GreaterEq => goast::GoBinaryOp::GreaterEq,
                common_defs::BinaryOp::Eq => goast::GoBinaryOp::Eq,
                common_defs::BinaryOp::NotEq => goast::GoBinaryOp::NotEq,
            };
            CompiledValue {
                stmts: Vec::new(),
                expr: goast::Expr::BinaryOp {
                    op: go_op,
                    lhs: Box::new(compile_imm(goenv, lhs)),
                    rhs: Box::new(compile_imm(goenv, rhs)),
                    ty: tast_ty_to_go_type(ty),
                },
            }
        }
        anf::ValueExpr::Call { func, args, ty } => {
            if let anf::ImmExpr::Var { id, .. } = func
                && (id.0 == "print" || id.0 == "println")
            {
                let call_expr = goast::Expr::Call {
                    func: Box::new(compile_imm(goenv, func)),
                    args: args.iter().map(|arg| compile_imm(goenv, arg)).collect(),
                    ty: goty::GoType::TVoid,
                };
                CompiledValue {
                    stmts: vec![goast::Stmt::Expr(call_expr)],
                    expr: goast::Expr::Unit {
                        ty: tast_ty_to_go_type(ty),
                    },
                }
            } else {
                CompiledValue {
                    stmts: Vec::new(),
                    expr: compile_call(goenv, func, args, ty),
                }
            }
        }
        anf::ValueExpr::ToDyn {
            trait_name,
            for_ty,
            expr,
            ty,
        } => {
            let dyn_struct_ty = tast_ty_to_go_type(ty);
            let vtable_struct_name = dyn_vtable_struct_go_name(&trait_name.0);
            let vtable_ptr_ty = goty::GoType::TPointer {
                elem: Box::new(goty::GoType::TName {
                    name: vtable_struct_name,
                }),
            };

            let ctor_name = dyn_vtable_ctor_go_name(&trait_name.0, for_ty);
            let ctor_ty = goty::GoType::TFunc {
                params: vec![],
                ret_ty: Box::new(vtable_ptr_ty.clone()),
            };
            let vtable_expr = goast::Expr::Call {
                func: Box::new(goast::Expr::Var {
                    name: ctor_name,
                    ty: ctor_ty,
                }),
                args: vec![],
                ty: vtable_ptr_ty,
            };

            CompiledValue {
                stmts: Vec::new(),
                expr: goast::Expr::StructLiteral {
                    fields: vec![
                        ("data".to_string(), compile_imm(goenv, expr)),
                        ("vtable".to_string(), vtable_expr),
                    ],
                    ty: dyn_struct_ty,
                },
            }
        }
        anf::ValueExpr::DynCall {
            trait_name,
            method_name,
            receiver,
            args,
            ty,
        } => {
            let receiver_expr_for_vtable = compile_imm(goenv, receiver);
            let receiver_expr_for_data = compile_imm(goenv, receiver);

            let (method_params, method_ret) = trait_method_sigs(goenv, &trait_name.0)
                .into_iter()
                .find(|(name, _, _)| name == &method_name.0)
                .map(|(_name, params, ret)| (params, ret))
                .unwrap_or_else(|| {
                    panic!("missing trait method {}::{}", trait_name.0, method_name.0)
                });

            let mut fn_params = Vec::with_capacity(method_params.len() + 1);
            fn_params.push(any_go_type());
            fn_params.extend(method_params.iter().map(tast_ty_to_go_type));
            let fn_ret = tast_ty_to_go_type(&method_ret);

            let vtable_ptr_expr = goast::Expr::FieldAccess {
                obj: Box::new(receiver_expr_for_vtable),
                field: "vtable".to_string(),
                ty: goty::GoType::TPointer {
                    elem: Box::new(goty::GoType::TName {
                        name: dyn_vtable_struct_go_name(&trait_name.0),
                    }),
                },
            };
            let method_expr = goast::Expr::FieldAccess {
                obj: Box::new(vtable_ptr_expr),
                field: go_ident(&method_name.0),
                ty: goty::GoType::TFunc {
                    params: fn_params,
                    ret_ty: Box::new(fn_ret.clone()),
                },
            };

            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(goast::Expr::FieldAccess {
                obj: Box::new(receiver_expr_for_data),
                field: "data".to_string(),
                ty: any_go_type(),
            });
            call_args.extend(args.iter().map(|arg| compile_imm(goenv, arg)));

            CompiledValue {
                stmts: Vec::new(),
                expr: goast::Expr::Call {
                    func: Box::new(method_expr),
                    args: call_args,
                    ty: tast_ty_to_go_type(ty),
                },
            }
        }
        anf::ValueExpr::Go { closure, ty } => CompiledValue {
            stmts: vec![compile_go(goenv, closure)],
            expr: goast::Expr::Unit {
                ty: tast_ty_to_go_type(ty),
            },
        },
        anf::ValueExpr::Proj { tuple, index, ty } => {
            let obj = compile_imm(goenv, tuple);
            CompiledValue {
                stmts: Vec::new(),
                expr: goast::Expr::FieldAccess {
                    obj: Box::new(obj),
                    field: format!("_{}", index),
                    ty: tast_ty_to_go_type(ty),
                },
            }
        }
    }
}

fn panic_stmt(msg: &str) -> goast::Stmt {
    goast::Stmt::Expr(goast::Expr::Call {
        func: Box::new(goast::Expr::Var {
            name: "panic".to_string(),
            ty: goty::GoType::TFunc {
                params: vec![any_go_type()],
                ret_ty: Box::new(goty::GoType::TVoid),
            },
        }),
        args: vec![goast::Expr::String {
            value: msg.to_string(),
            ty: goty::GoType::TString,
        }],
        ty: goty::GoType::TVoid,
    })
}

fn is_while_loop(join: &anf::JoinBind) -> Option<WhileLoop> {
    if !join.params.is_empty() {
        return None;
    }
    let body = &join.body;
    let anf::Term::If {
        cond,
        then_,
        else_,
        ..
    } = &body.term
    else {
        return None;
    };
    let then_reaches = any_path_reaches(then_, &join.id);
    let else_reaches = any_path_reaches(else_, &join.id);
    let (continue_block, break_block, negate) = if then_reaches && !else_reaches {
        (then_.as_ref(), else_.as_ref(), false)
    } else if else_reaches && !then_reaches {
        (else_.as_ref(), then_.as_ref(), true)
    } else {
        return None;
    };
    let exit_id = match &break_block.term {
        anf::Term::Jump { target, args, .. } if args.is_empty() => Some(target.clone()),
        _ => None,
    };
    Some(WhileLoop {
        cond_binds: body.binds.clone(),
        cond: cond.clone(),
        negate,
        loop_body: continue_block.clone(),
        after: break_block.clone(),
        loop_id: join.id.clone(),
        exit_id,
    })
}

struct WhileLoop {
    cond_binds: Vec<anf::Bind>,
    cond: anf::ImmExpr,
    negate: bool,
    loop_body: anf::Block,
    after: anf::Block,
    loop_id: anf::JoinId,
    exit_id: Option<anf::JoinId>,
}

fn any_path_reaches(block: &anf::Block, target: &anf::JoinId) -> bool {
    any_path_reaches_with_joins(block, target, &HashMap::new(), &mut HashSet::new())
}

fn any_path_reaches_with_joins(
    block: &anf::Block,
    target: &anf::JoinId,
    parent_joins: &HashMap<&anf::JoinId, &anf::JoinBind>,
    visited: &mut HashSet<anf::JoinId>,
) -> bool {
    let mut local_joins = parent_joins.clone();
    for bind in &block.binds {
        match bind {
            anf::Bind::Join(jb) => {
                local_joins.insert(&jb.id, jb);
            }
            anf::Bind::JoinRec(group) => {
                for jb in group {
                    local_joins.insert(&jb.id, jb);
                }
            }
            anf::Bind::Let(_) => {}
        }
    }
    any_path_reaches_term(&block.term, target, &local_joins, visited)
}

fn any_path_reaches_term(
    term: &anf::Term,
    target: &anf::JoinId,
    joins: &HashMap<&anf::JoinId, &anf::JoinBind>,
    visited: &mut HashSet<anf::JoinId>,
) -> bool {
    match term {
        anf::Term::Jump {
            target: t, args, ..
        } => {
            if t == target && args.is_empty() {
                return true;
            }
            if visited.contains(t) {
                return false;
            }
            if let Some(jb) = joins.get(t) {
                visited.insert(t.clone());
                any_path_reaches_with_joins(&jb.body, target, joins, visited)
            } else {
                false
            }
        }
        anf::Term::If { then_, else_, .. } => {
            any_path_reaches_with_joins(then_, target, joins, visited)
                || any_path_reaches_with_joins(else_, target, joins, visited)
        }
        anf::Term::Match { arms, default, .. } => {
            arms.iter()
                .any(|arm| any_path_reaches_with_joins(&arm.body, target, joins, visited))
                || default
                    .as_deref()
                    .is_some_and(|d| any_path_reaches_with_joins(d, target, joins, visited))
        }
        _ => false,
    }
}

fn compile_let_bind(goenv: &GlobalGoEnv, bind: &anf::LetBind) -> Vec<goast::Stmt> {
    let compiled = compile_value_expr(goenv, &bind.value);
    let mut stmts = compiled.stmts;
    if bind.id.0 == "_" {
        if matches!(compiled.expr, goast::Expr::Call { .. }) {
            stmts.push(goast::Stmt::Expr(compiled.expr));
        }
    } else {
        let expr_go_ty = compiled.expr.get_ty();
        let var_ty = if let goty::GoType::TName { name } = expr_go_ty
            && is_closure_env_struct(name)
        {
            expr_go_ty.clone()
        } else {
            tast_ty_to_go_type(&bind.ty)
        };
        stmts.push(goast::Stmt::VarDecl {
            name: go_ident(&bind.id.0),
            ty: var_ty,
            value: Some(compiled.expr),
        });
    }
    stmts
}

fn needs_closure_to_func_wrap(arg_ty: &tast::Ty, param_ty: &tast::Ty) -> bool {
    if let tast::Ty::TStruct { name } = arg_ty
        && is_closure_env_struct(name)
        && matches!(param_ty, tast::Ty::TFunc { .. })
    {
        return true;
    }
    false
}

fn closure_to_func_lit(
    goenv: &GlobalGoEnv,
    closure_imm: &anf::ImmExpr,
    func_ty: &tast::Ty,
) -> goast::Expr {
    let closure_ty = imm_ty(closure_imm);
    let apply = find_closure_apply_fn(goenv, &closure_ty)
        .expect("closure struct must have an apply method");

    let tast::Ty::TFunc { params: func_params, ret_ty: func_ret } = func_ty else {
        unreachable!("closure_to_func_lit called with non-function target type");
    };

    let param_names: Vec<String> = func_params
        .iter()
        .enumerate()
        .map(|(i, _)| format!("p{}", i))
        .collect();

    let go_params: Vec<(String, goty::GoType)> = param_names
        .iter()
        .zip(func_params.iter())
        .map(|(name, ty)| (name.clone(), tast_ty_to_go_type(ty)))
        .collect();

    let mut call_args: Vec<goast::Expr> = vec![compile_imm(goenv, closure_imm)];
    for (name, ty) in &go_params {
        call_args.push(goast::Expr::Var {
            name: name.clone(),
            ty: ty.clone(),
        });
    }

    let apply_expr = goast::Expr::Var {
        name: go_ident(&apply.name),
        ty: tast_ty_to_go_type(&apply.ty),
    };

    let go_ret_ty = tast_ty_to_go_type(func_ret);
    let call_expr = goast::Expr::Call {
        func: Box::new(apply_expr),
        args: call_args,
        ty: go_ret_ty.clone(),
    };

    let body = vec![goast::Stmt::Return {
        expr: Some(call_expr),
    }];

    let func_go_ty = tast_ty_to_go_type(func_ty);

    goast::Expr::FuncLit {
        params: go_params,
        body,
        ty: func_go_ty,
    }
}

fn compile_jump_args(
    goenv: &GlobalGoEnv,
    params: &[(anf::LocalId, tast::Ty)],
    args: &[anf::ImmExpr],
) -> Vec<goast::Stmt> {
    let mut stmts = Vec::new();
    for ((param_id, param_ty), arg) in params.iter().zip(args.iter()) {
        if param_id.0 == "_" {
            continue;
        }
        let arg_ty = imm_ty(arg);
        let value = if needs_closure_to_func_wrap(&arg_ty, param_ty) {
            closure_to_func_lit(goenv, arg, param_ty)
        } else {
            compile_imm(goenv, arg)
        };
        stmts.push(goast::Stmt::Assignment {
            name: go_ident(&param_id.0),
            value,
        });
    }
    stmts
}

fn all_branches_jump_to(term: &anf::Term) -> Option<&anf::JoinId> {
    match term {
        anf::Term::Jump { target, .. } => Some(target),
        anf::Term::If { then_, else_, .. } => {
            let t = block_tail_target(then_)?;
            let e = block_tail_target(else_)?;
            if t == e { Some(t) } else { None }
        }
        anf::Term::Match {
            arms, default, ..
        } => {
            let mut result: Option<&anf::JoinId> = None;
            for arm in arms {
                let t = block_tail_target(&arm.body)?;
                if let Some(prev) = result {
                    if prev != t {
                        return None;
                    }
                } else {
                    result = Some(t);
                }
            }
            if let Some(def) = default {
                let t = block_tail_target(def)?;
                if let Some(prev) = result {
                    if prev != t {
                        return None;
                    }
                } else {
                    result = Some(t);
                }
            }
            result
        }
        _ => None,
    }
}

fn block_tail_target(block: &anf::Block) -> Option<&anf::JoinId> {
    all_branches_jump_to(&block.term)
}

fn compile_block_structured(
    goenv: &GlobalGoEnv,
    block: &anf::Block,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    let mut stmts = Vec::new();
    let mut pending_joins: Vec<&anf::JoinBind> = Vec::new();
    let mut while_loop_ids: Vec<anf::JoinId> = Vec::new();
    for bind in &block.binds {
        match bind {
            anf::Bind::Let(let_bind) => {
                stmts.extend(compile_let_bind(goenv, let_bind));
            }
            anf::Bind::Join(join_bind) => {
                for (id, ty) in &join_bind.params {
                    if id.0 != "_" {
                        stmts.push(goast::Stmt::VarDecl {
                            name: go_ident(&id.0),
                            ty: tast_ty_to_go_type(ty),
                            value: None,
                        });
                    }
                }
                pending_joins.push(join_bind);
            }
            anf::Bind::JoinRec(group) => {
                for j in group {
                    if is_while_loop(j).is_some() {
                        while_loop_ids.push(j.id.clone());
                    }
                }
                stmts.extend(compile_joinrec(goenv, group, join_env));
            }
        }
    }

    if let anf::Term::Jump { target, args, .. } = &block.term
        && args.is_empty()
        && while_loop_ids.contains(target)
    {
        return stmts;
    }

    stmts.extend(compile_term_with_continuations(
        goenv,
        &block.term,
        join_env,
        &pending_joins,
    ));
    stmts
}

fn compile_term_with_continuations(
    goenv: &GlobalGoEnv,
    term: &anf::Term,
    join_env: &JoinEnv,
    pending_joins: &[&anf::JoinBind],
) -> Vec<goast::Stmt> {
    if let Some(target) = all_branches_jump_to(term)
        && let Some(join_bind) = join_env.get(target)
        && pending_joins.iter().any(|j| j.id == *target)
    {
        let mut stmts = compile_term_jump_to(goenv, term, target, join_bind, join_env);
        stmts.extend(compile_block_structured(goenv, &join_bind.body, join_env));
        return stmts;
    }
    compile_term_leaf(goenv, term, join_env)
}

fn compile_term_jump_to(
    goenv: &GlobalGoEnv,
    term: &anf::Term,
    target: &anf::JoinId,
    join_bind: &anf::JoinBind,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    match term {
        anf::Term::Jump { args, .. } => compile_jump_args(goenv, &join_bind.params, args),
        anf::Term::If {
            cond,
            then_,
            else_,
            ..
        } => {
            let then_stmts = compile_branch_to_join(goenv, then_, target, join_bind, join_env);
            let else_stmts = compile_branch_to_join(goenv, else_, target, join_bind, join_env);
            vec![goast::Stmt::If {
                cond: compile_imm(goenv, cond),
                then: goast::Block { stmts: then_stmts },
                else_: Some(goast::Block { stmts: else_stmts }),
            }]
        }
        anf::Term::Match {
            scrut,
            arms,
            default,
            ..
        } => compile_match_to_join(goenv, scrut, arms, default.as_deref(), target, join_bind, join_env),
        _ => compile_term_leaf(goenv, term, join_env),
    }
}

fn compile_branch_to_join(
    goenv: &GlobalGoEnv,
    block: &anf::Block,
    target: &anf::JoinId,
    join_bind: &anf::JoinBind,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    let mut stmts = Vec::new();
    let mut inner_pending: Vec<&anf::JoinBind> = Vec::new();
    for bind in &block.binds {
        match bind {
            anf::Bind::Let(let_bind) => {
                stmts.extend(compile_let_bind(goenv, let_bind));
            }
            anf::Bind::Join(jb) => {
                for (id, ty) in &jb.params {
                    if id.0 != "_" {
                        stmts.push(goast::Stmt::VarDecl {
                            name: go_ident(&id.0),
                            ty: tast_ty_to_go_type(ty),
                            value: None,
                        });
                    }
                }
                inner_pending.push(jb);
            }
            anf::Bind::JoinRec(group) => {
                stmts.extend(compile_joinrec(goenv, group, join_env));
            }
        }
    }

    if let Some(inner_target) = all_branches_jump_to(&block.term) {
        if inner_target == target {
            stmts.extend(compile_term_jump_to(goenv, &block.term, target, join_bind, join_env));
            return stmts;
        }
        if let Some(inner_join) = join_env.get(inner_target)
            && inner_pending.iter().any(|j| j.id == *inner_target)
        {
            stmts.extend(compile_term_jump_to(
                goenv,
                &block.term,
                inner_target,
                inner_join,
                join_env,
            ));
            stmts.extend(compile_branch_to_join(
                goenv,
                &inner_join.body,
                target,
                join_bind,
                join_env,
            ));
            return stmts;
        }
    }
    stmts.extend(compile_term_with_continuations(goenv, &block.term, join_env, &inner_pending));
    stmts
}

fn compile_match_to_join(
    goenv: &GlobalGoEnv,
    scrut: &anf::ImmExpr,
    arms: &[anf::Arm],
    default: Option<&anf::Block>,
    target: &anf::JoinId,
    join_bind: &anf::JoinBind,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    let scrut_ty = imm_ty(scrut);

    let mut var_arm: Option<(&anf::LocalId, &anf::Block)> = None;
    let mut literal_arms = Vec::new();
    for arm in arms {
        match &arm.lhs {
            anf::ImmExpr::Var { id, .. } => {
                var_arm = Some((id, &arm.body));
            }
            _ => literal_arms.push((&arm.lhs, &arm.body)),
        }
    }

    if scrut_ty == tast::Ty::TUnit {
        let block = literal_arms
            .first()
            .map(|(_, b)| *b)
            .or(var_arm.map(|(_, b)| b))
            .or(default);
        if let Some(block) = block {
            return compile_branch_to_join(goenv, block, target, join_bind, join_env);
        }
        return vec![panic_stmt("non-exhaustive match")];
    }

    match &scrut_ty {
        tast::Ty::TEnum { .. } => compile_enum_match_to_join(
            goenv, scrut, &literal_arms, var_arm, default, target, join_bind, join_env,
        ),
        tast::Ty::TApp { ty, .. } if matches!(ty.as_ref(), tast::Ty::TEnum { .. }) => {
            compile_enum_match_to_join(
                goenv, scrut, &literal_arms, var_arm, default, target, join_bind, join_env,
            )
        }
        _ => compile_switch_match_to_join(
            goenv, scrut, &literal_arms, var_arm, default, target, join_bind, join_env,
        ),
    }
}

#[allow(clippy::too_many_arguments)]
fn compile_enum_match_to_join(
    goenv: &GlobalGoEnv,
    scrut: &anf::ImmExpr,
    arms: &[(&anf::ImmExpr, &anf::Block)],
    var_default: Option<(&anf::LocalId, &anf::Block)>,
    default: Option<&anf::Block>,
    target: &anf::JoinId,
    join_bind: &anf::JoinBind,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    let mut cases = Vec::new();
    for (pat, body) in arms {
        let anf::ImmExpr::Tag { index, ty } = pat else {
            panic!("expected tag pattern in enum match, got {:?}", pat);
        };
        let vty = variant_ty_by_index(goenv, ty, *index);
        let stmts = compile_branch_to_join(goenv, body, target, join_bind, join_env);
        cases.push((vty, goast::Block { stmts }));
    }

    let default_block = if let Some((id, body)) = var_default {
        let mut stmts = Vec::new();
        if id.0 != "_" {
            stmts.push(goast::Stmt::VarDecl {
                name: go_ident(&id.0),
                ty: tast_ty_to_go_type(&imm_ty(scrut)),
                value: Some(compile_imm(goenv, scrut)),
            });
        }
        stmts.extend(compile_branch_to_join(goenv, body, target, join_bind, join_env));
        Some(goast::Block { stmts })
    } else if let Some(def_block) = default {
        Some(goast::Block {
            stmts: compile_branch_to_join(goenv, def_block, target, join_bind, join_env),
        })
    } else {
        Some(goast::Block {
            stmts: vec![panic_stmt("non-exhaustive match")],
        })
    };

    vec![goast::Stmt::SwitchType {
        bind: None,
        expr: compile_imm(goenv, scrut),
        cases,
        default: default_block,
    }]
}

#[allow(clippy::too_many_arguments)]
fn compile_switch_match_to_join(
    goenv: &GlobalGoEnv,
    scrut: &anf::ImmExpr,
    arms: &[(&anf::ImmExpr, &anf::Block)],
    var_default: Option<(&anf::LocalId, &anf::Block)>,
    default: Option<&anf::Block>,
    target: &anf::JoinId,
    join_bind: &anf::JoinBind,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    let mut cases = Vec::new();
    for (pat, body) in arms {
        let stmts = compile_branch_to_join(goenv, body, target, join_bind, join_env);
        cases.push((compile_imm(goenv, pat), goast::Block { stmts }));
    }

    let default_block = if let Some((id, body)) = var_default {
        let mut stmts = Vec::new();
        if id.0 != "_" {
            stmts.push(goast::Stmt::VarDecl {
                name: go_ident(&id.0),
                ty: tast_ty_to_go_type(&imm_ty(scrut)),
                value: Some(compile_imm(goenv, scrut)),
            });
        }
        stmts.extend(compile_branch_to_join(goenv, body, target, join_bind, join_env));
        Some(goast::Block { stmts })
    } else if let Some(def_block) = default {
        Some(goast::Block {
            stmts: compile_branch_to_join(goenv, def_block, target, join_bind, join_env),
        })
    } else {
        Some(goast::Block {
            stmts: vec![panic_stmt("non-exhaustive match")],
        })
    };

    vec![goast::Stmt::SwitchExpr {
        expr: compile_imm(goenv, scrut),
        cases,
        default: default_block,
    }]
}

fn compile_term_leaf(
    goenv: &GlobalGoEnv,
    term: &anf::Term,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    match term {
        anf::Term::Return(imm) => vec![goast::Stmt::Return {
            expr: Some(compile_imm(goenv, imm)),
        }],
        anf::Term::Jump { target, args, .. } => {
            if join_env.continue_targets.contains(target) {
                return vec![goast::Stmt::Continue];
            }
            if join_env.break_targets.contains(target) {
                return vec![goast::Stmt::Break];
            }
            if let Some(join_bind) = join_env.get(target) {
                let mut stmts = compile_jump_args(goenv, &join_bind.params, args);
                stmts.extend(compile_block_structured(goenv, &join_bind.body, join_env));
                stmts
            } else {
                vec![panic_stmt(&format!("unknown join {:?}", target))]
            }
        }
        anf::Term::If {
            cond,
            then_,
            else_,
            ..
        } => {
            let then_stmts = compile_block_structured(goenv, then_, join_env);
            let else_stmts = compile_block_structured(goenv, else_, join_env);
            vec![goast::Stmt::If {
                cond: compile_imm(goenv, cond),
                then: goast::Block { stmts: then_stmts },
                else_: Some(goast::Block { stmts: else_stmts }),
            }]
        }
        anf::Term::Match {
            scrut,
            arms,
            default,
            ..
        } => compile_match_leaf(goenv, scrut, arms, default.as_deref(), join_env),
        anf::Term::Unreachable { .. } => vec![panic_stmt("unreachable")],
    }
}

fn compile_match_leaf(
    goenv: &GlobalGoEnv,
    scrut: &anf::ImmExpr,
    arms: &[anf::Arm],
    default: Option<&anf::Block>,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    let scrut_ty = imm_ty(scrut);

    let mut var_arm: Option<(&anf::LocalId, &anf::Block)> = None;
    let mut literal_arms = Vec::new();
    for arm in arms {
        match &arm.lhs {
            anf::ImmExpr::Var { id, .. } => {
                var_arm = Some((id, &arm.body));
            }
            _ => literal_arms.push((&arm.lhs, &arm.body)),
        }
    }

    if scrut_ty == tast::Ty::TUnit {
        let block = literal_arms
            .first()
            .map(|(_, b)| *b)
            .or(var_arm.map(|(_, b)| b))
            .or(default);
        if let Some(block) = block {
            return compile_block_structured(goenv, block, join_env);
        }
        return vec![panic_stmt("non-exhaustive match")];
    }

    match &scrut_ty {
        tast::Ty::TEnum { .. } => compile_enum_match_leaf(
            goenv, scrut, &literal_arms, var_arm, default, join_env,
        ),
        tast::Ty::TApp { ty, .. } if matches!(ty.as_ref(), tast::Ty::TEnum { .. }) => {
            compile_enum_match_leaf(
                goenv, scrut, &literal_arms, var_arm, default, join_env,
            )
        }
        _ => compile_switch_match_leaf(
            goenv, scrut, &literal_arms, var_arm, default, join_env,
        ),
    }
}

fn compile_enum_match_leaf(
    goenv: &GlobalGoEnv,
    scrut: &anf::ImmExpr,
    arms: &[(&anf::ImmExpr, &anf::Block)],
    var_default: Option<(&anf::LocalId, &anf::Block)>,
    default: Option<&anf::Block>,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    let mut cases = Vec::new();
    for (pat, body) in arms {
        let anf::ImmExpr::Tag { index, ty } = pat else {
            panic!("expected tag pattern in enum match, got {:?}", pat);
        };
        let vty = variant_ty_by_index(goenv, ty, *index);
        let stmts = compile_block_structured(goenv, body, join_env);
        cases.push((vty, goast::Block { stmts }));
    }

    let default_block = if let Some((id, body)) = var_default {
        let mut stmts = Vec::new();
        if id.0 != "_" {
            stmts.push(goast::Stmt::VarDecl {
                name: go_ident(&id.0),
                ty: tast_ty_to_go_type(&imm_ty(scrut)),
                value: Some(compile_imm(goenv, scrut)),
            });
        }
        stmts.extend(compile_block_structured(goenv, body, join_env));
        Some(goast::Block { stmts })
    } else if let Some(def_block) = default {
        Some(goast::Block {
            stmts: compile_block_structured(goenv, def_block, join_env),
        })
    } else {
        Some(goast::Block {
            stmts: vec![panic_stmt("non-exhaustive match")],
        })
    };

    vec![goast::Stmt::SwitchType {
        bind: None,
        expr: compile_imm(goenv, scrut),
        cases,
        default: default_block,
    }]
}

fn compile_switch_match_leaf(
    goenv: &GlobalGoEnv,
    scrut: &anf::ImmExpr,
    arms: &[(&anf::ImmExpr, &anf::Block)],
    var_default: Option<(&anf::LocalId, &anf::Block)>,
    default: Option<&anf::Block>,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    let mut cases = Vec::new();
    for (pat, body) in arms {
        let stmts = compile_block_structured(goenv, body, join_env);
        cases.push((compile_imm(goenv, pat), goast::Block { stmts }));
    }

    let default_block = if let Some((id, body)) = var_default {
        let mut stmts = Vec::new();
        if id.0 != "_" {
            stmts.push(goast::Stmt::VarDecl {
                name: go_ident(&id.0),
                ty: tast_ty_to_go_type(&imm_ty(scrut)),
                value: Some(compile_imm(goenv, scrut)),
            });
        }
        stmts.extend(compile_block_structured(goenv, body, join_env));
        Some(goast::Block { stmts })
    } else if let Some(def_block) = default {
        Some(goast::Block {
            stmts: compile_block_structured(goenv, def_block, join_env),
        })
    } else {
        Some(goast::Block {
            stmts: vec![panic_stmt("non-exhaustive match")],
        })
    };

    vec![goast::Stmt::SwitchExpr {
        expr: compile_imm(goenv, scrut),
        cases,
        default: default_block,
    }]
}

fn compile_joinrec(
    goenv: &GlobalGoEnv,
    group: &[anf::JoinBind],
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    if group.len() == 1
        && let Some(wl) = is_while_loop(&group[0])
    {
        return compile_while_loop(goenv, &wl, join_env);
    }
    Vec::new()
}

fn compile_while_loop(
    goenv: &GlobalGoEnv,
    wl: &WhileLoop,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    let mut loop_body_stmts = Vec::new();
    for bind in &wl.cond_binds {
        if let anf::Bind::Let(let_bind) = bind {
            loop_body_stmts.extend(compile_let_bind(goenv, let_bind));
        }
    }

    let cond_expr = compile_imm(goenv, &wl.cond);
    let break_cond = if wl.negate {
        cond_expr
    } else {
        goast::Expr::UnaryOp {
            op: goast::GoUnaryOp::Not,
            expr: Box::new(cond_expr),
            ty: goty::GoType::TBool,
        }
    };
    loop_body_stmts.push(goast::Stmt::If {
        cond: break_cond,
        then: goast::Block {
            stmts: vec![goast::Stmt::Break],
        },
        else_: None,
    });

    let mut inner_env = join_env.clone();
    inner_env.continue_targets.insert(wl.loop_id.clone());
    if let Some(exit_id) = &wl.exit_id {
        inner_env.break_targets.insert(exit_id.clone());
    }
    loop_body_stmts.extend(compile_block_structured(goenv, &wl.loop_body, &inner_env));

    let mut stmts = vec![goast::Stmt::Loop {
        body: goast::Block {
            stmts: loop_body_stmts,
        },
    }];

    stmts.extend(compile_block_structured(goenv, &wl.after, join_env));
    stmts
}

fn build_join_env(block: &anf::Block) -> JoinEnv {
    let mut env = JoinEnv::default();
    collect_joins_from_block(block, &mut env);
    env
}

#[derive(Default, Clone)]
struct JoinEnv {
    joins: HashMap<anf::JoinId, anf::JoinBind>,
    continue_targets: HashSet<anf::JoinId>,
    break_targets: HashSet<anf::JoinId>,
}

impl JoinEnv {
    fn get(&self, id: &anf::JoinId) -> Option<&anf::JoinBind> {
        self.joins.get(id)
    }
}

fn collect_joins_from_block(block: &anf::Block, env: &mut JoinEnv) {
    for bind in &block.binds {
        match bind {
            anf::Bind::Join(join_bind) => {
                env.joins.insert(join_bind.id.clone(), join_bind.clone());
                collect_joins_from_block(&join_bind.body, env);
            }
            anf::Bind::JoinRec(group) => {
                for join_bind in group {
                    if is_while_loop(join_bind).is_none() {
                        env.joins.insert(join_bind.id.clone(), join_bind.clone());
                    }
                    collect_joins_from_block(&join_bind.body, env);
                }
            }
            anf::Bind::Let(_) => {}
        }
    }
    match &block.term {
        anf::Term::If { then_, else_, .. } => {
            collect_joins_from_block(then_, env);
            collect_joins_from_block(else_, env);
        }
        anf::Term::Match { arms, default, .. } => {
            for arm in arms {
                collect_joins_from_block(&arm.body, env);
            }
            if let Some(def) = default {
                collect_joins_from_block(def, env);
            }
        }
        _ => {}
    }
}

fn compile_fn(goenv: &GlobalGoEnv, _gensym: &Gensym, f: anf::Fn) -> goast::Fn {
    let params = f
        .params
        .into_iter()
        .map(|(id, ty)| (go_ident(&id.0), tast_ty_to_go_type(&ty)))
        .collect::<Vec<_>>();

    let go_ret_ty = tast_ty_to_go_type(&f.ret_ty);
    let ret_ty = match go_ret_ty {
        goty::GoType::TVoid => None,
        _ => Some(go_ret_ty.clone()),
    };

    let is_entry = f.name == ENTRY_FUNCTION || f.name.ends_with(&format!("::{}", ENTRY_FUNCTION));
    let patched_name = if is_entry {
        ENTRY_WRAPPER_FUNCTION.to_string()
    } else {
        go_ident(&f.name)
    };

    let join_env = build_join_env(&f.body);
    let stmts = compile_block_structured(goenv, &f.body, &join_env);

    goast::Fn {
        name: patched_name,
        params,
        ret_ty,
        body: goast::Block { stmts },
    }
}

pub fn go_file(
    anfenv: GlobalAnfEnv,
    gensym: &Gensym,
    file: anf::File,
) -> (goast::File, GlobalGoEnv) {
    let goenv = GlobalGoEnv::from_anf_env(anfenv);
    let mut all = Vec::new();

    let (tuple_types, array_types, ref_types, hashmap_types, missing_types) =
        collect_runtime_types(&file);

    all.extend(runtime::make_runtime());
    all.extend(runtime::make_array_runtime(&array_types));
    all.extend(runtime::make_ref_runtime(&ref_types));
    all.extend(runtime::make_hashmap_runtime(&goenv, &hashmap_types));
    all.extend(runtime::make_missing_runtime(&missing_types));

    if !goenv.genv.value_env.extern_funcs.is_empty() || !goenv.genv.type_env.extern_types.is_empty()
    {
        let mut existing_imports: IndexSet<String> = IndexSet::new();
        for item in &all {
            if let goast::Item::Import(import_decl) = item {
                for spec in &import_decl.specs {
                    existing_imports.insert(spec.path.clone());
                }
            }
        }

        let mut extra_specs = Vec::new();
        for extern_fn in goenv.genv.value_env.extern_funcs.values() {
            if existing_imports.insert(extern_fn.package_path.clone()) {
                extra_specs.push(goast::ImportSpec {
                    alias: None,
                    path: extern_fn.package_path.clone(),
                });
            }
        }
        for extern_ty in goenv.genv.type_env.extern_types.values() {
            if let Some(package_path) = &extern_ty.package_path
                && existing_imports.insert(package_path.clone())
            {
                extra_specs.push(goast::ImportSpec {
                    alias: None,
                    path: package_path.clone(),
                });
            }
        }

        if !extra_specs.is_empty() {
            if let Some(import_decl) = all.iter_mut().find_map(|item| {
                if let goast::Item::Import(import_decl) = item {
                    Some(import_decl)
                } else {
                    None
                }
            }) {
                import_decl.specs.extend(extra_specs);
            } else {
                let insert_pos = if matches!(all.first(), Some(goast::Item::Package(_))) {
                    1
                } else {
                    0
                };
                all.insert(
                    insert_pos,
                    goast::Item::Import(goast::ImportDecl { specs: extra_specs }),
                );
            }
        }
    }

    for ty in tuple_types.iter() {
        match tuple_to_go_struct_type(ty) {
            goty::GoType::TStruct { name, fields } => {
                let fields = fields
                    .into_iter()
                    .map(|(field_name, field_ty)| goast::Field {
                        name: field_name,
                        ty: field_ty,
                    })
                    .collect();
                all.push(goast::Item::Struct(goast::Struct {
                    name,
                    fields,
                    methods: vec![],
                }));
            }
            other => panic!("expected struct type, got {:?}", other),
        }
    }

    let file = anf::anf_renamer::rename(file);
    let dyn_req = collect_dyn_requirements(&file);

    let mut toplevels = gen_type_definition(&goenv);
    toplevels.extend(gen_dyn_type_definitions(&goenv, &dyn_req));
    let impl_name_map = build_trait_impl_name_map(&file.toplevels);
    toplevels.extend(gen_dyn_helper_fns(&goenv, &dyn_req, &impl_name_map));
    for item in file.toplevels {
        let gof = compile_fn(&goenv, gensym, item);
        toplevels.push(goast::Item::Fn(gof));
    }
    all.extend(toplevels);
    all.push(goast::Item::Fn(goast::Fn {
        name: ENTRY_FUNCTION.to_string(),
        params: vec![],
        ret_ty: None,
        body: goast::Block {
            stmts: vec![goast::Stmt::Expr(goast::Expr::Call {
                func: Box::new(goast::Expr::Var {
                    name: ENTRY_WRAPPER_FUNCTION.to_string(),
                    ty: goty::GoType::TFunc {
                        params: vec![],
                        ret_ty: Box::new(goty::GoType::TVoid),
                    },
                }),
                args: vec![],
                ty: goty::GoType::TVoid,
            })],
        },
    }));
    // Run a simple DCE pass to drop unused local variables for Go
    let file = goast::File { toplevels: all };
    (crate::go::dce::eliminate_dead_vars(file), goenv)
}

fn gen_type_definition(goenv: &GlobalGoEnv) -> Vec<goast::Item> {
    let mut defs = Vec::new();
    for (name, def) in goenv.structs() {
        let has_type_param = name.0.contains("TParam")
            || !def.generics.is_empty()
            || def
                .fields
                .iter()
                .any(|(_, ty)| matches!(ty, tast::Ty::TParam { .. }));
        if has_type_param {
            continue;
        }

        let fields = def
            .fields
            .iter()
            .map(|(fname, fty)| goast::Field {
                name: go_ident(&fname.0),
                ty: tast_ty_to_go_type(fty),
            })
            .collect();
        defs.push(goast::Item::Struct(goast::Struct {
            name: go_ident(&name.0),
            fields,
            methods: vec![],
        }));
    }

    for (name, def) in goenv.enums() {
        // Skip generating Go types for generic-specialized enums whose fields still contain type parameters
        let has_type_param = name.0.contains("TParam")
            || def
                .variants
                .iter()
                .any(|(_, fields)| fields.iter().any(|f| matches!(f, tast::Ty::TParam { .. })));
        if has_type_param {
            continue;
        }
        let type_identifier_method = format!("is{}", go_ident(&name.0));

        defs.push(goast::Item::Interface(goast::Interface {
            name: go_ident(&name.0),
            methods: vec![goast::MethodElem {
                name: type_identifier_method.clone(),
                params: vec![],
                ret: None,
            }],
        }));
        for (variant_name, variant_fields) in def.variants.iter() {
            let variant_name = variant_struct_name(goenv, &name.0, &variant_name.0);
            let mut fields = Vec::new();
            for (i, field) in variant_fields.iter().enumerate() {
                fields.push(goast::Field {
                    name: format!("_{}", i),
                    ty: tast_ty_to_go_type(field),
                });
            }

            let methods = vec![goast::Method {
                receiver: goast::Receiver {
                    name: "_".to_string(),
                    ty: goty::GoType::TName {
                        name: variant_name.clone(),
                    },
                },
                name: type_identifier_method.clone(),
                params: vec![],
                body: goast::Block { stmts: vec![] },
            }];

            defs.push(goast::Item::Struct(goast::Struct {
                name: variant_name,
                fields,
                methods,
            }));
        }
    }

    for (name, ext) in goenv.genv.type_env.extern_types.iter() {
        if let Some(package_path) = &ext.package_path {
            let alias = go_package_alias(package_path);
            let go_ty = goty::GoType::TName {
                name: format!("{}.{}", alias, ext.go_name),
            };
            defs.push(goast::Item::TypeAlias(goast::TypeAlias {
                name: go_ident(name),
                ty: go_ty,
            }));
        }
    }
    defs
}
