use crate::{
    anf::{self, AExpr, GlobalAnfEnv},
    common::{Constructor, Prim},
    env::{EnumDef, Gensym, GlobalTypeEnv, InherentImplKey, StructDef},
    go::goast::{self, go_type_name_for, tast_ty_to_go_type},
    go::mangle::{encode_ty, go_ident},
    lift::{GlobalLiftEnv, is_closure_env_struct},
    names::{inherent_method_fn_name, trait_impl_fn_name},
    tast::{self, TastIdent},
};

use indexmap::IndexSet;
use std::collections::HashMap;

use super::goty;
use super::runtime;

#[derive(Debug, Clone)]
pub struct GlobalGoEnv {
    pub genv: GlobalTypeEnv,
    pub liftenv: GlobalLiftEnv,
}

impl Default for GlobalGoEnv {
    fn default() -> Self {
        let genv = crate::env::GlobalTypeEnv::new();
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
        anf::ImmExpr::ImmVar { name, ty: _ } => goast::Expr::Var {
            name: go_ident(name),
            ty: tast_ty_to_go_type(&imm_ty(imm)),
        },
        anf::ImmExpr::ImmPrim { value, .. } => {
            let ty = imm_ty(imm);
            go_literal_from_primitive(value, &ty)
        }
        anf::ImmExpr::ImmTag { index, ty } => goast::Expr::StructLiteral {
            fields: vec![],
            ty: variant_ty_by_index(goenv, ty, *index),
        },
    }
}

fn imm_ty(imm: &anf::ImmExpr) -> tast::Ty {
    match imm {
        anf::ImmExpr::ImmVar { ty, .. }
        | anf::ImmExpr::ImmPrim { ty, .. }
        | anf::ImmExpr::ImmTag { ty, .. } => ty.clone(),
    }
}

fn cexpr_ty(goenv: &GlobalGoEnv, e: &anf::CExpr) -> goty::GoType {
    let t = match e {
        anf::CExpr::CImm { imm } => imm_ty(imm),
        anf::CExpr::EConstr { ty, .. }
        | anf::CExpr::ETuple { ty, .. }
        | anf::CExpr::EArray { ty, .. }
        | anf::CExpr::EMatch { ty, .. }
        | anf::CExpr::EIf { ty, .. }
        | anf::CExpr::EWhile { ty, .. }
        | anf::CExpr::EUnary { ty, .. }
        | anf::CExpr::EBinary { ty, .. }
        | anf::CExpr::ECall { ty, .. }
        | anf::CExpr::EToDyn { ty, .. }
        | anf::CExpr::EDynCall { ty, .. }
        | anf::CExpr::EGo { ty, .. }
        | anf::CExpr::EProj { ty, .. } => ty.clone(),
        // For EConstrGet, compute field type from the scrutinee's data constructor
        anf::CExpr::EConstrGet {
            expr,
            constructor,
            field_index,
            ty: _,
        } => match constructor {
            Constructor::Enum(enum_constructor) => {
                let def = goenv
                    .get_enum(&enum_constructor.type_name)
                    .expect("unknown enum in EConstrGet");
                def.variants[enum_constructor.index].1[*field_index].clone()
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
                let fields =
                    instantiate_struct_fields(goenv, &struct_constructor.type_name, &type_args);
                fields[*field_index].1.clone()
            }
        },
    };
    tast_ty_to_go_type(&t)
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
        | tast::Ty::TString => ty.clone(),
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
        tast::Ty::TVec { elem } => tast::Ty::TVec {
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        tast::Ty::TRef { elem } => tast::Ty::TRef {
            elem: Box::new(substitute_ty_params(elem, subst)),
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

fn collect_runtime_types(
    file: &anf::File,
) -> (IndexSet<tast::Ty>, IndexSet<tast::Ty>, IndexSet<tast::Ty>) {
    struct Collector {
        tuples: IndexSet<tast::Ty>,
        arrays: IndexSet<tast::Ty>,
        refs: IndexSet<tast::Ty>,
    }

    impl Collector {
        fn collect_file(
            mut self,
            file: &anf::File,
        ) -> (IndexSet<tast::Ty>, IndexSet<tast::Ty>, IndexSet<tast::Ty>) {
            for item in &file.toplevels {
                self.collect_fn(item);
            }
            (self.tuples, self.arrays, self.refs)
        }

        fn collect_fn(&mut self, item: &anf::Fn) {
            for (_, ty) in &item.params {
                self.collect_type(ty);
            }
            self.collect_type(&item.ret_ty);
            self.collect_aexpr(&item.body);
        }

        fn collect_aexpr(&mut self, expr: &anf::AExpr) {
            match expr {
                anf::AExpr::ACExpr { expr } => self.collect_cexpr(expr),
                anf::AExpr::ALet {
                    value, body, ty, ..
                } => {
                    self.collect_cexpr(value);
                    self.collect_aexpr(body);
                    self.collect_type(ty);
                }
            }
        }

        fn collect_cexpr(&mut self, expr: &anf::CExpr) {
            match expr {
                anf::CExpr::CImm { imm } => self.collect_imm(imm),
                anf::CExpr::EConstr { args, ty, .. }
                | anf::CExpr::ETuple { items: args, ty }
                | anf::CExpr::EArray { items: args, ty } => {
                    for arg in args {
                        self.collect_imm(arg);
                    }
                    self.collect_type(ty);
                }
                anf::CExpr::EMatch {
                    expr,
                    arms,
                    default,
                    ty,
                } => {
                    self.collect_imm(expr);
                    for arm in arms {
                        self.collect_aexpr(&arm.body);
                    }
                    if let Some(default) = default {
                        self.collect_aexpr(default);
                    }
                    self.collect_type(ty);
                }
                anf::CExpr::EIf {
                    cond,
                    then,
                    else_,
                    ty,
                } => {
                    self.collect_imm(cond);
                    self.collect_aexpr(then);
                    self.collect_aexpr(else_);
                    self.collect_type(ty);
                }
                anf::CExpr::EWhile { cond, body, ty } => {
                    self.collect_aexpr(cond);
                    self.collect_aexpr(body);
                    self.collect_type(ty);
                }
                anf::CExpr::EConstrGet { expr, ty, .. } => {
                    self.collect_imm(expr);
                    self.collect_type(ty);
                }
                anf::CExpr::EUnary { expr, ty, .. } => {
                    self.collect_imm(expr);
                    self.collect_type(ty);
                }
                anf::CExpr::EBinary { lhs, rhs, ty, .. } => {
                    self.collect_imm(lhs);
                    self.collect_imm(rhs);
                    self.collect_type(ty);
                }
                anf::CExpr::ECall { func, args, ty } => {
                    self.collect_imm(func);
                    for arg in args {
                        self.collect_imm(arg);
                    }
                    self.collect_type(ty);
                }
                anf::CExpr::EToDyn {
                    for_ty, expr, ty, ..
                } => {
                    self.collect_type(for_ty);
                    self.collect_imm(expr);
                    self.collect_type(ty);
                }
                anf::CExpr::EDynCall {
                    receiver, args, ty, ..
                } => {
                    self.collect_imm(receiver);
                    for arg in args {
                        self.collect_imm(arg);
                    }
                    self.collect_type(ty);
                }
                anf::CExpr::EGo { closure, ty } => {
                    self.collect_imm(closure);
                    self.collect_type(ty);
                }
                anf::CExpr::EProj { tuple, ty, .. } => {
                    self.collect_imm(tuple);
                    self.collect_type(ty);
                }
            }
        }

        fn collect_imm(&mut self, imm: &anf::ImmExpr) {
            match imm {
                anf::ImmExpr::ImmVar { ty, .. }
                | anf::ImmExpr::ImmPrim { ty, .. }
                | anf::ImmExpr::ImmTag { ty, .. } => {
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
                tast::Ty::TRef { elem } => {
                    if self.refs.insert(ty.clone()) {
                        self.collect_type(elem);
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
            anf::ImmExpr::ImmVar { ty, .. }
            | anf::ImmExpr::ImmPrim { ty, .. }
            | anf::ImmExpr::ImmTag { ty, .. } => collect_ty(req, ty),
        }
    }

    fn collect_cexpr(req: &mut DynRequirements, expr: &anf::CExpr) {
        match expr {
            anf::CExpr::CImm { imm } => collect_imm(req, imm),
            anf::CExpr::EConstr { args, ty, .. } => {
                for a in args {
                    collect_imm(req, a);
                }
                collect_ty(req, ty);
            }
            anf::CExpr::ETuple { items, ty } | anf::CExpr::EArray { items, ty } => {
                for a in items {
                    collect_imm(req, a);
                }
                collect_ty(req, ty);
            }
            anf::CExpr::EMatch {
                expr,
                arms,
                default,
                ty,
            } => {
                collect_imm(req, expr);
                for arm in arms {
                    collect_aexpr(req, &arm.body);
                }
                if let Some(default) = default {
                    collect_aexpr(req, default);
                }
                collect_ty(req, ty);
            }
            anf::CExpr::EIf {
                cond,
                then,
                else_,
                ty,
            } => {
                collect_imm(req, cond);
                collect_aexpr(req, then);
                collect_aexpr(req, else_);
                collect_ty(req, ty);
            }
            anf::CExpr::EWhile { cond, body, ty } => {
                collect_aexpr(req, cond);
                collect_aexpr(req, body);
                collect_ty(req, ty);
            }
            anf::CExpr::EConstrGet { expr, ty, .. } => {
                collect_imm(req, expr);
                collect_ty(req, ty);
            }
            anf::CExpr::EUnary { expr, ty, .. } => {
                collect_imm(req, expr);
                collect_ty(req, ty);
            }
            anf::CExpr::EBinary { lhs, rhs, ty, .. } => {
                collect_imm(req, lhs);
                collect_imm(req, rhs);
                collect_ty(req, ty);
            }
            anf::CExpr::ECall { func, args, ty } => {
                collect_imm(req, func);
                for a in args {
                    collect_imm(req, a);
                }
                collect_ty(req, ty);
            }
            anf::CExpr::EToDyn {
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
            anf::CExpr::EDynCall {
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
            anf::CExpr::EGo { closure, ty } => {
                collect_imm(req, closure);
                collect_ty(req, ty);
            }
            anf::CExpr::EProj { tuple, ty, .. } => {
                collect_imm(req, tuple);
                collect_ty(req, ty);
            }
        }
    }

    fn collect_aexpr(req: &mut DynRequirements, expr: &anf::AExpr) {
        match expr {
            anf::AExpr::ACExpr { expr } => collect_cexpr(req, expr),
            anf::AExpr::ALet {
                value, body, ty, ..
            } => {
                collect_cexpr(req, value);
                collect_aexpr(req, body);
                collect_ty(req, ty);
            }
        }
    }

    let mut req = DynRequirements::default();
    for f in &file.toplevels {
        for (_, ty) in &f.params {
            collect_ty(&mut req, ty);
        }
        collect_ty(&mut req, &f.ret_ty);
        collect_aexpr(&mut req, &f.body);
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

fn gen_dyn_helper_fns(goenv: &GlobalGoEnv, req: &DynRequirements) -> Vec<goast::Item> {
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
) -> goast::Fn {
    let fn_name = dyn_wrap_go_name(trait_name, for_ty, method_name);

    let mut go_params = Vec::with_capacity(params.len() + 1);
    go_params.push(("self".to_string(), any_go_type()));
    for (i, pty) in params.iter().enumerate() {
        go_params.push((format!("p{}", i), tast_ty_to_go_type(pty)));
    }

    let trait_ident = TastIdent(trait_name.to_string());
    let impl_name = trait_impl_fn_name(&trait_ident, for_ty, method_name);
    let impl_go_name = go_ident(&impl_name);

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
                let fields = args
                    .iter()
                    .enumerate()
                    .map(|(i, a)| (format!("_{}", i), compile_imm(goenv, a)))
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
                    .map(|((fname, _), arg)| (go_ident(&fname.0), compile_imm(goenv, arg)))
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
            let elems = items.iter().map(|item| compile_imm(goenv, item)).collect();
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
                    let fields =
                        instantiate_struct_fields(goenv, &struct_constructor.type_name, &type_args);
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
                && (*name == "ref" || *name == "ref_get" || *name == "ref_set")
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
    default: &Option<Box<anf::AExpr>>,
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
                        stmts: build_branch(arm.body.clone()),
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
        stmts: build_branch((**d).clone()),
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
    default: &Option<Box<anf::AExpr>>,
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
                        stmts: build_branch(arm.body.clone()),
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
        stmts: build_branch((**d).clone()),
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
    default: &Option<Box<anf::AExpr>>,
    mut build_branch: F,
) -> Vec<goast::Stmt>
where
    F: FnMut(anf::AExpr) -> Vec<goast::Stmt>,
{
    match imm_ty(scrutinee) {
        tast::Ty::TUnit => {
            if let Some(first) = arms.first() {
                return build_branch(first.body.clone());
            }
            if let Some(default_arm) = default.as_ref() {
                return build_branch((**default_arm).clone());
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
                                stmts: build_branch(arm.body.clone()),
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
                stmts: build_branch((**d).clone()),
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
        tast::Ty::TFloat32 => {
            compile_float_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                v.as_float32().map(|x| x as f64)
            })
        }
        tast::Ty::TFloat64 => {
            compile_float_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                v.as_float64()
            })
        }
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
                                stmts: build_branch(arm.body.clone()),
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
                stmts: build_branch((**d).clone()),
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
                            stmts: build_branch(arm.body.clone()),
                        },
                    ));
                } else {
                    panic!("expected ImmTag in enum match arm");
                }
            }
            let default_block = default.as_ref().map(|d| goast::Block {
                stmts: build_branch((**d).clone()),
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
                                stmts: build_branch(arm.body.clone()),
                            },
                        ));
                    } else {
                        panic!("expected ImmTag in enum match arm");
                    }
                }
                let default_block = default.as_ref().map(|d| goast::Block {
                    stmts: build_branch((**d).clone()),
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

fn compile_aexpr_effect(goenv: &GlobalGoEnv, gensym: &Gensym, e: anf::AExpr) -> Vec<goast::Stmt> {
    match e {
        AExpr::ACExpr { expr } => match expr {
            anf::CExpr::EIf {
                cond, then, else_, ..
            } => {
                let cond_e = compile_imm(goenv, &cond);
                let then_block = goast::Block {
                    stmts: compile_aexpr_effect(goenv, gensym, *then),
                };
                let else_block = goast::Block {
                    stmts: compile_aexpr_effect(goenv, gensym, *else_),
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
    cond: anf::AExpr,
    body: anf::AExpr,
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

    let mut loop_body = compile_aexpr_assign(goenv, gensym, &cond_var, cond);
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
    loop_body.extend(compile_aexpr_effect(goenv, gensym, body));

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
                let then_stmts = compile_aexpr_assign(goenv, gensym, target, *then);
                let else_stmts = compile_aexpr_assign(goenv, gensym, target, *else_);
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
                    stmts: compile_aexpr(goenv, gensym, *then),
                };
                let else_block = goast::Block {
                    stmts: compile_aexpr(goenv, gensym, *else_),
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

    let is_entry = f.name == "main" || f.name.ends_with("::main");
    let patched_name = if is_entry {
        "main0".to_string()
    } else {
        go_ident(&f.name)
    };

    let body = f.body;

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

pub fn go_file(
    anfenv: GlobalAnfEnv,
    gensym: &Gensym,
    file: anf::File,
) -> (goast::File, GlobalGoEnv) {
    let goenv = GlobalGoEnv::from_anf_env(anfenv);
    let mut all = Vec::new();

    let (tuple_types, array_types, ref_types) = collect_runtime_types(&file);

    all.extend(runtime::make_runtime());
    all.extend(runtime::make_array_runtime(&array_types));
    all.extend(runtime::make_ref_runtime(&ref_types));

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
    toplevels.extend(gen_dyn_helper_fns(&goenv, &dyn_req));
    for item in file.toplevels {
        let gof = compile_fn(&goenv, gensym, item);
        toplevels.push(goast::Item::Fn(gof));
    }
    all.extend(toplevels);
    all.push(goast::Item::Fn(goast::Fn {
        name: "main".to_string(),
        params: vec![],
        ret_ty: None,
        body: goast::Block {
            stmts: vec![goast::Stmt::Expr(goast::Expr::Call {
                func: Box::new(goast::Expr::Var {
                    name: "main0".to_string(),
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
