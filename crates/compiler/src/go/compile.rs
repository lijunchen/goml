use ast::ast::Ident;

use crate::{
    anf::{self, AExpr, GlobalAnfEnv},
    common::{Constructor, Prim},
    env::{EnumDef, ExternFunc, ExternType, Gensym, StructDef},
    go::goast::{self, go_type_name_for, tast_ty_to_go_type},
    lift::is_closure_env_struct,
    mangle::encode_ty,
    tast::{self},
};

use indexmap::{IndexMap, IndexSet};
use std::collections::HashMap;

use super::goty;
use super::runtime;

#[derive(Debug, Clone, Default)]
pub struct GlobalGoEnv {
    pub enums: IndexMap<Ident, EnumDef>,
    pub structs: IndexMap<Ident, StructDef>,
    pub trait_defs: IndexMap<(String, String), tast::Ty>,
    pub overloaded_funcs_to_trait_name: IndexMap<String, Ident>,
    pub trait_impls: IndexMap<(String, String, Ident), tast::Ty>,
    pub inherent_impls: IndexMap<(String, Ident), (String, tast::Ty)>,
    pub funcs: IndexMap<String, tast::Ty>,
    pub extern_funcs: IndexMap<String, ExternFunc>,
    pub extern_types: IndexMap<String, ExternType>,
    pub array_types: IndexSet<tast::Ty>,
    pub ref_types: IndexSet<tast::Ty>,
}

impl GlobalGoEnv {
    pub fn from_anf_env(anfenv: GlobalAnfEnv) -> Self {
        Self {
            enums: anfenv.enums,
            structs: anfenv.structs,
            trait_defs: anfenv.trait_defs,
            overloaded_funcs_to_trait_name: anfenv.overloaded_funcs_to_trait_name,
            trait_impls: anfenv.trait_impls,
            inherent_impls: anfenv.inherent_impls,
            funcs: anfenv.funcs,
            extern_funcs: anfenv.extern_funcs,
            extern_types: anfenv.extern_types,
            array_types: anfenv.array_types,
            ref_types: anfenv.ref_types,
        }
    }

    pub fn enums(&self) -> &IndexMap<Ident, EnumDef> {
        &self.enums
    }

    pub fn structs(&self) -> &IndexMap<Ident, StructDef> {
        &self.structs
    }

    /// Lookup the apply method for a closure environment struct via inherent_impls.
    pub fn closure_apply_method(&self, closure_ty: &tast::Ty) -> Option<(String, tast::Ty)> {
        let tast::Ty::TStruct { name } = closure_ty else {
            return None;
        };
        if !is_closure_env_struct(name) {
            unreachable!(
                "closure_apply_method called on non-closure environment struct {:?}",
                closure_ty
            );
        }
        let key = (encode_ty(closure_ty), Ident::new("apply"));
        self.inherent_impls.get(&key).cloned()
    }

    pub fn insert_enum(&mut self, def: EnumDef) {
        self.enums.insert(def.name.clone(), def);
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

    if let Some(signed_value) = value.as_signed() {
        return goast::Expr::Int {
            value: signed_value.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(unsigned_value) = value.as_unsigned() {
        return goast::Expr::Int {
            value: unsigned_value.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(float_value) = value.as_float() {
        return goast::Expr::Float {
            value: float_value,
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
            name: name.clone(),
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
        | anf::CExpr::ECall { ty, .. }
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
                    .enums()
                    .get(&enum_constructor.type_name)
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

const NEG_BUILTINS: &[&str] = &[
    "int8_neg",
    "int16_neg",
    "int32_neg",
    "int64_neg",
    "uint8_neg",
    "uint16_neg",
    "uint32_neg",
    "uint64_neg",
    "float32_neg",
    "float64_neg",
];

const ADD_BUILTINS: &[&str] = &[
    "int8_add",
    "int16_add",
    "int32_add",
    "int64_add",
    "uint8_add",
    "uint16_add",
    "uint32_add",
    "uint64_add",
    "float32_add",
    "float64_add",
    "string_add",
];

const SUB_BUILTINS: &[&str] = &[
    "int8_sub",
    "int16_sub",
    "int32_sub",
    "int64_sub",
    "uint8_sub",
    "uint16_sub",
    "uint32_sub",
    "uint64_sub",
    "float32_sub",
    "float64_sub",
];

const MUL_BUILTINS: &[&str] = &[
    "int8_mul",
    "int16_mul",
    "int32_mul",
    "int64_mul",
    "uint8_mul",
    "uint16_mul",
    "uint32_mul",
    "uint64_mul",
    "float32_mul",
    "float64_mul",
];

const DIV_BUILTINS: &[&str] = &[
    "int8_div",
    "int16_div",
    "int32_div",
    "int64_div",
    "uint8_div",
    "uint16_div",
    "uint32_div",
    "uint64_div",
    "float32_div",
    "float64_div",
];

const LESS_BUILTINS: &[&str] = &[
    "int8_less",
    "int16_less",
    "int32_less",
    "int64_less",
    "uint8_less",
    "uint16_less",
    "uint32_less",
    "uint64_less",
    "float32_less",
    "float64_less",
];

fn builtin_unary_operator(name: &str) -> Option<goast::UnaryOp> {
    if name == "bool_not" {
        return Some(goast::UnaryOp::Not);
    }
    if NEG_BUILTINS.contains(&name) {
        return Some(goast::UnaryOp::Neg);
    }
    None
}

fn builtin_binary_operator(name: &str) -> Option<goast::BinaryOp> {
    if name == "bool_and" {
        return Some(goast::BinaryOp::And);
    }
    if name == "bool_or" {
        return Some(goast::BinaryOp::Or);
    }
    if ADD_BUILTINS.contains(&name) {
        return Some(goast::BinaryOp::Add);
    }
    if SUB_BUILTINS.contains(&name) {
        return Some(goast::BinaryOp::Sub);
    }
    if MUL_BUILTINS.contains(&name) {
        return Some(goast::BinaryOp::Mul);
    }
    if DIV_BUILTINS.contains(&name) {
        return Some(goast::BinaryOp::Div);
    }
    if LESS_BUILTINS.contains(&name) {
        return Some(goast::BinaryOp::Less);
    }
    None
}

fn compile_builtin_call(
    name: &str,
    args: Vec<goast::Expr>,
    ty: &tast::Ty,
) -> Result<goast::Expr, Vec<goast::Expr>> {
    if let Some(op) = builtin_unary_operator(name) {
        let mut args = args.into_iter();
        let arg = args
            .next()
            .unwrap_or_else(|| panic!("builtin {} expects 1 argument", name));
        if args.next().is_some() {
            panic!("builtin {} expects 1 argument", name);
        }
        return Ok(goast::Expr::UnaryOp {
            op,
            expr: Box::new(arg),
            ty: tast_ty_to_go_type(ty),
        });
    }

    if let Some(op) = builtin_binary_operator(name) {
        let mut args = args.into_iter();
        let lhs = args
            .next()
            .unwrap_or_else(|| panic!("builtin {} expects 2 arguments", name));
        let rhs = args
            .next()
            .unwrap_or_else(|| panic!("builtin {} expects 2 arguments", name));
        if args.next().is_some() {
            panic!("builtin {} expects 2 arguments", name);
        }
        return Ok(goast::Expr::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            ty: tast_ty_to_go_type(ty),
        });
    }

    Err(args)
}

fn variant_struct_name(goenv: &GlobalGoEnv, enum_name: &str, variant_name: &str) -> String {
    // Count how many enums define a variant with this name.
    let mut count = 0;
    for (_ename, edef) in goenv.enums().iter() {
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
        format!("{}_{}", enum_name, variant_name)
    } else {
        variant_name.to_string()
    }
}

fn lookup_variant_name(goenv: &GlobalGoEnv, ty: &tast::Ty, index: usize) -> String {
    let name = ty.get_constr_name_unsafe();
    if let Some(def) = goenv.enums().get(&Ident::new(&name)) {
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
    type_name: &Ident,
    type_args: &[tast::Ty],
) -> Vec<(String, tast::Ty)> {
    let struct_def = goenv
        .structs()
        .get(type_name)
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

fn collect_tuple_types(file: &anf::File) -> IndexSet<tast::Ty> {
    struct Collector {
        tuples: IndexSet<tast::Ty>,
    }

    impl Collector {
        fn collect_file(mut self, file: &anf::File) -> IndexSet<tast::Ty> {
            for item in &file.toplevels {
                self.collect_fn(item);
            }
            self.tuples
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
                anf::CExpr::ECall { func, args, ty } => {
                    self.collect_imm(func);
                    for arg in args {
                        self.collect_imm(arg);
                    }
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
                tast::Ty::TArray { elem, .. } | tast::Ty::TRef { elem } => {
                    self.collect_type(elem);
                }
                tast::Ty::TApp { ty, args } => {
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
    }
    .collect_file(file)
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
                    .structs()
                    .get(&struct_constructor.type_name)
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
                    .map(|((fname, _), arg)| (fname.0.clone(), compile_imm(goenv, arg)))
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
                        .enums()
                        .get(&enum_constructor.type_name)
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
                        field: field_name.clone(),
                        ty: tast_ty_to_go_type(field_ty),
                    }
                }
            }
        }
        anf::CExpr::ECall { func, args, ty } => {
            let mut compiled_args: Option<Vec<_>> = None;
            if let anf::ImmExpr::ImmVar { name, .. } = &func {
                let args_exprs = args
                    .iter()
                    .map(|arg| compile_imm(goenv, arg))
                    .collect::<Vec<_>>();
                match compile_builtin_call(name, args_exprs, ty) {
                    Ok(expr) => return expr,
                    Err(args_exprs) => compiled_args = Some(args_exprs),
                }
            }

            let compiled_args = compiled_args
                .unwrap_or_else(|| args.iter().map(|arg| compile_imm(goenv, arg)).collect());
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
                && let Some(extern_fn) = goenv.extern_funcs.get(name)
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
        tast::Ty::TInt32
        | tast::Ty::TInt8
        | tast::Ty::TInt16
        | tast::Ty::TInt64
        | tast::Ty::TUint8
        | tast::Ty::TUint16
        | tast::Ty::TUint32
        | tast::Ty::TUint64
        | tast::Ty::TFloat32
        | tast::Ty::TFloat64 => {
            let mut cases = Vec::new();
            for arm in arms {
                if let anf::ImmExpr::ImmPrim { value, .. } = &arm.lhs {
                    let case_expr = if let Some(signed) = value.as_signed() {
                        goast::Expr::Int {
                            value: signed.to_string(),
                            ty: tast_ty_to_go_type(&imm_ty(&arm.lhs)),
                        }
                    } else if let Some(unsigned) = value.as_unsigned() {
                        goast::Expr::Int {
                            value: unsigned.to_string(),
                            ty: tast_ty_to_go_type(&imm_ty(&arm.lhs)),
                        }
                    } else if let Some(float_value) = value.as_float() {
                        goast::Expr::Float {
                            value: float_value,
                            ty: tast_ty_to_go_type(&imm_ty(&arm.lhs)),
                        }
                    } else {
                        panic!("expected numeric primitive in match arm");
                    };
                    cases.push((
                        case_expr,
                        goast::Block {
                            stmts: build_branch(arm.body.clone()),
                        },
                    ));
                } else {
                    panic!("expected primitive literal in numeric match arm");
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
        | anf::CExpr::EProj { .. } => Vec::new(),
        anf::CExpr::ECall { func, args, .. } => {
            if let Some(spawn) = compile_spawn_call(goenv, func, args) {
                vec![spawn.stmt]
            } else {
                vec![goast::Stmt::Expr(compile_cexpr(goenv, expr))]
            }
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

struct SpawnCompilation {
    stmt: goast::Stmt,
    result_ty: goty::GoType,
    result_expr: goast::Expr,
}

fn compile_spawn_call(
    goenv: &GlobalGoEnv,
    func: &anf::ImmExpr,
    args: &[anf::ImmExpr],
) -> Option<SpawnCompilation> {
    let anf::ImmExpr::ImmVar { name, .. } = func else {
        return None;
    };
    if name != "spawn" {
        return None;
    }

    if args.len() != 1 {
        return None;
    }
    let closure_env = &args[0];

    let closure_ty = imm_ty(closure_env);
    let apply = find_closure_apply_fn(goenv, &closure_ty)?;

    let apply_call = anf::CExpr::ECall {
        func: anf::ImmExpr::ImmVar {
            name: apply.name.clone(),
            ty: apply.ty.clone(),
        },
        args: vec![closure_env.clone()],
        ty: apply.ret_ty.clone(),
    };

    let call_expr = compile_cexpr(goenv, &apply_call);
    Some(SpawnCompilation {
        stmt: goast::Stmt::Go { call: call_expr },
        result_ty: goty::GoType::TUnit,
        result_expr: goast::Expr::Unit {
            ty: goty::GoType::TUnit,
        },
    })
}

fn find_closure_apply_fn(goenv: &GlobalGoEnv, closure_ty: &tast::Ty) -> Option<ClosureApplyFn> {
    // Look up the apply method via inherent_impls
    let (apply_name, fn_ty) = goenv.closure_apply_method(closure_ty)?;

    let tast::Ty::TFunc { params, ret_ty } = &fn_ty else {
        return None;
    };

    if params.first()? != closure_ty {
        return None;
    }

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
                        name: name.clone(),
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
                ref simple @ anf::CExpr::ECall {
                    ref func, ref args, ..
                } => {
                    if let Some(spawn) = compile_spawn_call(goenv, func, args) {
                        out.push(spawn.stmt);
                        out.push(goast::Stmt::VarDecl {
                            name: name.clone(),
                            ty: spawn.result_ty.clone(),
                            value: Some(spawn.result_expr),
                        });
                        out.extend(compile_aexpr_effect(goenv, gensym, *body));
                        return out;
                    }

                    out.push(goast::Stmt::VarDecl {
                        name: name.clone(),
                        ty: cexpr_ty(goenv, simple),
                        value: Some(compile_cexpr(goenv, simple)),
                    });
                }
                simple => {
                    out.push(goast::Stmt::VarDecl {
                        name: name.clone(),
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
        name: cond_var.clone(),
        ty: goty::GoType::TBool,
        value: None,
    });

    let mut loop_body = compile_aexpr_assign(goenv, gensym, &cond_var, cond);
    let not_cond = goast::Expr::UnaryOp {
        op: goast::UnaryOp::Not,
        expr: Box::new(goast::Expr::Var {
            name: cond_var.clone(),
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
                    name: target.to_string(),
                    value: goast::Expr::Unit {
                        ty: goty::GoType::TUnit,
                    },
                });
                stmts
            }
            other @ (anf::CExpr::CImm { .. }
            | anf::CExpr::EConstr { .. }
            | anf::CExpr::EConstrGet { .. }
            | anf::CExpr::EProj { .. }
            | anf::CExpr::ETuple { .. }
            | anf::CExpr::EArray { .. }) => vec![goast::Stmt::Assignment {
                name: target.to_string(),
                value: compile_cexpr(goenv, &other),
            }],
            anf::CExpr::ECall { func, args, ty } => {
                if let Some(spawn) = compile_spawn_call(goenv, &func, &args) {
                    vec![
                        spawn.stmt,
                        goast::Stmt::Assignment {
                            name: target.to_string(),
                            value: spawn.result_expr,
                        },
                    ]
                } else {
                    vec![goast::Stmt::Assignment {
                        name: target.to_string(),
                        value: compile_cexpr(goenv, &anf::CExpr::ECall { func, args, ty }),
                    }]
                }
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
                        name: name.clone(),
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
                ref simple @ anf::CExpr::ECall {
                    ref func, ref args, ..
                } => {
                    if let Some(spawn) = compile_spawn_call(goenv, func, args) {
                        out.push(spawn.stmt);
                        out.push(goast::Stmt::VarDecl {
                            name: name.clone(),
                            ty: spawn.result_ty.clone(),
                            value: Some(spawn.result_expr),
                        });
                        out.extend(compile_aexpr_assign(goenv, gensym, target, *body));
                        return out;
                    }

                    out.push(goast::Stmt::VarDecl {
                        name: name.clone(),
                        ty: cexpr_ty(goenv, simple),
                        value: Some(compile_cexpr(goenv, simple)),
                    });
                }
                simple => {
                    out.push(goast::Stmt::VarDecl {
                        name: name.clone(),
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
                        name: name.clone(),
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
                ref simple @ anf::CExpr::ECall {
                    ref func, ref args, ..
                } => {
                    if let Some(spawn) = compile_spawn_call(goenv, func, args) {
                        stmts.push(spawn.stmt);
                        stmts.push(goast::Stmt::VarDecl {
                            name: name.clone(),
                            ty: spawn.result_ty.clone(),
                            value: Some(spawn.result_expr),
                        });
                        stmts.extend(compile_aexpr(goenv, gensym, *body));
                        return stmts;
                    }

                    stmts.push(goast::Stmt::VarDecl {
                        name: name.clone(),
                        ty: cexpr_ty(goenv, simple),
                        value: Some(compile_cexpr(goenv, simple)),
                    });
                }
                simple => {
                    stmts.push(goast::Stmt::VarDecl {
                        name: name.clone(),
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
        params.push((name, tast_ty_to_go_type(&ty)));
    }

    let go_ret_ty = tast_ty_to_go_type(&f.ret_ty);

    let patched_name = if f.name == "main" {
        "main0".to_string()
    } else {
        f.name.clone()
    };

    let body = f.body;

    let (ret_ty, body_stmts) = match go_ret_ty {
        goty::GoType::TVoid => (None, compile_aexpr(goenv, gensym, body)),
        _ => {
            let ret_name = gensym.gensym("ret");
            let mut stmts = Vec::new();

            stmts.push(goast::Stmt::VarDecl {
                name: ret_name.clone(),
                ty: go_ret_ty.clone(),
                value: None,
            });

            stmts.extend(compile_aexpr_assign(goenv, gensym, &ret_name, body));

            stmts.push(goast::Stmt::Return {
                expr: Some(goast::Expr::Var {
                    name: ret_name,
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

    all.extend(runtime::make_runtime());
    all.extend(runtime::make_array_runtime(&goenv.array_types));
    all.extend(runtime::make_ref_runtime(&goenv.ref_types));

    if !goenv.extern_funcs.is_empty() || !goenv.extern_types.is_empty() {
        let mut existing_imports: IndexSet<String> = IndexSet::new();
        for item in &all {
            if let goast::Item::Import(import_decl) = item {
                for spec in &import_decl.specs {
                    existing_imports.insert(spec.path.clone());
                }
            }
        }

        let mut extra_specs = Vec::new();
        for extern_fn in goenv.extern_funcs.values() {
            if existing_imports.insert(extern_fn.package_path.clone()) {
                extra_specs.push(goast::ImportSpec {
                    alias: None,
                    path: extern_fn.package_path.clone(),
                });
            }
        }
        for extern_ty in goenv.extern_types.values() {
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

    let tuple_types = collect_tuple_types(&file);
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

    let mut toplevels = gen_type_definition(&goenv);
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
    for (name, def) in goenv.structs().iter() {
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
                name: fname.0.clone(),
                ty: tast_ty_to_go_type(fty),
            })
            .collect();
        defs.push(goast::Item::Struct(goast::Struct {
            name: name.0.clone(),
            fields,
            methods: vec![],
        }));
    }

    for (name, def) in goenv.enums().iter() {
        // Skip generating Go types for generic-specialized enums whose fields still contain type parameters
        let has_type_param = name.0.contains("TParam")
            || def
                .variants
                .iter()
                .any(|(_, fields)| fields.iter().any(|f| matches!(f, tast::Ty::TParam { .. })));
        if has_type_param {
            continue;
        }
        let type_identifier_method = format!("is{}", name.0);

        defs.push(goast::Item::Interface(goast::Interface {
            name: name.0.clone(),
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

    for (name, ext) in goenv.extern_types.iter() {
        if let Some(package_path) = &ext.package_path {
            let alias = go_package_alias(package_path);
            let go_ty = goty::GoType::TName {
                name: format!("{}.{}", alias, ext.go_name),
            };
            defs.push(goast::Item::TypeAlias(goast::TypeAlias {
                name: name.clone(),
                ty: go_ty,
            }));
        }
    }
    defs
}

#[test]
fn test_type_gen() {
    use crate::env::EnumDef;
    use expect_test::expect;

    let mut goenv = GlobalGoEnv::default();
    goenv.insert_enum(EnumDef {
        name: Ident::new("Tree"),
        generics: vec![],
        variants: vec![
            (Ident::new("Empty"), vec![]),
            (Ident::new("Leaf"), vec![tast::Ty::TInt32]),
            (
                Ident::new("Node"),
                vec![
                    tast::Ty::TEnum {
                        name: "Tree".to_string(),
                    },
                    tast::Ty::TEnum {
                        name: "Tree".to_string(),
                    },
                ],
            ),
        ],
    });

    let item = gen_type_definition(&goenv);
    let dummy_file = goast::File { toplevels: item };
    expect![[r#"
        type Tree interface {
            isTree()
        }

        type Empty struct {}

        func (_ Empty) isTree() {}

        type Leaf struct {
            _0 int32
        }

        func (_ Leaf) isTree() {}

        type Node struct {
            _0 Tree
            _1 Tree
        }

        func (_ Node) isTree() {}
    "#]]
    .assert_eq(&dummy_file.to_pretty(&goenv, 120));
}
