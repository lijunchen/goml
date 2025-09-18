use std::collections::{HashMap, HashSet};

use ::ast::ast::{ImplBlock, Lident, StructDef, TraitDef, TraitMethodSignature};
use ast::ast;
use ena::unify::InPlaceUnificationTable;

use crate::{
    env::{self, Constraint, Env},
    rename,
    tast::{self, TypeVar},
};

pub fn check_file(ast: ast::File) -> (tast::File, env::Env) {
    let mut env = env::Env::new();
    let ast = rename::Rename::default().rename_file(ast);
    collect_typedefs(&mut env, &ast);
    let mut typer = TypeInference::new();
    let mut typed_toplevel_tasts = vec![];
    for item in ast.toplevels.iter() {
        match item {
            ast::Item::EnumDef(..) => (),
            ast::Item::StructDef(..) => (),
            ast::Item::TraitDef(..) => (),
            ast::Item::ImplBlock(impl_blcok) => {
                let for_ty = ast_ty_to_tast_ty_with_tparams_env(&impl_blcok.for_type, &[]);
                let mut typed_methods = Vec::new();
                for f in impl_blcok.methods.iter() {
                    let mut vars = im::HashMap::<Lident, tast::Ty>::new();
                    for (name, ty) in f.params.iter() {
                        let ty = ast_ty_to_tast_ty_with_tparams_env(ty, &f.generics);
                        vars.insert(name.clone(), ty);
                    }
                    let new_params = f
                        .params
                        .iter()
                        .map(|(name, ty)| {
                            (
                                name.0.clone(),
                                ast_ty_to_tast_ty_with_tparams_env(ty, &f.generics),
                            )
                        })
                        .collect::<Vec<_>>();

                    let ret_ty = match &f.ret_ty {
                        Some(ty) => ast_ty_to_tast_ty_with_tparams_env(ty, &f.generics),
                        None => tast::Ty::TUnit,
                    };

                    let typed_body = typer.infer(&mut env, &vars, &f.body);
                    typer.solve(&env);
                    let typed_body = typer.subst(typed_body);
                    typed_methods.push(tast::Fn {
                        name: f.name.0.clone(),
                        params: new_params,
                        ret_ty,
                        body: typed_body,
                    });
                }
                let trait_name = impl_blcok.trait_name.clone();
                let trait_impl = tast::ImplBlock {
                    trait_name,
                    for_type: for_ty,
                    methods: typed_methods,
                };
                typed_toplevel_tasts.push(tast::Item::ImplBlock(trait_impl));
            }
            ast::Item::Fn(f) => {
                let mut vars = im::HashMap::<Lident, tast::Ty>::new();
                for (name, ty) in f.params.iter() {
                    let ty = ast_ty_to_tast_ty_with_tparams_env(ty, &f.generics);
                    vars.insert(name.clone(), ty);
                }
                let new_params = f
                    .params
                    .iter()
                    .map(|(name, ty)| {
                        (
                            name.0.clone(),
                            ast_ty_to_tast_ty_with_tparams_env(ty, &f.generics),
                        )
                    })
                    .collect::<Vec<_>>();

                let ret_ty = match &f.ret_ty {
                    Some(ty) => ast_ty_to_tast_ty_with_tparams_env(ty, &f.generics),
                    None => tast::Ty::TUnit,
                };

                let typed_body = typer.infer(&mut env, &vars, &f.body);
                typer.solve(&env);
                let typed_body = typer.subst(typed_body);
                typed_toplevel_tasts.push(tast::Item::Fn(tast::Fn {
                    name: f.name.0.clone(),
                    params: new_params,
                    ret_ty,
                    body: typed_body,
                }));
            }
        }
    }

    (
        tast::File {
            toplevels: typed_toplevel_tasts,
        },
        env,
    )
}

fn type_param_name_set(tparams: &[ast::Uident]) -> HashSet<String> {
    tparams.iter().map(|param| param.0.clone()).collect()
}

fn validate_ty(env: &Env, ty: &tast::Ty, tparams: &HashSet<String>) {
    match ty {
        tast::Ty::TVar(_) => {}
        tast::Ty::TUnit | tast::Ty::TBool | tast::Ty::TInt | tast::Ty::TString => {}
        tast::Ty::TTuple { typs } => {
            for ty in typs.iter() {
                validate_ty(env, ty, tparams);
            }
        }
        tast::Ty::TFunc { params, ret_ty } => {
            for param in params.iter() {
                validate_ty(env, param, tparams);
            }
            validate_ty(env, ret_ty.as_ref(), tparams);
        }
        tast::Ty::TParam { name } => {
            if !tparams.contains(name) {
                panic!("Unbound type parameter {}", name);
            }
        }
        tast::Ty::TApp { name, args } => {
            for arg in args.iter() {
                validate_ty(env, arg, tparams);
            }

            if let Some(enum_def) = env.enums.get(name) {
                let expected = enum_def.generics.len();
                let actual = args.len();
                if expected != actual {
                    panic!(
                        "Type {} expects {} type arguments, but got {}",
                        name.0, expected, actual
                    );
                }
            } else if let Some(struct_def) = env.structs.get(name) {
                let expected = struct_def.generics.len();
                let actual = args.len();
                if expected != actual {
                    panic!(
                        "Type {} expects {} type arguments, but got {}",
                        name.0, expected, actual
                    );
                }
            } else {
                panic!("Unknown type constructor {}", name.0);
            }
        }
    }
}

fn collect_typedefs(env: &mut Env, ast: &ast::File) {
    for item in ast.toplevels.iter() {
        match item {
            ast::Item::EnumDef(enum_def) => {
                env.enums
                    .entry(enum_def.name.clone())
                    .or_insert_with(|| env::EnumDef {
                        name: enum_def.name.clone(),
                        generics: enum_def.generics.clone(),
                        variants: Vec::new(),
                    });
            }
            ast::Item::StructDef(StructDef { name, generics, .. }) => {
                env.structs
                    .entry(name.clone())
                    .or_insert_with(|| env::StructDef {
                        name: name.clone(),
                        generics: generics.clone(),
                        fields: Vec::new(),
                    });
            }
            _ => {}
        }
    }

    let empty_tparams = HashSet::new();

    for item in ast.toplevels.iter() {
        match item {
            ast::Item::EnumDef(enum_def) => {
                let params_env = &enum_def.generics;
                let tparam_names = type_param_name_set(params_env);

                let variants = enum_def
                    .variants
                    .iter()
                    .map(|(vcon, typs)| {
                        let typs = typs
                            .iter()
                            .map(|ast_ty| ast_ty_to_tast_ty_with_tparams_env(ast_ty, params_env))
                            .collect::<Vec<_>>();
                        for ty in typs.iter() {
                            validate_ty(env, ty, &tparam_names);
                        }
                        (vcon.clone(), typs)
                    })
                    .collect();
                env.enums.insert(
                    enum_def.name.clone(),
                    env::EnumDef {
                        name: enum_def.name.clone(),
                        generics: enum_def.generics.clone(),
                        variants,
                    },
                );
            }
            ast::Item::StructDef(StructDef {
                name,
                generics,
                fields,
            }) => {
                let tparam_names = type_param_name_set(generics);
                let fields = fields
                    .iter()
                    .map(|(fname, ast_ty)| {
                        let ty = ast_ty_to_tast_ty_with_tparams_env(ast_ty, generics);
                        validate_ty(env, &ty, &tparam_names);
                        (fname.clone(), ty)
                    })
                    .collect();

                env.structs.insert(
                    name.clone(),
                    env::StructDef {
                        name: name.clone(),
                        generics: generics.clone(),
                        fields,
                    },
                );
            }
            ast::Item::TraitDef(TraitDef {
                name: trait_name,
                method_sigs: methods,
            }) => {
                for TraitMethodSignature {
                    name: method_name,
                    params,
                    ret_ty,
                } in methods.iter()
                {
                    env.overloaded_funcs_to_trait_name
                        .insert(method_name.0.clone(), trait_name.clone());

                    let param_tys = params
                        .iter()
                        .map(|ast_ty| ast_ty_to_tast_ty_with_tparams_env(ast_ty, &[]))
                        .collect::<Vec<_>>();
                    let ret_ty = ast_ty_to_tast_ty_with_tparams_env(ret_ty, &[]);
                    let fn_ty = tast::Ty::TFunc {
                        params: param_tys,
                        ret_ty: Box::new(ret_ty),
                    };

                    env.trait_defs
                        .insert((trait_name.0.clone(), method_name.0.clone()), fn_ty);
                }
            }
            ast::Item::ImplBlock(ImplBlock {
                trait_name,
                for_type,
                methods,
            }) => {
                let for_ty = ast_ty_to_tast_ty_with_tparams_env(for_type, &[]);
                validate_ty(env, &for_ty, &empty_tparams);
                let trait_name_str = trait_name.0.clone();
                let trait_method_names: HashSet<String> = env
                    .trait_defs
                    .keys()
                    .filter_map(|(t_name, method_name)| {
                        if t_name == &trait_name_str {
                            Some(method_name.clone())
                        } else {
                            None
                        }
                    })
                    .collect();

                if trait_method_names.is_empty() {
                    panic!(
                        "Trait {} is not defined, cannot implement it for {:?}",
                        trait_name_str, for_ty
                    );
                }

                let mut implemented_methods: HashSet<String> = HashSet::new();

                for m in methods.iter() {
                    let method_name = m.name.clone();
                    let method_name_str = method_name.0.clone();

                    if !trait_method_names.contains(&method_name_str) {
                        panic!(
                            "Method {} is not declared in trait {}",
                            method_name_str, trait_name_str
                        );
                    }

                    if !implemented_methods.insert(method_name_str.clone()) {
                        panic!(
                            "Method {} implemented multiple times in impl of trait {}",
                            method_name_str, trait_name_str
                        );
                    }

                    let trait_sig = env
                        .trait_defs
                        .get(&(trait_name_str.clone(), method_name_str.clone()))
                        .expect("trait method signature to exist");

                    let tparam_names = type_param_name_set(&m.generics);
                    let params = m
                        .params
                        .iter()
                        .map(|(_, ty)| {
                            let ty = ast_ty_to_tast_ty_with_tparams_env(ty, &m.generics);
                            validate_ty(env, &ty, &tparam_names);
                            ty
                        })
                        .collect::<Vec<_>>();
                    let ret = match &m.ret_ty {
                        Some(ty) => {
                            let ret = ast_ty_to_tast_ty_with_tparams_env(ty, &m.generics);
                            validate_ty(env, &ret, &tparam_names);
                            ret
                        }
                        None => tast::Ty::TUnit,
                    };

                    let impl_method_ty = tast::Ty::TFunc {
                        params: params.clone(),
                        ret_ty: Box::new(ret.clone()),
                    };

                    let expected_method_ty = instantiate_trait_method_ty(trait_sig, &for_ty);

                    match (&expected_method_ty, &impl_method_ty) {
                        (
                            tast::Ty::TFunc {
                                params: expected_params,
                                ret_ty: expected_ret,
                            },
                            tast::Ty::TFunc {
                                params: impl_params,
                                ret_ty: impl_ret,
                            },
                        ) => {
                            if expected_params.len() != impl_params.len() {
                                panic!(
                                    "Trait {}::{} expects {} parameters but impl has {}",
                                    trait_name_str,
                                    method_name_str,
                                    expected_params.len(),
                                    impl_params.len()
                                );
                            }

                            for (idx, (expected, actual)) in
                                expected_params.iter().zip(impl_params.iter()).enumerate()
                            {
                                if expected != actual {
                                    panic!(
                                        "Trait {}::{} parameter {} expected type {:?} but found {:?}",
                                        trait_name_str, method_name_str, idx, expected, actual
                                    );
                                }
                            }

                            if **expected_ret != **impl_ret {
                                panic!(
                                    "Trait {}::{} expected return type {:?} but found {:?}",
                                    trait_name_str, method_name_str, expected_ret, impl_ret
                                );
                            }
                        }
                        _ => panic!(
                            "Trait {}::{} does not have a function type signature",
                            trait_name_str, method_name_str
                        ),
                    }

                    env.trait_impls.insert(
                        (trait_name_str.clone(), for_ty.clone(), method_name),
                        impl_method_ty,
                    );
                }

                for method_name in trait_method_names.iter() {
                    if !implemented_methods.contains(method_name) {
                        panic!(
                            "Trait {} implementation for {:?} is missing method {}",
                            trait_name_str, for_ty, method_name
                        );
                    }
                }
            }
            ast::Item::Fn(func) => {
                let name = func.name.clone();
                let tparam_names = type_param_name_set(&func.generics);
                let params = func
                    .params
                    .iter()
                    .map(|(_, ty)| {
                        let ty = ast_ty_to_tast_ty_with_tparams_env(ty, &func.generics);
                        validate_ty(env, &ty, &tparam_names);
                        ty
                    })
                    .collect::<Vec<_>>();
                let ret = match &func.ret_ty {
                    Some(ty) => {
                        let ret = ast_ty_to_tast_ty_with_tparams_env(ty, &func.generics);
                        validate_ty(env, &ret, &tparam_names);
                        ret
                    }
                    None => tast::Ty::TUnit,
                };
                env.funcs.insert(
                    name.0.clone(),
                    tast::Ty::TFunc {
                        params,
                        ret_ty: Box::new(ret),
                    },
                );
            }
        }
    }
}

fn instantiate_trait_method_ty(ty: &tast::Ty, self_ty: &tast::Ty) -> tast::Ty {
    match ty {
        tast::Ty::TVar(var) => tast::Ty::TVar(*var),
        tast::Ty::TUnit => tast::Ty::TUnit,
        tast::Ty::TBool => tast::Ty::TBool,
        tast::Ty::TInt => tast::Ty::TInt,
        tast::Ty::TString => tast::Ty::TString,
        tast::Ty::TTuple { typs } => tast::Ty::TTuple {
            typs: typs
                .iter()
                .map(|ty| instantiate_trait_method_ty(ty, self_ty))
                .collect(),
        },
        tast::Ty::TApp { name, args } => {
            if name.0 == "Self" && args.is_empty() {
                self_ty.clone()
            } else {
                tast::Ty::TApp {
                    name: name.clone(),
                    args: args
                        .iter()
                        .map(|ty| instantiate_trait_method_ty(ty, self_ty))
                        .collect(),
                }
            }
        }
        tast::Ty::TParam { name } => tast::Ty::TParam { name: name.clone() },
        tast::Ty::TFunc { params, ret_ty } => tast::Ty::TFunc {
            params: params
                .iter()
                .map(|param| instantiate_trait_method_ty(param, self_ty))
                .collect(),
            ret_ty: Box::new(instantiate_trait_method_ty(ret_ty, self_ty)),
        },
    }
}

pub fn ast_ty_to_tast_ty_with_tparams_env(
    ast_ty: &ast::Ty,
    tparams_env: &[ast::Uident],
) -> tast::Ty {
    match ast_ty {
        ast::Ty::TUnit => tast::Ty::TUnit,
        ast::Ty::TBool => tast::Ty::TBool,
        ast::Ty::TInt => tast::Ty::TInt,
        ast::Ty::TString => tast::Ty::TString,
        ast::Ty::TTuple { typs } => {
            let typs = typs
                .iter()
                .map(|ty| ast_ty_to_tast_ty_with_tparams_env(ty, tparams_env))
                .collect();
            tast::Ty::TTuple { typs }
        }
        ast::Ty::TApp { name, args } => {
            if tparams_env.contains(name) {
                return tast::Ty::TParam {
                    name: name.0.clone(),
                };
            }
            tast::Ty::TApp {
                name: name.clone(),
                args: args
                    .iter()
                    .map(|ty| ast_ty_to_tast_ty_with_tparams_env(ty, tparams_env))
                    .collect(),
            }
        }
        ast::Ty::TFunc { params, ret_ty } => tast::Ty::TFunc {
            params: params
                .iter()
                .map(|ty| ast_ty_to_tast_ty_with_tparams_env(ty, tparams_env))
                .collect(),
            ret_ty: Box::new(ast_ty_to_tast_ty_with_tparams_env(ret_ty, tparams_env)),
        },
    }
}

pub struct TypeInference {
    pub uni: InPlaceUnificationTable<TypeVar>,
}

impl Default for TypeInference {
    fn default() -> Self {
        Self::new()
    }
}

fn occurs(var: TypeVar, ty: &tast::Ty) {
    match ty {
        tast::Ty::TVar(v) => {
            if var == *v {
                panic!("occurs check failed: {:?} occurs in {:?}", var, ty);
            }
        }
        tast::Ty::TUnit => {}
        tast::Ty::TBool => {}
        tast::Ty::TInt => {}
        tast::Ty::TString => {}
        tast::Ty::TTuple { typs } => {
            for ty in typs.iter() {
                occurs(var, ty);
            }
        }
        tast::Ty::TApp { .. } => {}
        tast::Ty::TFunc { params, ret_ty } => {
            for param in params.iter() {
                occurs(var, param);
            }
            occurs(var, ret_ty);
        }
        tast::Ty::TParam { .. } => {}
    }
}

impl TypeInference {
    pub fn solve(&mut self, env: &Env) {
        let mut constraints = env.constraints.clone();
        let mut changed = true;

        fn is_concrete(norm_ty: &tast::Ty) -> bool {
            match norm_ty {
                tast::Ty::TVar(..) => false,
                tast::Ty::TUnit
                | tast::Ty::TBool
                | tast::Ty::TInt
                | tast::Ty::TString
                | tast::Ty::TParam { .. } => true, // TParam is treated as concrete here
                tast::Ty::TTuple { typs } => typs.iter().all(is_concrete),
                tast::Ty::TApp { args, .. } => args.iter().all(is_concrete),
                tast::Ty::TFunc { params, ret_ty } => {
                    params.iter().all(is_concrete) && is_concrete(ret_ty)
                }
            }
        }

        while changed {
            changed = false;
            let mut still_pending = Vec::new();
            for constraint in constraints.drain(..) {
                match constraint {
                    Constraint::TypeEqual(l, r) => {
                        self.unify(&l, &r);
                        changed = true;
                    }
                    Constraint::Overloaded {
                        op,
                        trait_name,
                        call_site_type,
                    } => {
                        let norm_call_site_type = self.norm(&call_site_type);
                        if let tast::Ty::TFunc {
                            params: norm_arg_types,
                            ret_ty: norm_ret_ty,
                        } = norm_call_site_type
                        {
                            if let Some(self_ty) = norm_arg_types.first() {
                                match self_ty {
                                    ty if is_concrete(ty) => {
                                        match env.get_trait_impl(&trait_name, self_ty, &op) {
                                            Some(impl_scheme) => {
                                                let impl_fun_ty = self.inst_ty(&impl_scheme);

                                                let call_fun_ty = tast::Ty::TFunc {
                                                    params: norm_arg_types,
                                                    ret_ty: norm_ret_ty,
                                                };

                                                still_pending.push(Constraint::TypeEqual(
                                                    call_fun_ty,
                                                    impl_fun_ty,
                                                ));

                                                // Made progress!
                                                changed = true;
                                            }
                                            None => {
                                                panic!(
                                                    "No instance found for trait {}<{:?}> for operator {}",
                                                    trait_name.0, ty, op.0
                                                );
                                            }
                                        }
                                    }
                                    tast::Ty::TVar(_) => {
                                        // We cannot resolve this yet. Defer it.
                                        still_pending.push(Constraint::Overloaded {
                                            op,
                                            trait_name,
                                            call_site_type, // Push original back
                                        });
                                    }
                                    _ => {
                                        panic!(
                                            "Overload resolution failed for non-concrete, non-variable type {:?}",
                                            self_ty
                                        );
                                    }
                                }
                            } else {
                                panic!("Overloaded operator {} called with no arguments?", op.0);
                            }
                        } else {
                            panic!(
                                "Overloaded constraint does not involve a function type: {:?}",
                                norm_call_site_type
                            );
                        }
                    }
                }
            }
            constraints.extend(still_pending);
            if !changed && !constraints.is_empty() {
                panic!("Could not solve all constraints: {:?}", constraints);
            }
        }
        if !constraints.is_empty() {
            panic!(
                "Type inference failed, remaining constraints: {:?}",
                constraints
            );
        }
    }

    fn norm(&mut self, ty: &tast::Ty) -> tast::Ty {
        match ty {
            tast::Ty::TVar(v) => {
                if let Some(value) = self.uni.probe_value(*v) {
                    self.norm(&value)
                } else {
                    tast::Ty::TVar(self.uni.find(*v))
                }
            }
            tast::Ty::TUnit => tast::Ty::TUnit,
            tast::Ty::TBool => tast::Ty::TBool,
            tast::Ty::TInt => tast::Ty::TInt,
            tast::Ty::TString => tast::Ty::TString,
            tast::Ty::TTuple { typs } => {
                let typs = typs.iter().map(|ty| self.norm(ty)).collect();
                tast::Ty::TTuple { typs }
            }
            tast::Ty::TApp { name, args } => tast::Ty::TApp {
                name: name.clone(),
                args: args.iter().map(|ty| self.norm(ty)).collect(),
            },
            tast::Ty::TFunc { params, ret_ty } => {
                let params = params.iter().map(|ty| self.norm(ty)).collect();
                let ret_ty = Box::new(self.norm(ret_ty));
                tast::Ty::TFunc { params, ret_ty }
            }
            tast::Ty::TParam { name } => tast::Ty::TParam { name: name.clone() },
        }
    }

    fn unify(&mut self, l: &tast::Ty, r: &tast::Ty) {
        let l_norm = self.norm(l);
        let r_norm = self.norm(r);
        match (&l_norm, &r_norm) {
            (tast::Ty::TVar(a), tast::Ty::TVar(b)) => {
                self.uni.unify_var_var(*a, *b).unwrap_or_else(|_| {
                    panic!("Failed to unify type variables {:?} and {:?}", a, b)
                });
            }
            (tast::Ty::TVar(a), t) | (t, tast::Ty::TVar(a)) => {
                occurs(*a, t);
                self.uni
                    .unify_var_value(*a, Some(t.clone()))
                    .unwrap_or_else(|_| {
                        panic!("Failed to unify type variable {:?} with {:?}", a, t)
                    });
            }

            (tast::Ty::TUnit, tast::Ty::TUnit) => {}
            (tast::Ty::TBool, tast::Ty::TBool) => {}
            (tast::Ty::TInt, tast::Ty::TInt) => {}
            (tast::Ty::TString, tast::Ty::TString) => {}
            (tast::Ty::TTuple { typs: typs1 }, tast::Ty::TTuple { typs: typs2 }) => {
                if typs1.len() != typs2.len() {
                    panic!("Tuple types have different lengths: {:?} and {:?}", l, r);
                }
                for (ty1, ty2) in typs1.iter().zip(typs2.iter()) {
                    self.unify(ty1, ty2);
                }
            }
            (
                tast::Ty::TFunc {
                    params: param1,
                    ret_ty: ret_ty1,
                },
                tast::Ty::TFunc {
                    params: param2,
                    ret_ty: ret_ty2,
                },
            ) => {
                if param1.len() != param2.len() {
                    panic!(
                        "Function types have different parameter lengths: {:?} and {:?}",
                        l, r
                    );
                }
                for (p1, p2) in param1.iter().zip(param2.iter()) {
                    self.unify(p1, p2);
                }
                self.unify(ret_ty1, ret_ty2);
            }
            (
                tast::Ty::TApp {
                    name: name1,
                    args: args1,
                },
                tast::Ty::TApp {
                    name: name2,
                    args: args2,
                },
            ) => {
                if name1 != name2 {
                    panic!("Constructor types are different: {:?} and {:?}", l, r);
                }
                if args1.len() != args2.len() {
                    panic!(
                        "Constructor types have different argument lengths: {:?} and {:?}",
                        l, r
                    );
                }
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    self.unify(arg1, arg2);
                }
            }
            (tast::Ty::TParam { name }, tast::Ty::TParam { name: name2 }) => {
                if name != name2 {
                    panic!("Type parameters are different: {:?} and {:?}", l, r);
                }
            }
            _ => {
                panic!("type not equal {:?} and {:?}", l_norm, r_norm);
            }
        }
    }
}

impl TypeInference {
    pub fn new() -> Self {
        Self {
            uni: InPlaceUnificationTable::new(),
        }
    }

    fn fresh_ty_var(&mut self) -> tast::Ty {
        tast::Ty::TVar(self.uni.new_key(None))
    }

    fn inst_ty(&mut self, ty: &tast::Ty) -> tast::Ty {
        let mut subst: HashMap<String, tast::Ty> = HashMap::new();
        self._go_inst_ty(ty, &mut subst)
    }

    fn _go_inst_ty(&mut self, ty: &tast::Ty, subst: &mut HashMap<String, tast::Ty>) -> tast::Ty {
        match ty {
            tast::Ty::TVar(_) => ty.clone(),
            tast::Ty::TUnit => ty.clone(),
            tast::Ty::TBool => ty.clone(),
            tast::Ty::TInt => ty.clone(),
            tast::Ty::TString => ty.clone(),
            tast::Ty::TTuple { typs } => {
                let typs = typs
                    .iter()
                    .map(|ty| self._go_inst_ty(ty, subst))
                    .collect::<Vec<_>>();
                tast::Ty::TTuple { typs }
            }
            tast::Ty::TApp { name, args } => {
                let args = args
                    .iter()
                    .map(|arg| self._go_inst_ty(arg, subst))
                    .collect::<Vec<_>>();
                tast::Ty::TApp {
                    name: name.clone(),
                    args,
                }
            }
            tast::Ty::TParam { name } => {
                if subst.contains_key(name) {
                    let ty = subst.get(name).unwrap();
                    ty.clone()
                } else {
                    let new_ty = self.fresh_ty_var();
                    subst.insert(name.clone(), new_ty.clone());
                    new_ty
                }
            }
            tast::Ty::TFunc { params, ret_ty } => {
                let params = params
                    .iter()
                    .map(|ty| self._go_inst_ty(ty, subst))
                    .collect::<Vec<_>>();
                let ret_ty = Box::new(self._go_inst_ty(ret_ty, subst));
                tast::Ty::TFunc { params, ret_ty }
            }
        }
    }

    fn subst_ty(&mut self, ty: &tast::Ty) -> tast::Ty {
        match ty {
            tast::Ty::TVar(v) => {
                if let Some(value) = self.uni.probe_value(*v) {
                    self.subst_ty(&value)
                } else {
                    panic!("Type variable {:?} not resolved", v);
                }
            }
            tast::Ty::TUnit => tast::Ty::TUnit,
            tast::Ty::TBool => tast::Ty::TBool,
            tast::Ty::TInt => tast::Ty::TInt,
            tast::Ty::TString => tast::Ty::TString,
            tast::Ty::TTuple { typs } => {
                let typs = typs.iter().map(|ty| self.subst_ty(ty)).collect();
                tast::Ty::TTuple { typs }
            }
            tast::Ty::TApp { name, args } => tast::Ty::TApp {
                name: name.clone(),
                args: args.iter().map(|arg| self.subst_ty(arg)).collect(),
            },
            tast::Ty::TFunc { params, ret_ty } => {
                let params = params.iter().map(|ty| self.subst_ty(ty)).collect();
                let ret_ty = Box::new(self.subst_ty(ret_ty));
                tast::Ty::TFunc { params, ret_ty }
            }
            tast::Ty::TParam { name } => tast::Ty::TParam { name: name.clone() },
        }
    }

    fn subst_pat(&mut self, p: tast::Pat) -> tast::Pat {
        match p {
            tast::Pat::PVar { name, ty, astptr } => {
                let ty = self.subst_ty(&ty);
                tast::Pat::PVar {
                    name: name.clone(),
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Pat::PUnit { ty } => {
                let ty = self.subst_ty(&ty);
                tast::Pat::PUnit { ty: ty.clone() }
            }
            tast::Pat::PBool { value, ty } => {
                let ty = self.subst_ty(&ty);
                tast::Pat::PBool {
                    value,
                    ty: ty.clone(),
                }
            }
            tast::Pat::PInt { value, ty } => {
                let ty = self.subst_ty(&ty);
                tast::Pat::PInt {
                    value,
                    ty: ty.clone(),
                }
            }
            tast::Pat::PString { value, ty } => {
                let ty = self.subst_ty(&ty);
                tast::Pat::PString {
                    value,
                    ty: ty.clone(),
                }
            }
            tast::Pat::PConstr {
                constructor,
                args,
                ty,
            } => {
                let ty = self.subst_ty(&ty);
                let args = args
                    .into_iter()
                    .map(|arg| self.subst_pat(arg))
                    .collect::<Vec<_>>();
                tast::Pat::PConstr {
                    constructor,
                    args,
                    ty: ty.clone(),
                }
            }
            tast::Pat::PTuple { items, ty } => {
                let ty = self.subst_ty(&ty);
                let items = items
                    .into_iter()
                    .map(|item| self.subst_pat(item))
                    .collect::<Vec<_>>();
                tast::Pat::PTuple {
                    items,
                    ty: ty.clone(),
                }
            }
            tast::Pat::PWild { ty } => {
                let ty = self.subst_ty(&ty);
                tast::Pat::PWild { ty: ty.clone() }
            }
        }
    }

    pub fn subst(&mut self, e: tast::Expr) -> tast::Expr {
        match e {
            tast::Expr::EVar { name, ty, astptr } => {
                let ty = self.subst_ty(&ty);
                tast::Expr::EVar {
                    name,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Expr::EUnit { ty } => {
                let ty = self.subst_ty(&ty);
                tast::Expr::EUnit { ty: ty.clone() }
            }
            tast::Expr::EBool { value, ty } => {
                let ty = self.subst_ty(&ty);
                tast::Expr::EBool {
                    value,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EInt { value, ty } => {
                let ty = self.subst_ty(&ty);
                tast::Expr::EInt {
                    value,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EString { value, ty } => {
                let ty = self.subst_ty(&ty);
                tast::Expr::EString {
                    value: value.clone(),
                    ty: ty.clone(),
                }
            }
            tast::Expr::EConstr {
                constructor,
                args,
                ty,
            } => {
                let ty = self.subst_ty(&ty);
                let args = args
                    .into_iter()
                    .map(|arg| self.subst(arg))
                    .collect::<Vec<_>>();
                tast::Expr::EConstr {
                    constructor,
                    args,
                    ty,
                }
            }
            tast::Expr::ETuple { items, ty } => {
                let ty = self.subst_ty(&ty);
                let items = items
                    .into_iter()
                    .map(|item| self.subst(item))
                    .collect::<Vec<_>>();
                tast::Expr::ETuple {
                    items,
                    ty: ty.clone(),
                }
            }
            tast::Expr::ELet {
                pat,
                value,
                body,
                ty,
            } => {
                let ty = self.subst_ty(&ty);
                let pat = self.subst_pat(pat);
                let value = Box::new(self.subst(*value));
                let body = Box::new(self.subst(*body));
                tast::Expr::ELet {
                    pat,
                    value,
                    body,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EMatch { expr, arms, ty } => {
                let ty = self.subst_ty(&ty);
                let expr = Box::new(self.subst(*expr));
                let arms = arms
                    .into_iter()
                    .map(|arm| tast::Arm {
                        pat: self.subst_pat(arm.pat),
                        body: self.subst(arm.body),
                    })
                    .collect::<Vec<_>>();
                tast::Expr::EMatch {
                    expr,
                    arms,
                    ty: ty.clone(),
                }
            }
            tast::Expr::ECall { func, args, ty } => {
                let ty = self.subst_ty(&ty);
                let args = args
                    .into_iter()
                    .map(|arg| self.subst(arg))
                    .collect::<Vec<_>>();
                tast::Expr::ECall {
                    func,
                    args,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EProj { tuple, index, ty } => {
                let ty = self.subst_ty(&ty);
                let tuple = Box::new(self.subst(*tuple));
                tast::Expr::EProj {
                    tuple,
                    index,
                    ty: ty.clone(),
                }
            }
        }
    }

    pub fn infer(
        &mut self,
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        e: &ast::Expr,
    ) -> tast::Expr {
        match e {
            ast::Expr::EVar { name, astptr } => {
                let ty = vars
                    .get(name)
                    .unwrap_or_else(|| panic!("Variable {} not found in environment", name.0));
                tast::Expr::EVar {
                    name: name.0.clone(),
                    ty: ty.clone(),
                    astptr: Some(*astptr),
                }
            }
            ast::Expr::EUnit => tast::Expr::EUnit {
                ty: tast::Ty::TUnit,
            },
            ast::Expr::EBool { value } => tast::Expr::EBool {
                value: *value,
                ty: tast::Ty::TBool,
            },
            ast::Expr::EInt { value } => tast::Expr::EInt {
                value: *value,
                ty: tast::Ty::TInt,
            },
            ast::Expr::EString { value } => tast::Expr::EString {
                value: value.clone(),
                ty: tast::Ty::TString,
            },
            ast::Expr::EConstr { vcon, args } => {
                let (constructor, constr_ty) = env
                    .lookup_constructor(vcon)
                    .unwrap_or_else(|| panic!("Constructor {} not found in environment", vcon.0));

                let expected_arity = match &constructor {
                    tast::Constructor::Enum(enum_constructor) => env
                        .enums
                        .get(&enum_constructor.type_name)
                        .map(|def| def.variants[enum_constructor.index].1.len())
                        .unwrap_or_else(|| {
                            panic!(
                                "Enum {} not found when checking constructor {}",
                                enum_constructor.type_name.0,
                                constructor.name().0
                            )
                        }),
                    tast::Constructor::Struct(struct_constructor) => env
                        .structs
                        .get(&struct_constructor.type_name)
                        .map(|def| def.fields.len())
                        .unwrap_or_else(|| {
                            panic!(
                                "Struct {} not found when checking constructor {}",
                                struct_constructor.type_name.0,
                                constructor.name().0
                            )
                        }),
                };

                if expected_arity != args.len() {
                    panic!(
                        "Constructor {} expects {} arguments, but got {}",
                        constructor.name().0,
                        expected_arity,
                        args.len()
                    );
                }

                let inst_constr_ty = self.inst_ty(&constr_ty);

                let ret_ty = self.fresh_ty_var();
                let mut args_tast = Vec::new();
                for arg in args.iter() {
                    let arg_tast = self.infer(env, vars, arg);
                    args_tast.push(arg_tast.clone());
                }

                if !args_tast.is_empty() {
                    let actual_ty = tast::Ty::TFunc {
                        params: args_tast.iter().map(|arg| arg.get_ty()).collect(),
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    env.constraints
                        .push(Constraint::TypeEqual(inst_constr_ty, actual_ty));
                } else {
                    env.constraints
                        .push(Constraint::TypeEqual(inst_constr_ty, ret_ty.clone()));
                }

                tast::Expr::EConstr {
                    constructor,
                    args: args_tast,
                    ty: ret_ty,
                }
            }
            ast::Expr::EStructLiteral { name, fields } => {
                let (constructor, constr_ty) = env
                    .lookup_constructor(name)
                    .unwrap_or_else(|| panic!("Constructor {} not found in environment", name.0));

                let struct_fields = match &constructor {
                    tast::Constructor::Struct(struct_constructor) => {
                        let type_name = &struct_constructor.type_name;
                        let struct_def = env.structs.get(type_name).unwrap_or_else(|| {
                            panic!(
                                "Struct {} not found when checking literal {}",
                                type_name.0, name.0
                            )
                        });
                        struct_def.fields.clone()
                    }
                    tast::Constructor::Enum { .. } => {
                        panic!(
                            "Constructor {} refers to an enum, but a struct literal was used",
                            name.0
                        )
                    }
                };

                let mut field_positions: HashMap<Lident, usize> = HashMap::new();
                for (idx, (fname, _)) in struct_fields.iter().enumerate() {
                    field_positions.insert(fname.clone(), idx);
                }

                let mut ordered_args: Vec<Option<tast::Expr>> = vec![None; struct_fields.len()];
                for (field_name, expr) in fields.iter() {
                    let idx = field_positions.get(field_name).unwrap_or_else(|| {
                        panic!(
                            "Unknown field {} on struct literal {}",
                            field_name.0, name.0
                        )
                    });
                    if ordered_args[*idx].is_some() {
                        panic!(
                            "Duplicate field {} in struct literal {}",
                            field_name.0, name.0
                        );
                    }
                    let field_expr = self.infer(env, vars, expr);
                    ordered_args[*idx] = Some(field_expr);
                }

                for (idx, slot) in ordered_args.iter().enumerate() {
                    if slot.is_none() {
                        let missing = &struct_fields[idx].0;
                        panic!("Missing field {} in struct literal {}", missing.0, name.0);
                    }
                }

                let args_tast: Vec<tast::Expr> = ordered_args
                    .into_iter()
                    .map(|arg| arg.expect("field checked to exist"))
                    .collect();

                let inst_constr_ty = self.inst_ty(&constr_ty);
                let ret_ty = self.fresh_ty_var();

                if !args_tast.is_empty() {
                    let actual_ty = tast::Ty::TFunc {
                        params: args_tast.iter().map(|arg| arg.get_ty()).collect(),
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    env.constraints
                        .push(Constraint::TypeEqual(inst_constr_ty, actual_ty));
                } else {
                    env.constraints
                        .push(Constraint::TypeEqual(inst_constr_ty, ret_ty.clone()));
                }

                tast::Expr::EConstr {
                    constructor,
                    args: args_tast,
                    ty: ret_ty,
                }
            }
            ast::Expr::ETuple { items } => {
                let mut typs = Vec::new();
                let mut items_tast = Vec::new();
                for item in items.iter() {
                    let item_tast = self.infer(env, vars, item);
                    items_tast.push(item_tast.clone());
                    typs.push(item_tast.get_ty());
                }
                tast::Expr::ETuple {
                    items: items_tast,
                    ty: tast::Ty::TTuple { typs },
                }
            }
            ast::Expr::ELet { pat, value, body } => {
                let value_tast = self.infer(env, vars, value);
                let value_ty = value_tast.get_ty();

                let mut new_vars = vars.clone();
                let pat_tast = self.check_pat(env, &mut new_vars, pat, &value_ty);

                let body_tast = self.infer(env, &new_vars, body);
                let body_ty = body_tast.get_ty();
                tast::Expr::ELet {
                    pat: pat_tast,
                    value: Box::new(value_tast),
                    body: Box::new(body_tast),
                    ty: body_ty.clone(),
                }
            }
            ast::Expr::EMatch { expr, arms } => {
                let expr_tast = self.infer(env, vars, expr);
                let expr_ty = expr_tast.get_ty();

                let mut arms_tast = Vec::new();
                let arm_ty = self.fresh_ty_var();
                for arm in arms.iter() {
                    let mut new_vars = vars.clone();
                    let arm_tast = self.check_pat(env, &mut new_vars, &arm.pat, &expr_ty);
                    let arm_body_tast = self.infer(env, &new_vars, &arm.body);
                    env.constraints.push(Constraint::TypeEqual(
                        arm_body_tast.get_ty(),
                        arm_ty.clone(),
                    ));

                    arms_tast.push(tast::Arm {
                        pat: arm_tast,
                        body: arm_body_tast,
                    });
                }
                tast::Expr::EMatch {
                    expr: Box::new(expr_tast),
                    arms: arms_tast,
                    ty: arm_ty,
                }
            }
            ast::Expr::ECall { func, args } => {
                let f = &func.0;
                match f.as_str() {
                    "unit_to_string" => {
                        if args.len() != 1 {
                            panic!("unit_to_string takes exactly one argument");
                        }
                        let arg0_tast = self.check(env, vars, &args[0], &tast::Ty::TUnit);
                        tast::Expr::ECall {
                            func: func.0.clone(),
                            args: vec![arg0_tast],
                            ty: tast::Ty::TString,
                        }
                    }
                    "bool_to_string" => {
                        if args.len() != 1 {
                            panic!("bool_to_string takes exactly one argument");
                        }
                        let arg0_tast = self.check(env, vars, &args[0], &tast::Ty::TBool);
                        tast::Expr::ECall {
                            func: func.0.clone(),
                            args: vec![arg0_tast],
                            ty: tast::Ty::TString,
                        }
                    }
                    "int_to_string" => {
                        if args.len() != 1 {
                            panic!("int_to_string takes exactly one argument");
                        }
                        let arg0_tast = self.check(env, vars, &args[0], &tast::Ty::TInt);
                        tast::Expr::ECall {
                            func: func.0.clone(),
                            args: vec![arg0_tast],
                            ty: tast::Ty::TString,
                        }
                    }
                    "int_add" => {
                        if args.len() != 2 {
                            panic!("int_add takes exactly two arguments");
                        }
                        let arg0_tast = self.check(env, vars, &args[0], &tast::Ty::TInt);
                        let arg1_tast = self.check(env, vars, &args[1], &tast::Ty::TInt);
                        tast::Expr::ECall {
                            func: func.0.clone(),
                            args: vec![arg0_tast, arg1_tast],
                            ty: tast::Ty::TInt,
                        }
                    }
                    "int_sub" => {
                        if args.len() != 2 {
                            panic!("int_sub takes exactly two arguments");
                        }
                        let arg0_tast = self.check(env, vars, &args[0], &tast::Ty::TInt);
                        let arg1_tast = self.check(env, vars, &args[1], &tast::Ty::TInt);
                        tast::Expr::ECall {
                            func: func.0.clone(),
                            args: vec![arg0_tast, arg1_tast],
                            ty: tast::Ty::TInt,
                        }
                    }
                    "int_less" => {
                        if args.len() != 2 {
                            panic!("int_less takes exactly two arguments");
                        }
                        let arg0_tast = self.check(env, vars, &args[0], &tast::Ty::TInt);
                        let arg1_tast = self.check(env, vars, &args[1], &tast::Ty::TInt);
                        tast::Expr::ECall {
                            func: func.0.clone(),
                            args: vec![arg0_tast, arg1_tast],
                            ty: tast::Ty::TBool,
                        }
                    }
                    "string_add" => {
                        if args.len() != 2 {
                            panic!("string_add takes exactly two arguments");
                        }
                        let arg0_tast = self.check(env, vars, &args[0], &tast::Ty::TString);
                        let arg1_tast = self.check(env, vars, &args[1], &tast::Ty::TString);
                        tast::Expr::ECall {
                            func: func.0.clone(),
                            args: vec![arg0_tast, arg1_tast],
                            ty: tast::Ty::TString,
                        }
                    }
                    "string_print" | "string_println" => {
                        if args.len() != 1 {
                            panic!("string_print/string_println takes exactly one argument");
                        }
                        let arg0_tast = self.check(env, vars, &args[0], &tast::Ty::TString);
                        tast::Expr::ECall {
                            func: func.0.clone(),
                            args: vec![arg0_tast],
                            ty: tast::Ty::TUnit,
                        }
                    }
                    _ => {
                        let is_overloaded =
                            env.overloaded_funcs_to_trait_name.contains_key(&func.0);

                        if !is_overloaded {
                            let func_ty = env.get_type_of_function(&func.0).unwrap_or_else(|| {
                                panic!("Function {} not found in environment", func.0)
                            });
                            let inst_func_ty = self.inst_ty(&func_ty);
                            let ret_ty = self.fresh_ty_var();
                            let mut args_tast = Vec::new();
                            for arg in args.iter() {
                                let arg_tast = self.infer(env, vars, arg);
                                args_tast.push(arg_tast.clone());
                            }

                            env.constraints.push(Constraint::TypeEqual(
                                inst_func_ty.clone(),
                                tast::Ty::TFunc {
                                    params: args_tast.iter().map(|arg| arg.get_ty()).collect(),
                                    ret_ty: Box::new(ret_ty.clone()),
                                },
                            ));

                            tast::Expr::ECall {
                                func: func.0.clone(),
                                args: args_tast,
                                ty: ret_ty,
                            }
                        } else {
                            let trait_name = env
                                .overloaded_funcs_to_trait_name
                                .get(&func.0)
                                .unwrap()
                                .clone();
                            let mut args_tast = Vec::new();
                            let mut arg_types = Vec::new();
                            for arg in args.iter() {
                                let arg_tast = self.infer(env, vars, arg);
                                args_tast.push(arg_tast.clone());
                                arg_types.push(arg_tast.get_ty());
                            }

                            let ret_ty = self.fresh_ty_var();
                            let call_site_func_ty = tast::Ty::TFunc {
                                params: arg_types,
                                ret_ty: Box::new(ret_ty.clone()),
                            };

                            env.constraints.push(Constraint::Overloaded {
                                op: func.clone(),
                                trait_name,
                                call_site_type: call_site_func_ty,
                            });

                            tast::Expr::ECall {
                                func: func.0.clone(),
                                args: args_tast,
                                ty: ret_ty,
                            }
                        }
                    }
                }
            }
        }
    }

    fn check(
        &mut self,
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        e: &ast::Expr,
        ty: &tast::Ty,
    ) -> tast::Expr {
        match e {
            ast::Expr::EVar { name, astptr } => {
                let tast = self.infer(env, vars, e);
                env.constraints
                    .push(Constraint::TypeEqual(tast.get_ty(), ty.clone()));
                tast::Expr::EVar {
                    name: name.0.clone(),
                    ty: tast.get_ty(),
                    astptr: Some(*astptr),
                }
            }
            ast::Expr::EUnit => {
                let tast = self.infer(env, vars, e);
                env.constraints
                    .push(Constraint::TypeEqual(tast.get_ty(), ty.clone()));
                tast
            }
            ast::Expr::EBool { .. } => {
                let tast = self.infer(env, vars, e);
                env.constraints
                    .push(Constraint::TypeEqual(tast.get_ty(), ty.clone()));
                tast
            }
            ast::Expr::EInt { value: _ } => {
                let tast = self.infer(env, vars, e);
                env.constraints
                    .push(Constraint::TypeEqual(tast.get_ty(), ty.clone()));
                tast
            }
            ast::Expr::EString { value: _ } => {
                let tast = self.infer(env, vars, e);
                env.constraints
                    .push(Constraint::TypeEqual(tast.get_ty(), ty.clone()));
                tast
            }
            ast::Expr::EConstr { .. } => {
                let tast = self.infer(env, vars, e);
                env.constraints
                    .push(Constraint::TypeEqual(tast.get_ty(), ty.clone()));
                tast
            }
            ast::Expr::EStructLiteral { .. } => {
                let tast = self.infer(env, vars, e);
                env.constraints
                    .push(Constraint::TypeEqual(tast.get_ty(), ty.clone()));
                tast
            }
            ast::Expr::ETuple { .. } => {
                let tast = self.infer(env, vars, e);
                env.constraints
                    .push(Constraint::TypeEqual(tast.get_ty(), ty.clone()));
                tast
            }
            ast::Expr::ELet { .. } => {
                let tast = self.infer(env, vars, e);
                env.constraints
                    .push(Constraint::TypeEqual(tast.get_ty(), ty.clone()));
                tast
            }
            ast::Expr::EMatch { .. } => {
                let tast = self.infer(env, vars, e);
                env.constraints
                    .push(Constraint::TypeEqual(tast.get_ty(), ty.clone()));
                tast
            }
            ast::Expr::ECall { .. } => {
                let tast = self.infer(env, vars, e);
                env.constraints
                    .push(Constraint::TypeEqual(tast.get_ty(), ty.clone()));
                tast
            }
        }
    }

    fn check_pat(
        &mut self,
        env: &mut Env,
        vars: &mut im::HashMap<Lident, tast::Ty>,
        pat: &ast::Pat,
        ty: &tast::Ty,
    ) -> tast::Pat {
        match pat {
            ast::Pat::PVar { name, astptr } => {
                vars.insert(name.clone(), ty.clone());
                tast::Pat::PVar {
                    name: name.0.clone(),
                    ty: ty.clone(),
                    astptr: Some(*astptr),
                }
            }
            ast::Pat::PUnit => tast::Pat::PUnit {
                ty: tast::Ty::TUnit,
            },
            ast::Pat::PBool { value } => tast::Pat::PBool {
                value: *value,
                ty: tast::Ty::TBool,
            },
            ast::Pat::PInt { value } => {
                env.constraints
                    .push(Constraint::TypeEqual(tast::Ty::TInt, ty.clone()));
                tast::Pat::PInt {
                    value: *value,
                    ty: tast::Ty::TInt,
                }
            }
            ast::Pat::PString { value } => {
                env.constraints
                    .push(Constraint::TypeEqual(tast::Ty::TString, ty.clone()));
                tast::Pat::PString {
                    value: value.clone(),
                    ty: tast::Ty::TString,
                }
            }
            ast::Pat::PConstr { .. } => {
                let tast = self.infer_pat(env, vars, pat);
                env.constraints
                    .push(Constraint::TypeEqual(tast.get_ty(), ty.clone()));
                tast
            }
            ast::Pat::PStruct { .. } => {
                let tast = self.infer_pat(env, vars, pat);
                env.constraints
                    .push(Constraint::TypeEqual(tast.get_ty(), ty.clone()));
                tast
            }
            ast::Pat::PTuple { pats } => {
                let mut pats_tast = Vec::new();
                let mut pat_typs = Vec::new();
                for pat in pats.iter() {
                    let pat_tast = self.infer_pat(env, vars, pat);
                    pat_typs.push(pat_tast.get_ty());
                    pats_tast.push(pat_tast);
                }
                let pat_ty = tast::Ty::TTuple { typs: pat_typs };
                env.constraints
                    .push(Constraint::TypeEqual(pat_ty.clone(), ty.clone()));
                tast::Pat::PTuple {
                    items: pats_tast,
                    ty: pat_ty,
                }
            }
            ast::Pat::PWild => {
                let pat_ty = self.fresh_ty_var();
                env.constraints
                    .push(Constraint::TypeEqual(pat_ty.clone(), ty.clone()));
                tast::Pat::PWild { ty: pat_ty }
            }
        }
    }

    fn infer_pat(
        &mut self,
        env: &mut Env,
        vars: &mut im::HashMap<Lident, tast::Ty>,
        pat: &ast::Pat,
    ) -> tast::Pat {
        match pat {
            ast::Pat::PVar { name, astptr } => {
                let pat_ty = match vars.get(name) {
                    Some(ty) => ty.clone(),
                    None => {
                        let newvar = self.fresh_ty_var();
                        vars.insert(name.clone(), newvar.clone());
                        newvar
                    }
                };
                tast::Pat::PVar {
                    name: name.0.clone(),
                    ty: pat_ty.clone(),
                    astptr: Some(*astptr),
                }
            }
            ast::Pat::PConstr { vcon, args } => {
                let (constructor, constr_ty) = env
                    .lookup_constructor(vcon)
                    .unwrap_or_else(|| panic!("Constructor {} not found in environment", vcon.0));

                let expected_arity = match &constructor {
                    tast::Constructor::Enum(enum_constructor) => env
                        .enums
                        .get(&enum_constructor.type_name)
                        .map(|def| def.variants[enum_constructor.index].1.len())
                        .unwrap_or_else(|| {
                            panic!(
                                "Enum {} not found when checking constructor {}",
                                enum_constructor.type_name.0,
                                constructor.name().0
                            )
                        }),
                    tast::Constructor::Struct(_) => {
                        panic!(
                            "Struct {} patterns must use field syntax",
                            constructor.name().0
                        )
                    }
                };

                if expected_arity != args.len() {
                    panic!(
                        "Constructor {} expects {} arguments, but got {}",
                        constructor.name().0,
                        expected_arity,
                        args.len()
                    );
                }

                let inst_constr_ty = self.inst_ty(&constr_ty);

                let ret_ty = self.fresh_ty_var();
                let mut args_tast = Vec::new();
                let mut args_ty = Vec::new();
                for arg in args.iter() {
                    let arg_tast = self.infer_pat(env, vars, arg);
                    args_ty.push(arg_tast.get_ty());
                    args_tast.push(arg_tast);
                }

                if !args_ty.is_empty() {
                    let actual_ty = tast::Ty::TFunc {
                        params: args_ty,
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    env.constraints
                        .push(Constraint::TypeEqual(inst_constr_ty.clone(), actual_ty));
                } else {
                    env.constraints.push(Constraint::TypeEqual(
                        inst_constr_ty.clone(),
                        ret_ty.clone(),
                    ));
                }

                tast::Pat::PConstr {
                    constructor,
                    args: args_tast,
                    ty: ret_ty,
                }
            }
            ast::Pat::PStruct { name, fields } => {
                let struct_fields = {
                    let struct_def = env.structs.get(name).unwrap_or_else(|| {
                        panic!("Struct {} not found when checking pattern", name.0)
                    });
                    let expected_len = struct_def.fields.len();
                    if expected_len != fields.len() {
                        panic!(
                            "Struct pattern {} expects {} fields, but got {}",
                            name.0,
                            expected_len,
                            fields.len()
                        );
                    }
                    struct_def.fields.clone()
                };

                let mut field_map: HashMap<String, &ast::Pat> = HashMap::new();
                for (fname, pat) in fields.iter() {
                    if field_map.insert(fname.0.clone(), pat).is_some() {
                        panic!("Struct pattern {} has duplicate field {}", name.0, fname.0);
                    }
                }

                let (constructor, constr_ty) = env.lookup_constructor(name).unwrap_or_else(|| {
                    panic!("Struct {} not found when checking constructor", name.0)
                });

                let mut args_tast = Vec::new();
                let mut args_ty = Vec::new();

                for (field_name, _) in struct_fields.iter() {
                    let pat_ast = field_map.remove(&field_name.0).unwrap_or_else(|| {
                        panic!("Struct pattern {} missing field {}", name.0, field_name.0)
                    });
                    let pat_tast = self.infer_pat(env, vars, pat_ast);
                    args_ty.push(pat_tast.get_ty());
                    args_tast.push(pat_tast);
                }

                if !field_map.is_empty() {
                    let extra = field_map.keys().cloned().collect::<Vec<_>>().join(", ");
                    panic!("Struct pattern {} has unknown fields: {}", name.0, extra);
                }

                let inst_constr_ty = self.inst_ty(&constr_ty);
                let ret_ty = self.fresh_ty_var();

                if !args_ty.is_empty() {
                    let actual_ty = tast::Ty::TFunc {
                        params: args_ty,
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    env.constraints
                        .push(Constraint::TypeEqual(inst_constr_ty.clone(), actual_ty));
                } else {
                    env.constraints.push(Constraint::TypeEqual(
                        inst_constr_ty.clone(),
                        ret_ty.clone(),
                    ));
                }

                tast::Pat::PConstr {
                    constructor,
                    args: args_tast,
                    ty: ret_ty,
                }
            }
            ast::Pat::PTuple { pats } => {
                let mut pats_tast = Vec::new();
                let mut pat_typs = Vec::new();
                for pat in pats.iter() {
                    let pat_tast = self.infer_pat(env, vars, pat);
                    pat_typs.push(pat_tast.get_ty());
                    pats_tast.push(pat_tast);
                }
                tast::Pat::PTuple {
                    items: pats_tast,
                    ty: tast::Ty::TTuple { typs: pat_typs },
                }
            }
            ast::Pat::PUnit => tast::Pat::PUnit {
                ty: tast::Ty::TUnit,
            },
            ast::Pat::PBool { value } => tast::Pat::PBool {
                value: *value,
                ty: tast::Ty::TBool,
            },
            ast::Pat::PInt { value } => tast::Pat::PInt {
                value: *value,
                ty: tast::Ty::TInt,
            },
            ast::Pat::PString { value } => tast::Pat::PString {
                value: value.clone(),
                ty: tast::Ty::TString,
            },
            ast::Pat::PWild => {
                let pat_ty = self.fresh_ty_var();
                tast::Pat::PWild { ty: pat_ty }
            }
        }
    }
}
