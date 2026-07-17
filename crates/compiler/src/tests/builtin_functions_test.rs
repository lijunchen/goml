use expect_test::{Expect, expect};

use crate::{builtins, env::GlobalTypeEnv, intrinsics::LangItemId, tast};

fn expect_function_types(env: &GlobalTypeEnv, names: &[&str], expected: Expect) {
    let mut lines = Vec::new();
    for name in names {
        lines.push(format!("{name}: {:?}", env.get_type_of_function(name)));
    }
    expected.assert_eq(&lines.join("\n"));
}

#[test]
fn env_registers_builtin_function_signatures() {
    let env = builtins::builtin_env();

    expect_function_types(
        &env,
        &[
            "string_print",
            "string_byte_slice",
            "int8_to_string",
            "uint8_to_string",
            "ref",
            "ref_get",
            "ref_set",
            "ptr_eq",
            "ptr_hash",
            "slice",
            "slice_get",
            "slice_len",
            "slice_sub",
            "vec_set",
            "range",
        ],
        expect![[r#"
            string_print: Some(TFunc([TString], TUnit))
            string_byte_slice: Some(TFunc([TString, TInt32, TInt32], TString))
            int8_to_string: Some(TFunc([TInt8], TString))
            uint8_to_string: Some(TFunc([TUint8], TString))
            ref: Some(TFunc([TParam(T)], TRef(TParam(T))))
            ref_get: Some(TFunc([TRef(TParam(T))], TParam(T)))
            ref_set: Some(TFunc([TRef(TParam(T)), TParam(T)], TUnit))
            ptr_eq: Some(TFunc([TRef(TParam(T)), TRef(TParam(T))], TBool))
            ptr_hash: Some(TFunc([TRef(TParam(T))], TUint64))
            slice: Some(TFunc([TVec(TParam(T)), TInt32, TInt32], TSlice(TParam(T))))
            slice_get: Some(TFunc([TSlice(TParam(T)), TInt32], TParam(T)))
            slice_len: Some(TFunc([TSlice(TParam(T))], TInt32))
            slice_sub: Some(TFunc([TSlice(TParam(T)), TInt32, TInt32], TSlice(TParam(T))))
            vec_set: Some(TFunc([TVec(TParam(T)), TInt32, TParam(T)], TUnit))
            range: Some(TFunc([TInt32, TInt32], TApp(TStruct(FnIterator), [TInt32])))"#]],
    );
}

#[test]
fn env_does_not_register_legacy_int_aliases() {
    let env = builtins::builtin_env();
    let legacy_symbols = [
        "int_to_string",
        "int_neg",
        "int_add",
        "int_sub",
        "int_mul",
        "int_div",
        "int_less",
    ];

    expect_function_types(
        &env,
        &legacy_symbols,
        expect![[r#"
            int_to_string: None
            int_neg: None
            int_add: None
            int_sub: None
            int_mul: None
            int_div: None
            int_less: None"#]],
    );
}

#[test]
fn env_registers_builtin_int32_inherent_to_string() {
    let env = builtins::builtin_env();
    let method = tast::TastIdent("to_string".to_string());

    let result = env.lookup_inherent_method(&tast::Ty::TInt32, &method);
    expect![[r#"
        Some(
            TFunc([TInt32], TString),
        )
    "#]]
    .assert_debug_eq(&result);
}

#[test]
fn env_registers_builtin_vec_inherent_methods() {
    let env = builtins::builtin_env();
    let receiver = tast::Ty::TVec {
        elem: Box::new(tast::Ty::TInt32),
    };

    let new = env.lookup_inherent_method(&receiver, &tast::TastIdent("new".to_string()));
    expect![[r#"
        Some(
            TFunc([], TVec(TParam(T))),
        )
    "#]]
    .assert_debug_eq(&new);

    let push = env.lookup_inherent_method(&receiver, &tast::TastIdent("push".to_string()));
    expect![[r#"
        Some(
            TFunc([TVec(TParam(T)), TParam(T)], TUnit),
        )
    "#]]
    .assert_debug_eq(&push);

    let get = env.lookup_inherent_method(&receiver, &tast::TastIdent("get".to_string()));
    expect![[r#"
        Some(
            TFunc([TVec(TParam(T)), TInt32], TParam(T)),
        )
    "#]]
    .assert_debug_eq(&get);

    let set = env.lookup_inherent_method(&receiver, &tast::TastIdent("set".to_string()));
    expect![[r#"
        Some(
            TFunc([TVec(TParam(T)), TInt32, TParam(T)], TUnit),
        )
    "#]]
    .assert_debug_eq(&set);

    let len = env.lookup_inherent_method(&receiver, &tast::TastIdent("len".to_string()));
    expect![[r#"
        Some(
            TFunc([TVec(TParam(T))], TInt32),
        )
    "#]]
    .assert_debug_eq(&len);

    let iter = env.lookup_inherent_method(&receiver, &tast::TastIdent("iter".to_string()));
    expect![[r#"
        Some(
            TFunc([TVec(TParam(T))], TApp(TStruct(FnIterator), [TParam(T)])),
        )
    "#]]
    .assert_debug_eq(&iter);
}

#[test]
fn env_registers_builtin_ref_inherent_methods() {
    let env = builtins::builtin_env();
    let receiver = tast::Ty::TRef {
        elem: Box::new(tast::Ty::TInt32),
    };

    let new = env.lookup_inherent_method(&receiver, &tast::TastIdent("new".to_string()));
    expect![[r#"
        Some(
            TFunc([TParam(T)], TRef(TParam(T))),
        )
    "#]]
    .assert_debug_eq(&new);

    let get = env.lookup_inherent_method(&receiver, &tast::TastIdent("get".to_string()));
    expect![[r#"
        Some(
            TFunc([TRef(TParam(T))], TParam(T)),
        )
    "#]]
    .assert_debug_eq(&get);

    let set = env.lookup_inherent_method(&receiver, &tast::TastIdent("set".to_string()));
    expect![[r#"
        Some(
            TFunc([TRef(TParam(T)), TParam(T)], TUnit),
        )
    "#]]
    .assert_debug_eq(&set);
}

#[test]
fn env_registers_builtin_slice_inherent_methods() {
    let env = builtins::builtin_env();
    let receiver = tast::Ty::TSlice {
        elem: Box::new(tast::Ty::TInt32),
    };

    let get = env.lookup_inherent_method(&receiver, &tast::TastIdent("get".to_string()));
    expect![[r#"
        Some(
            TFunc([TSlice(TParam(T)), TInt32], TParam(T)),
        )
    "#]]
    .assert_debug_eq(&get);

    let len = env.lookup_inherent_method(&receiver, &tast::TastIdent("len".to_string()));
    expect![[r#"
        Some(
            TFunc([TSlice(TParam(T))], TInt32),
        )
    "#]]
    .assert_debug_eq(&len);

    let sub = env.lookup_inherent_method(&receiver, &tast::TastIdent("sub".to_string()));
    expect![[r#"
        Some(
            TFunc([TSlice(TParam(T)), TInt32, TInt32], TSlice(TParam(T))),
        )
    "#]]
    .assert_debug_eq(&sub);

    let iter = env.lookup_inherent_method(&receiver, &tast::TastIdent("iter".to_string()));
    expect![[r#"
        Some(
            TFunc([TSlice(TParam(T))], TApp(TStruct(FnIterator), [TParam(T)])),
        )
    "#]]
    .assert_debug_eq(&iter);
}

#[test]
fn env_registers_builtin_iterator_trait_and_fn_iterator_methods() {
    let env = builtins::builtin_env();
    let iterator_name = env.lang_item(LangItemId::Iterator).unwrap().0.clone();
    let iterator_ref = tast::TraitRef::new(tast::TastIdent(iterator_name.clone()), Vec::new());
    let trait_def = env
        .trait_env
        .trait_defs
        .get(&iterator_name)
        .expect("iterator trait exists");
    assert!(trait_def.params.is_empty());
    assert!(trait_def.associated_types.contains_key("Item"));

    let next = env
        .lookup_trait_method_scheme(&iterator_ref, &tast::TastIdent("next".to_string()))
        .expect("iterator next method exists");
    assert_eq!(
        next.ty,
        tast::Ty::TFunc {
            params: vec![tast::Ty::TStruct {
                name: "Self".to_string(),
            }],
            ret_ty: Box::new(tast::Ty::TApp {
                ty: Box::new(tast::Ty::TEnum {
                    name: "Option".to_string(),
                }),
                args: vec![tast::Ty::TProjection {
                    trait_ref: Some(iterator_ref.clone()),
                    for_ty: Box::new(tast::Ty::TStruct {
                        name: "Self".to_string(),
                    }),
                    name: tast::TastIdent("Item".to_string()),
                }],
            }),
        }
    );

    let fn_iterator = tast::Ty::TApp {
        ty: Box::new(tast::Ty::TStruct {
            name: "FnIterator".to_string(),
        }),
        args: vec![tast::Ty::TInt32],
    };
    assert!(
        env.lookup_inherent_method(&fn_iterator, &tast::TastIdent("from_fn".to_string()))
            .is_some()
    );
    assert!(env.trait_env.trait_impls.keys().any(|key| {
        key.trait_ref.name.0 == iterator_name && key.for_ty.get_constr_name_unsafe() == "FnIterator"
    }));

    let into_iterator_name = env.lang_item(LangItemId::IntoIterator).unwrap().0.clone();
    let into_iterator_def = env
        .trait_env
        .trait_defs
        .get(&into_iterator_name)
        .expect("into iterator trait exists");
    assert!(into_iterator_def.associated_types.contains_key("Item"));
    assert!(into_iterator_def.associated_types.contains_key("IntoIter"));
    assert!(env.trait_env.trait_impls.keys().any(|key| {
        key.trait_ref.name.0 == into_iterator_name && matches!(key.for_ty, tast::Ty::TVec { .. })
    }));
    assert!(env.trait_env.trait_impls.keys().any(|key| {
        key.trait_ref.name.0 == into_iterator_name && matches!(key.for_ty, tast::Ty::TSlice { .. })
    }));
}

#[test]
fn env_registers_builtin_string_inherent_methods() {
    let env = builtins::builtin_env();
    let receiver = tast::Ty::TString;

    let len = env.lookup_inherent_method(&receiver, &tast::TastIdent("len".to_string()));
    expect![[r#"
        Some(
            TFunc([TString], TInt32),
        )
    "#]]
    .assert_debug_eq(&len);

    let get = env.lookup_inherent_method(&receiver, &tast::TastIdent("get".to_string()));
    expect![[r#"
        Some(
            TFunc([TString, TInt32], TChar),
        )
    "#]]
    .assert_debug_eq(&get);
}

#[test]
fn builtin_function_names_include_container_and_iterator_builtins() {
    let names = builtins::builtin_function_names();
    assert!(names.iter().any(|n| n == "ref"));
    assert!(names.iter().any(|n| n == "ref_get"));
    assert!(names.iter().any(|n| n == "ref_set"));
    assert!(names.iter().any(|n| n == "ptr_eq"));
    assert!(names.iter().any(|n| n == "ptr_hash"));
    assert!(names.iter().any(|n| n == "slice"));
    assert!(names.iter().any(|n| n == "slice_get"));
    assert!(names.iter().any(|n| n == "slice_len"));
    assert!(names.iter().any(|n| n == "slice_sub"));
    assert!(names.iter().any(|n| n == "vec_set"));
    assert!(names.iter().any(|n| n == "string_byte_slice"));
    assert!(names.iter().any(|n| n == "print"));
    assert!(names.iter().any(|n| n == "println"));
    assert!(names.iter().any(|n| n == "array_get"));
    assert!(names.iter().any(|n| n == "array_set"));
    assert!(names.iter().any(|n| n == "range"));
    assert!(names.iter().any(|n| n == "iterator_map"));
    assert!(names.iter().any(|n| n == "iterator_filter"));
    assert!(names.iter().any(|n| n == "iterator_take"));
    assert!(names.iter().any(|n| n == "iterator_fold"));
    assert!(names.iter().any(|n| n == "iterator_collect"));
}
