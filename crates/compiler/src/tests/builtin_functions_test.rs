use expect_test::{Expect, expect};

use crate::{builtins, env::GlobalTypeEnv, tast};

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
            "int8_to_string",
            "uint8_to_string",
            "ref",
            "ref_get",
            "ref_set",
            "ptr_eq",
            "slice",
            "slice_get",
            "slice_len",
            "slice_sub",
            "vec_set",
        ],
        expect![[r#"
            string_print: Some(TFunc([TString], TUnit))
            int8_to_string: Some(TFunc([TInt8], TString))
            uint8_to_string: Some(TFunc([TUint8], TString))
            ref: Some(TFunc([TParam(T)], TRef(TParam(T))))
            ref_get: Some(TFunc([TRef(TParam(T))], TParam(T)))
            ref_set: Some(TFunc([TRef(TParam(T)), TParam(T)], TUnit))
            ptr_eq: Some(TFunc([TRef(TParam(T)), TRef(TParam(T))], TBool))
            slice: Some(TFunc([TVec(TParam(T)), TInt32, TInt32], TSlice(TParam(T))))
            slice_get: Some(TFunc([TSlice(TParam(T)), TInt32], TParam(T)))
            slice_len: Some(TFunc([TSlice(TParam(T))], TInt32))
            slice_sub: Some(TFunc([TSlice(TParam(T)), TInt32, TInt32], TSlice(TParam(T))))
            vec_set: Some(TFunc([TVec(TParam(T)), TInt32, TParam(T)], TUnit))"#]],
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
            TFunc([TVec(TParam(T)), TParam(T)], TVec(TParam(T))),
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
fn builtin_function_names_include_ref_builtins() {
    let names = builtins::builtin_function_names();
    assert!(names.iter().any(|n| n == "ref"));
    assert!(names.iter().any(|n| n == "ref_get"));
    assert!(names.iter().any(|n| n == "ref_set"));
    assert!(names.iter().any(|n| n == "ptr_eq"));
    assert!(names.iter().any(|n| n == "slice"));
    assert!(names.iter().any(|n| n == "slice_get"));
    assert!(names.iter().any(|n| n == "slice_len"));
    assert!(names.iter().any(|n| n == "slice_sub"));
    assert!(names.iter().any(|n| n == "vec_set"));
    assert!(names.iter().any(|n| n == "array_get"));
    assert!(names.iter().any(|n| n == "array_set"));
}
