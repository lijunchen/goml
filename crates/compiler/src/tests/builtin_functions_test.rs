use expect_test::{Expect, expect};

use crate::{env::GlobalTypeEnv, tast};

fn expect_function_types(env: &GlobalTypeEnv, names: &[&str], expected: Expect) {
    let mut lines = Vec::new();
    for name in names {
        lines.push(format!("{name}: {:?}", env.get_type_of_function(name)));
    }
    expected.assert_eq(&lines.join("\n"));
}

#[test]
fn env_registers_builtin_function_signatures() {
    let env = GlobalTypeEnv::new();

    expect_function_types(
        &env,
        &[
            "string_print",
            "int8_to_string",
            "uint8_to_string",
            "ref",
            "ref_get",
            "ref_set",
        ],
        expect![[r#"
            string_print: Some(TFunc([TString], TUnit))
            int8_to_string: Some(TFunc([TInt8], TString))
            uint8_to_string: Some(TFunc([TUint8], TString))
            ref: Some(TFunc([TParam(T)], TRef(TParam(T))))
            ref_get: Some(TFunc([TRef(TParam(T))], TParam(T)))
            ref_set: Some(TFunc([TRef(TParam(T)), TParam(T)], TUnit))"#]],
    );
}

#[test]
fn env_does_not_register_legacy_int_aliases() {
    let env = GlobalTypeEnv::new();
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
    let env = GlobalTypeEnv::new();
    let method = tast::Ident("to_string".to_string());

    let result = env.lookup_inherent_method(&tast::Ty::TInt32, &method);
    expect![[r#"
        Some(
            TFunc([TInt32], TString),
        )
    "#]]
    .assert_debug_eq(&result);
}
