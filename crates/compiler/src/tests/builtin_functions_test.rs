use ast::ast::Ident;

use crate::{env::GlobalTypeEnv, tast};

#[test]
fn env_registers_builtin_function_signatures() {
    let env = GlobalTypeEnv::new();

    match env.get_type_of_function("string_print") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 1);
            assert!(matches!(params[0], tast::Ty::TString));
            assert!(matches!(ret_ty.as_ref(), tast::Ty::TUnit));
        }
        other => panic!(
            "expected string_print to have a function type signature, got {:?}",
            other
        ),
    }

    match env.get_type_of_function("int8_to_string") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 1);
            assert!(matches!(params[0], tast::Ty::TInt8));
            assert!(matches!(ret_ty.as_ref(), tast::Ty::TString));
        }
        other => panic!(
            "expected int8_to_string to have a function type signature, got {:?}",
            other
        ),
    }

    match env.get_type_of_function("uint8_to_string") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 1);
            assert!(matches!(params[0], tast::Ty::TUint8));
            assert!(matches!(ret_ty.as_ref(), tast::Ty::TString));
        }
        other => panic!(
            "expected uint8_to_string to have a function type signature, got {:?}",
            other
        ),
    }

    // Binary and unary operations are now handled as BinaryExpr and UnaryExpr nodes,
    // not builtin functions. This includes:
    // - Arithmetic: add, sub, mul, div
    // - Logical: and, or
    // - Comparison: less
    // - Unary: neg, not
    //
    // These operations are no longer registered as builtin functions.

    match env.get_type_of_function("ref") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 1);
            match &params[0] {
                tast::Ty::TParam { name } => assert_eq!(name, "T"),
                other => panic!("expected ref param to be type param, got {:?}", other),
            }
            match ret_ty.as_ref() {
                tast::Ty::TRef { elem } => match elem.as_ref() {
                    tast::Ty::TParam { name } => assert_eq!(name, "T"),
                    other => panic!(
                        "expected ref return element to be type param, got {:?}",
                        other
                    ),
                },
                other => panic!("expected ref to return a reference type, got {:?}", other),
            }
        }
        other => panic!(
            "expected ref to have a function type signature, got {:?}",
            other
        ),
    }

    match env.get_type_of_function("ref_get") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 1);
            match &params[0] {
                tast::Ty::TRef { elem } => match elem.as_ref() {
                    tast::Ty::TParam { name } => assert_eq!(name, "T"),
                    other => panic!(
                        "expected ref_get reference element to be type param, got {:?}",
                        other
                    ),
                },
                other => panic!("expected ref_get param to be reference, got {:?}", other),
            }
            match ret_ty.as_ref() {
                tast::Ty::TParam { name } => assert_eq!(name, "T"),
                other => panic!("expected ref_get to return type param, got {:?}", other),
            }
        }
        other => panic!(
            "expected ref_get to have a function type signature, got {:?}",
            other
        ),
    }

    match env.get_type_of_function("ref_set") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 2);
            match &params[0] {
                tast::Ty::TRef { elem } => match elem.as_ref() {
                    tast::Ty::TParam { name } => assert_eq!(name, "T"),
                    other => panic!(
                        "expected ref_set reference element to be type param, got {:?}",
                        other
                    ),
                },
                other => panic!(
                    "expected ref_set first param to be reference, got {:?}",
                    other
                ),
            }
            match &params[1] {
                tast::Ty::TParam { name } => assert_eq!(name, "T"),
                other => panic!(
                    "expected ref_set second param to be type param, got {:?}",
                    other
                ),
            }
            assert!(matches!(ret_ty.as_ref(), tast::Ty::TUnit));
        }
        other => panic!(
            "expected ref_set to have a function type signature, got {:?}",
            other
        ),
    }
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

    for symbol in legacy_symbols {
        assert!(
            env.get_type_of_function(symbol).is_none(),
            "legacy builtin `{}` should not be registered",
            symbol,
        );
    }
}

#[test]
fn env_registers_builtin_int32_inherent_to_string() {
    let env = GlobalTypeEnv::new();
    let method = Ident("to_string".to_string());

    match env.lookup_inherent_method(&tast::Ty::TInt32, &method) {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 1);
            assert!(matches!(params[0], tast::Ty::TInt32));
            assert!(matches!(ret_ty.as_ref(), tast::Ty::TString));
        }
        other => panic!(
            "expected builtin int32.to_string to return TFunc, got {:?}",
            other
        ),
    }
}
