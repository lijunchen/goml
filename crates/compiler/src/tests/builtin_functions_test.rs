use crate::{env::Env, tast};

#[test]
fn env_registers_builtin_function_signatures() {
    let env = Env::new();

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

    match env.get_type_of_function("int_add") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 2);
            assert!(matches!(params[0], tast::Ty::TInt));
            assert!(matches!(params[1], tast::Ty::TInt));
            assert!(matches!(ret_ty.as_ref(), tast::Ty::TInt));
        }
        other => panic!(
            "expected int_add to have a function type signature, got {:?}",
            other
        ),
    }

    match env.get_type_of_function("int8_add") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 2);
            assert!(matches!(params[0], tast::Ty::TInt8));
            assert!(matches!(params[1], tast::Ty::TInt8));
            assert!(matches!(ret_ty.as_ref(), tast::Ty::TInt8));
        }
        other => panic!(
            "expected int8_add to have a function type signature, got {:?}",
            other
        ),
    }

    match env.get_type_of_function("uint8_add") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 2);
            assert!(matches!(params[0], tast::Ty::TUint8));
            assert!(matches!(params[1], tast::Ty::TUint8));
            assert!(matches!(ret_ty.as_ref(), tast::Ty::TUint8));
        }
        other => panic!(
            "expected uint8_add to have a function type signature, got {:?}",
            other
        ),
    }

    match env.get_type_of_function("int16_add") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 2);
            assert!(matches!(params[0], tast::Ty::TInt16));
            assert!(matches!(params[1], tast::Ty::TInt16));
            assert!(matches!(ret_ty.as_ref(), tast::Ty::TInt16));
        }
        other => panic!(
            "expected int16_add to have a function type signature, got {:?}",
            other
        ),
    }

    match env.get_type_of_function("uint16_add") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 2);
            assert!(matches!(params[0], tast::Ty::TUint16));
            assert!(matches!(params[1], tast::Ty::TUint16));
            assert!(matches!(ret_ty.as_ref(), tast::Ty::TUint16));
        }
        other => panic!(
            "expected uint16_add to have a function type signature, got {:?}",
            other
        ),
    }

    match env.get_type_of_function("int32_add") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 2);
            assert!(matches!(params[0], tast::Ty::TInt32));
            assert!(matches!(params[1], tast::Ty::TInt32));
            assert!(matches!(ret_ty.as_ref(), tast::Ty::TInt32));
        }
        other => panic!(
            "expected int32_add to have a function type signature, got {:?}",
            other
        ),
    }

    match env.get_type_of_function("uint32_add") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 2);
            assert!(matches!(params[0], tast::Ty::TUint32));
            assert!(matches!(params[1], tast::Ty::TUint32));
            assert!(matches!(ret_ty.as_ref(), tast::Ty::TUint32));
        }
        other => panic!(
            "expected uint32_add to have a function type signature, got {:?}",
            other
        ),
    }

    match env.get_type_of_function("int64_add") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 2);
            assert!(matches!(params[0], tast::Ty::TInt64));
            assert!(matches!(params[1], tast::Ty::TInt64));
            assert!(matches!(ret_ty.as_ref(), tast::Ty::TInt64));
        }
        other => panic!(
            "expected int64_add to have a function type signature, got {:?}",
            other
        ),
    }

    match env.get_type_of_function("uint64_add") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 2);
            assert!(matches!(params[0], tast::Ty::TUint64));
            assert!(matches!(params[1], tast::Ty::TUint64));
            assert!(matches!(ret_ty.as_ref(), tast::Ty::TUint64));
        }
        other => panic!(
            "expected uint64_add to have a function type signature, got {:?}",
            other
        ),
    }

    match env.get_type_of_function("bool_not") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 1);
            assert!(matches!(params[0], tast::Ty::TBool));
            assert!(matches!(ret_ty.as_ref(), tast::Ty::TBool));
        }
        other => panic!(
            "expected bool_not to have a function type signature, got {:?}",
            other
        ),
    }

    match env.get_type_of_function("bool_and") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 2);
            assert!(matches!(params[0], tast::Ty::TBool));
            assert!(matches!(params[1], tast::Ty::TBool));
            assert!(matches!(ret_ty.as_ref(), tast::Ty::TBool));
        }
        other => panic!(
            "expected bool_and to have a function type signature, got {:?}",
            other
        ),
    }

    match env.get_type_of_function("bool_or") {
        Some(tast::Ty::TFunc { params, ret_ty }) => {
            assert_eq!(params.len(), 2);
            assert!(matches!(params[0], tast::Ty::TBool));
            assert!(matches!(params[1], tast::Ty::TBool));
            assert!(matches!(ret_ty.as_ref(), tast::Ty::TBool));
        }
        other => panic!(
            "expected bool_or to have a function type signature, got {:?}",
            other
        ),
    }

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
