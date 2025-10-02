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
}
