use std::collections::{HashMap, HashSet};

use crate::go::{goast, runtime};
use crate::intrinsics::{
    CallEffect, CallableBody, ExternCapability, IntrinsicId, RuntimeHookId,
    validate_callable_signature,
};
use crate::tast::Ty;

#[test]
fn callable_ids_have_unique_round_trip_keys() {
    let mut keys = HashSet::new();
    let mut intrinsic_source_names = HashSet::new();
    for id in IntrinsicId::ALL {
        assert!(keys.insert(id.key()));
        assert!(intrinsic_source_names.insert(id.source_name()));
        assert_eq!(IntrinsicId::from_key(id.key()), Some(id));
    }
    let mut runtime_go_names = HashSet::new();
    for id in RuntimeHookId::ALL {
        assert!(keys.insert(id.key()));
        assert!(runtime_go_names.insert(runtime::runtime_hook_fn_name(id)));
        assert_eq!(RuntimeHookId::from_key(id.key()), Some(id));
    }
}

#[test]
fn callable_catalog_signatures_validate_themselves() {
    for body in IntrinsicId::ALL
        .into_iter()
        .map(CallableBody::Intrinsic)
        .chain(RuntimeHookId::ALL.into_iter().map(CallableBody::Runtime))
    {
        let signature = body.signature().unwrap();
        assert!(
            validate_callable_signature(
                body,
                &signature.type_params,
                &signature.constraints,
                &signature.ty,
            )
            .is_ok()
        );
    }
}

#[test]
fn builtin_contract_declares_every_core_callable_once() {
    let callables = crate::builtins::builtin_callables();
    for id in IntrinsicId::ALL {
        let expected = usize::from(id != IntrinsicId::Missing);
        let count = callables
            .values()
            .filter(|body| **body == CallableBody::Intrinsic(id))
            .count();
        assert_eq!(count, expected, "{} declarations", id.key());
    }
    for id in RuntimeHookId::ALL.into_iter().filter(|id| id.is_core()) {
        let count = callables
            .values()
            .filter(|body| **body == CallableBody::Runtime(id))
            .count();
        assert_eq!(count, 1, "{} declarations", id.key());
    }
}

#[test]
fn callable_catalog_rejects_signature_drift() {
    let body = CallableBody::Intrinsic(IntrinsicId::VecPush);
    let signature = body.signature().unwrap();
    let Ty::TFunc { params, .. } = signature.ty else {
        panic!("expected function type");
    };
    let wrong_ty = Ty::TFunc {
        params,
        ret_ty: Box::new(Ty::TVec {
            elem: Box::new(Ty::TParam {
                name: "T".to_string(),
            }),
        }),
    };

    let error = validate_callable_signature(
        body,
        &signature.type_params,
        &signature.constraints,
        &wrong_ty,
    )
    .unwrap_err();
    assert!(error.contains("intrinsic vec.push has signature"));
}

#[test]
fn extern_capabilities_are_partitioned() {
    let core_runtime = CallableBody::Runtime(RuntimeHookId::Int32ToString);
    let std_runtime = CallableBody::Runtime(RuntimeHookId::StdEnvArgs);
    let intrinsic = CallableBody::Intrinsic(IntrinsicId::VecPush);

    assert!(ExternCapability::Core.permits(core_runtime));
    assert!(ExternCapability::Core.permits(intrinsic));
    assert!(!ExternCapability::Core.permits(std_runtime));
    assert!(ExternCapability::StandardLibrary.permits(std_runtime));
    assert!(!ExternCapability::StandardLibrary.permits(core_runtime));
    assert!(!ExternCapability::StandardLibrary.permits(intrinsic));
    assert!(!ExternCapability::None.permits(core_runtime));
    assert!(!ExternCapability::None.permits(std_runtime));
    assert!(!ExternCapability::None.permits(intrinsic));
}

#[test]
fn callable_effects_describe_mutation_and_host_calls() {
    assert_eq!(IntrinsicId::ArraySet.effect(), CallEffect::Pure);
    assert_eq!(
        IntrinsicId::VecPush.effect(),
        CallEffect::MutatesArgument(0)
    );
    assert_eq!(RuntimeHookId::StdIoPrint.effect(), CallEffect::Host);
    assert_eq!(RuntimeHookId::StdProcessExit.effect(), CallEffect::Diverges);
}

#[test]
fn every_runtime_hook_has_a_go_implementation() {
    let functions = runtime::make_runtime()
        .into_iter()
        .filter_map(|item| match item {
            goast::Item::Fn(function) => Some((function.name.clone(), function)),
            _ => None,
        })
        .collect::<HashMap<_, _>>();

    for id in RuntimeHookId::ALL {
        let function = functions
            .get(&runtime::runtime_hook_fn_name(id))
            .unwrap_or_else(|| panic!("missing runtime implementation for {}", id.key()));
        let signature = id.signature();
        let Ty::TFunc { params, ret_ty } = signature.ty else {
            panic!("runtime hook must have function type");
        };
        let expected_params = params
            .iter()
            .map(goast::tast_ty_to_go_type)
            .collect::<Vec<_>>();
        let actual_params = function
            .params
            .iter()
            .map(|(_, ty)| ty.clone())
            .collect::<Vec<_>>();
        assert_eq!(actual_params, expected_params, "{} parameters", id.key());
        assert_eq!(
            function.ret_ty,
            Some(goast::tast_ty_to_go_type(ret_ty.as_ref())),
            "{} return type",
            id.key()
        );
    }
}
