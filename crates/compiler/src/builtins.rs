use ast::ast::Lident;
use indexmap::IndexMap;

use crate::{tast, type_encoding::encode_ty};

pub(super) fn builtin_functions() -> IndexMap<String, tast::Ty> {
    let mut funcs = IndexMap::new();

    let make_fn_ty = |params: Vec<tast::Ty>, ret: tast::Ty| tast::Ty::TFunc {
        params,
        ret_ty: Box::new(ret),
    };

    funcs.insert(
        "unit_to_string".to_string(),
        make_fn_ty(vec![tast::Ty::TUnit], tast::Ty::TString),
    );
    funcs.insert(
        "bool_to_string".to_string(),
        make_fn_ty(vec![tast::Ty::TBool], tast::Ty::TString),
    );
    funcs.insert(
        "bool_not".to_string(),
        make_fn_ty(vec![tast::Ty::TBool], tast::Ty::TBool),
    );
    funcs.insert(
        "bool_and".to_string(),
        make_fn_ty(vec![tast::Ty::TBool, tast::Ty::TBool], tast::Ty::TBool),
    );
    funcs.insert(
        "bool_or".to_string(),
        make_fn_ty(vec![tast::Ty::TBool, tast::Ty::TBool], tast::Ty::TBool),
    );

    funcs.insert(
        "int16_to_string".to_string(),
        make_fn_ty(vec![tast::Ty::TInt16], tast::Ty::TString),
    );
    funcs.insert(
        "int16_neg".to_string(),
        make_fn_ty(vec![tast::Ty::TInt16], tast::Ty::TInt16),
    );
    funcs.insert(
        "int16_add".to_string(),
        make_fn_ty(vec![tast::Ty::TInt16, tast::Ty::TInt16], tast::Ty::TInt16),
    );
    funcs.insert(
        "int16_sub".to_string(),
        make_fn_ty(vec![tast::Ty::TInt16, tast::Ty::TInt16], tast::Ty::TInt16),
    );
    funcs.insert(
        "int16_mul".to_string(),
        make_fn_ty(vec![tast::Ty::TInt16, tast::Ty::TInt16], tast::Ty::TInt16),
    );
    funcs.insert(
        "int16_div".to_string(),
        make_fn_ty(vec![tast::Ty::TInt16, tast::Ty::TInt16], tast::Ty::TInt16),
    );
    funcs.insert(
        "int16_less".to_string(),
        make_fn_ty(vec![tast::Ty::TInt16, tast::Ty::TInt16], tast::Ty::TBool),
    );
    funcs.insert(
        "int32_to_string".to_string(),
        make_fn_ty(vec![tast::Ty::TInt32], tast::Ty::TString),
    );
    funcs.insert(
        "int32_neg".to_string(),
        make_fn_ty(vec![tast::Ty::TInt32], tast::Ty::TInt32),
    );
    funcs.insert(
        "int32_add".to_string(),
        make_fn_ty(vec![tast::Ty::TInt32, tast::Ty::TInt32], tast::Ty::TInt32),
    );
    funcs.insert(
        "int32_sub".to_string(),
        make_fn_ty(vec![tast::Ty::TInt32, tast::Ty::TInt32], tast::Ty::TInt32),
    );
    funcs.insert(
        "int32_mul".to_string(),
        make_fn_ty(vec![tast::Ty::TInt32, tast::Ty::TInt32], tast::Ty::TInt32),
    );
    funcs.insert(
        "int32_div".to_string(),
        make_fn_ty(vec![tast::Ty::TInt32, tast::Ty::TInt32], tast::Ty::TInt32),
    );
    funcs.insert(
        "int32_less".to_string(),
        make_fn_ty(vec![tast::Ty::TInt32, tast::Ty::TInt32], tast::Ty::TBool),
    );
    funcs.insert(
        "int64_to_string".to_string(),
        make_fn_ty(vec![tast::Ty::TInt64], tast::Ty::TString),
    );
    funcs.insert(
        "int64_neg".to_string(),
        make_fn_ty(vec![tast::Ty::TInt64], tast::Ty::TInt64),
    );
    funcs.insert(
        "int64_add".to_string(),
        make_fn_ty(vec![tast::Ty::TInt64, tast::Ty::TInt64], tast::Ty::TInt64),
    );
    funcs.insert(
        "int64_sub".to_string(),
        make_fn_ty(vec![tast::Ty::TInt64, tast::Ty::TInt64], tast::Ty::TInt64),
    );
    funcs.insert(
        "int64_mul".to_string(),
        make_fn_ty(vec![tast::Ty::TInt64, tast::Ty::TInt64], tast::Ty::TInt64),
    );
    funcs.insert(
        "int64_div".to_string(),
        make_fn_ty(vec![tast::Ty::TInt64, tast::Ty::TInt64], tast::Ty::TInt64),
    );
    funcs.insert(
        "int64_less".to_string(),
        make_fn_ty(vec![tast::Ty::TInt64, tast::Ty::TInt64], tast::Ty::TBool),
    );
    funcs.insert(
        "int8_to_string".to_string(),
        make_fn_ty(vec![tast::Ty::TInt8], tast::Ty::TString),
    );
    funcs.insert(
        "int8_neg".to_string(),
        make_fn_ty(vec![tast::Ty::TInt8], tast::Ty::TInt8),
    );
    funcs.insert(
        "int8_add".to_string(),
        make_fn_ty(vec![tast::Ty::TInt8, tast::Ty::TInt8], tast::Ty::TInt8),
    );
    funcs.insert(
        "int8_sub".to_string(),
        make_fn_ty(vec![tast::Ty::TInt8, tast::Ty::TInt8], tast::Ty::TInt8),
    );
    funcs.insert(
        "int8_mul".to_string(),
        make_fn_ty(vec![tast::Ty::TInt8, tast::Ty::TInt8], tast::Ty::TInt8),
    );
    funcs.insert(
        "int8_div".to_string(),
        make_fn_ty(vec![tast::Ty::TInt8, tast::Ty::TInt8], tast::Ty::TInt8),
    );
    funcs.insert(
        "int8_less".to_string(),
        make_fn_ty(vec![tast::Ty::TInt8, tast::Ty::TInt8], tast::Ty::TBool),
    );
    funcs.insert(
        "uint8_to_string".to_string(),
        make_fn_ty(vec![tast::Ty::TUint8], tast::Ty::TString),
    );
    funcs.insert(
        "uint8_neg".to_string(),
        make_fn_ty(vec![tast::Ty::TUint8], tast::Ty::TUint8),
    );
    funcs.insert(
        "uint8_add".to_string(),
        make_fn_ty(vec![tast::Ty::TUint8, tast::Ty::TUint8], tast::Ty::TUint8),
    );
    funcs.insert(
        "uint8_sub".to_string(),
        make_fn_ty(vec![tast::Ty::TUint8, tast::Ty::TUint8], tast::Ty::TUint8),
    );
    funcs.insert(
        "uint8_mul".to_string(),
        make_fn_ty(vec![tast::Ty::TUint8, tast::Ty::TUint8], tast::Ty::TUint8),
    );
    funcs.insert(
        "uint8_div".to_string(),
        make_fn_ty(vec![tast::Ty::TUint8, tast::Ty::TUint8], tast::Ty::TUint8),
    );
    funcs.insert(
        "uint8_less".to_string(),
        make_fn_ty(vec![tast::Ty::TUint8, tast::Ty::TUint8], tast::Ty::TBool),
    );
    funcs.insert(
        "uint16_to_string".to_string(),
        make_fn_ty(vec![tast::Ty::TUint16], tast::Ty::TString),
    );
    funcs.insert(
        "uint16_neg".to_string(),
        make_fn_ty(vec![tast::Ty::TUint16], tast::Ty::TUint16),
    );
    funcs.insert(
        "uint16_add".to_string(),
        make_fn_ty(
            vec![tast::Ty::TUint16, tast::Ty::TUint16],
            tast::Ty::TUint16,
        ),
    );
    funcs.insert(
        "uint16_sub".to_string(),
        make_fn_ty(
            vec![tast::Ty::TUint16, tast::Ty::TUint16],
            tast::Ty::TUint16,
        ),
    );
    funcs.insert(
        "uint16_mul".to_string(),
        make_fn_ty(
            vec![tast::Ty::TUint16, tast::Ty::TUint16],
            tast::Ty::TUint16,
        ),
    );
    funcs.insert(
        "uint16_div".to_string(),
        make_fn_ty(
            vec![tast::Ty::TUint16, tast::Ty::TUint16],
            tast::Ty::TUint16,
        ),
    );
    funcs.insert(
        "uint16_less".to_string(),
        make_fn_ty(vec![tast::Ty::TUint16, tast::Ty::TUint16], tast::Ty::TBool),
    );
    funcs.insert(
        "uint32_to_string".to_string(),
        make_fn_ty(vec![tast::Ty::TUint32], tast::Ty::TString),
    );
    funcs.insert(
        "uint32_neg".to_string(),
        make_fn_ty(vec![tast::Ty::TUint32], tast::Ty::TUint32),
    );
    funcs.insert(
        "uint32_add".to_string(),
        make_fn_ty(
            vec![tast::Ty::TUint32, tast::Ty::TUint32],
            tast::Ty::TUint32,
        ),
    );
    funcs.insert(
        "uint32_sub".to_string(),
        make_fn_ty(
            vec![tast::Ty::TUint32, tast::Ty::TUint32],
            tast::Ty::TUint32,
        ),
    );
    funcs.insert(
        "uint32_mul".to_string(),
        make_fn_ty(
            vec![tast::Ty::TUint32, tast::Ty::TUint32],
            tast::Ty::TUint32,
        ),
    );
    funcs.insert(
        "uint32_div".to_string(),
        make_fn_ty(
            vec![tast::Ty::TUint32, tast::Ty::TUint32],
            tast::Ty::TUint32,
        ),
    );
    funcs.insert(
        "uint32_less".to_string(),
        make_fn_ty(vec![tast::Ty::TUint32, tast::Ty::TUint32], tast::Ty::TBool),
    );
    funcs.insert(
        "uint64_to_string".to_string(),
        make_fn_ty(vec![tast::Ty::TUint64], tast::Ty::TString),
    );
    funcs.insert(
        "uint64_neg".to_string(),
        make_fn_ty(vec![tast::Ty::TUint64], tast::Ty::TUint64),
    );
    funcs.insert(
        "float32_to_string".to_string(),
        make_fn_ty(vec![tast::Ty::TFloat32], tast::Ty::TString),
    );
    funcs.insert(
        "float32_neg".to_string(),
        make_fn_ty(vec![tast::Ty::TFloat32], tast::Ty::TFloat32),
    );
    funcs.insert(
        "float32_add".to_string(),
        make_fn_ty(
            vec![tast::Ty::TFloat32, tast::Ty::TFloat32],
            tast::Ty::TFloat32,
        ),
    );
    funcs.insert(
        "float32_sub".to_string(),
        make_fn_ty(
            vec![tast::Ty::TFloat32, tast::Ty::TFloat32],
            tast::Ty::TFloat32,
        ),
    );
    funcs.insert(
        "float32_mul".to_string(),
        make_fn_ty(
            vec![tast::Ty::TFloat32, tast::Ty::TFloat32],
            tast::Ty::TFloat32,
        ),
    );
    funcs.insert(
        "float32_div".to_string(),
        make_fn_ty(
            vec![tast::Ty::TFloat32, tast::Ty::TFloat32],
            tast::Ty::TFloat32,
        ),
    );
    funcs.insert(
        "float32_less".to_string(),
        make_fn_ty(
            vec![tast::Ty::TFloat32, tast::Ty::TFloat32],
            tast::Ty::TBool,
        ),
    );
    funcs.insert(
        "float64_to_string".to_string(),
        make_fn_ty(vec![tast::Ty::TFloat64], tast::Ty::TString),
    );
    funcs.insert(
        "float64_neg".to_string(),
        make_fn_ty(vec![tast::Ty::TFloat64], tast::Ty::TFloat64),
    );
    funcs.insert(
        "float64_add".to_string(),
        make_fn_ty(
            vec![tast::Ty::TFloat64, tast::Ty::TFloat64],
            tast::Ty::TFloat64,
        ),
    );
    funcs.insert(
        "float64_sub".to_string(),
        make_fn_ty(
            vec![tast::Ty::TFloat64, tast::Ty::TFloat64],
            tast::Ty::TFloat64,
        ),
    );
    funcs.insert(
        "float64_mul".to_string(),
        make_fn_ty(
            vec![tast::Ty::TFloat64, tast::Ty::TFloat64],
            tast::Ty::TFloat64,
        ),
    );
    funcs.insert(
        "float64_div".to_string(),
        make_fn_ty(
            vec![tast::Ty::TFloat64, tast::Ty::TFloat64],
            tast::Ty::TFloat64,
        ),
    );
    funcs.insert(
        "float64_less".to_string(),
        make_fn_ty(
            vec![tast::Ty::TFloat64, tast::Ty::TFloat64],
            tast::Ty::TBool,
        ),
    );
    funcs.insert(
        "uint64_add".to_string(),
        make_fn_ty(
            vec![tast::Ty::TUint64, tast::Ty::TUint64],
            tast::Ty::TUint64,
        ),
    );
    funcs.insert(
        "uint64_sub".to_string(),
        make_fn_ty(
            vec![tast::Ty::TUint64, tast::Ty::TUint64],
            tast::Ty::TUint64,
        ),
    );
    funcs.insert(
        "uint64_mul".to_string(),
        make_fn_ty(
            vec![tast::Ty::TUint64, tast::Ty::TUint64],
            tast::Ty::TUint64,
        ),
    );
    funcs.insert(
        "uint64_div".to_string(),
        make_fn_ty(
            vec![tast::Ty::TUint64, tast::Ty::TUint64],
            tast::Ty::TUint64,
        ),
    );
    funcs.insert(
        "uint64_less".to_string(),
        make_fn_ty(vec![tast::Ty::TUint64, tast::Ty::TUint64], tast::Ty::TBool),
    );
    funcs.insert(
        "string_add".to_string(),
        make_fn_ty(
            vec![tast::Ty::TString, tast::Ty::TString],
            tast::Ty::TString,
        ),
    );
    funcs.insert(
        "string_print".to_string(),
        make_fn_ty(vec![tast::Ty::TString], tast::Ty::TUnit),
    );
    funcs.insert(
        "string_println".to_string(),
        make_fn_ty(vec![tast::Ty::TString], tast::Ty::TUnit),
    );

    let array_elem_param = tast::Ty::TParam {
        name: "T".to_string(),
    };
    let array_ty = tast::Ty::TArray {
        len: tast::ARRAY_WILDCARD_LEN,
        elem: Box::new(array_elem_param.clone()),
    };
    funcs.insert(
        "array_get".to_string(),
        make_fn_ty(
            vec![array_ty.clone(), tast::Ty::TInt32],
            array_elem_param.clone(),
        ),
    );
    funcs.insert(
        "array_set".to_string(),
        make_fn_ty(
            vec![array_ty.clone(), tast::Ty::TInt32, array_elem_param.clone()],
            array_ty,
        ),
    );

    let ref_elem_param = tast::Ty::TParam {
        name: "T".to_string(),
    };
    let ref_ty = tast::Ty::TRef {
        elem: Box::new(ref_elem_param.clone()),
    };
    funcs.insert(
        "ref".to_string(),
        make_fn_ty(
            vec![ref_elem_param.clone()],
            tast::Ty::TRef {
                elem: Box::new(ref_elem_param.clone()),
            },
        ),
    );
    funcs.insert(
        "ref_get".to_string(),
        make_fn_ty(vec![ref_ty.clone()], ref_elem_param.clone()),
    );
    funcs.insert(
        "ref_set".to_string(),
        make_fn_ty(
            vec![ref_ty.clone(), ref_elem_param.clone()],
            tast::Ty::TUnit,
        ),
    );

    funcs.insert(
        "spawn".to_string(),
        make_fn_ty(
            vec![tast::Ty::TFunc {
                params: Vec::new(),
                ret_ty: Box::new(tast::Ty::TUnit),
            }],
            tast::Ty::TUnit,
        ),
    );

    funcs
}

pub(super) fn builtin_inherent_methods() -> IndexMap<(String, Lident), (String, tast::Ty)> {
    let mut methods = IndexMap::new();

    let int32_ty = tast::Ty::TInt32;
    let method_name = Lident("to_string".to_string());
    let method_ty = tast::Ty::TFunc {
        params: vec![int32_ty.clone()],
        ret_ty: Box::new(tast::Ty::TString),
    };

    methods.insert(
        (encode_ty(&int32_ty), method_name.clone()),
        ("int32_to_string".to_string(), method_ty),
    );

    methods
}

pub fn builtin_function_names() -> Vec<String> {
    builtin_functions().into_keys().collect()
}
