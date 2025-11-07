#[derive(Debug, Clone)]
pub enum GoType {
    TVoid,
    TUnit, // struct{}, it has only one value: struct{}{}
    TBool,
    TInt,
    TInt8,
    TInt16,
    TInt32,
    TInt64,
    TUint8,
    TUint16,
    TUint32,
    TUint64,
    TFloat32,
    TFloat64,
    TString,
    TStruct {
        name: String,
        fields: Vec<(String, GoType)>,
    },
    TPointer {
        elem: Box<GoType>,
    },
    TFunc {
        params: Vec<GoType>,
        ret_ty: Box<GoType>,
    },
    TName {
        name: String,
    },
    TArray {
        len: usize,
        elem: Box<GoType>,
    },
    TSlice {
        elem: Box<GoType>,
    },
}
