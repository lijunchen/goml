#[derive(Debug, Clone)]
pub enum GoType {
    TVoid,
    TUnit, // struct{}, it has only one value: struct{}{}
    TBool,
    TInt,
    TString,
    TStruct {
        name: String,
        fields: Vec<(String, GoType)>,
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
}
