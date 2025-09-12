#[derive(Debug, Clone)]
pub enum GoType {
    TVoid,
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
}
