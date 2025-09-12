#[derive(Debug)]
pub enum Ty {
    TBool,
    TInt,
    TString,
    TStruct {
        name: String,
        fields: Vec<(String, Ty)>,
    },
    TFunc {
        params: Vec<Ty>,
        ret_ty: Box<Ty>,
    },
}
