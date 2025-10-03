use crate::tast::Ty;

pub fn encode_ty(ty: &Ty) -> String {
    match ty {
        Ty::TUnit => "unit".to_string(),
        Ty::TBool => "bool".to_string(),
        Ty::TInt => "int".to_string(),
        Ty::TString => "string".to_string(),
        Ty::TVar(_v) => "Var".to_string(),
        Ty::TParam { name } => format!("TParam_{}", name),
        Ty::TTuple { typs } => {
            let inner = typs.iter().map(encode_ty).collect::<Vec<_>>().join("_");
            format!("Tuple_{}", inner)
        }
        Ty::TCon { name } => name.clone(),
        Ty::TApp { ty, args } => {
            let base = ty.get_constr_name_unsafe();
            if args.is_empty() {
                base
            } else {
                let inner = args.iter().map(encode_ty).collect::<Vec<_>>().join("_");
                format!("{}_{}", base, inner)
            }
        }
        Ty::TFunc { params, ret_ty } => {
            let p = params.iter().map(encode_ty).collect::<Vec<_>>().join("_");
            let r = encode_ty(ret_ty);
            format!("Fn_{}_to_{}", p, r)
        }
    }
}
