use crate::tast;
use ::ast::ast;

pub fn mangle_impl_name(trait_name: &ast::Uident, for_ty: &tast::Ty, method_name: &str) -> String {
    // Create a unique string representation of the type `for_ty`. Handle complex types carefully.
    let for_ty_str = match for_ty {
        tast::Ty::TUnit => "unit".to_string(),
        tast::Ty::TBool => "bool".to_string(),
        tast::Ty::TInt => "int".to_string(),
        tast::Ty::TString => "string".to_string(),
        tast::Ty::TTuple { typs } => {
            let inner = typs
                .iter()
                .map(|ty| format!("{:?}", ty))
                .collect::<Vec<_>>()
                .join("_");
            format!("Tuple_{}", inner)
        }
        tast::Ty::TCon { name } => format!("Con_{}", name),
        tast::Ty::TApp { ty, args } => {
            let base = ty.get_constr_name_unsafe();
            let inner = args
                .iter()
                .map(|ty| format!("{:?}", ty))
                .collect::<Vec<_>>()
                .join("_");
            format!("App_{}_{}", base, inner)
        }
        tast::Ty::TParam { .. } => {
            unreachable!()
        }
        tast::Ty::TFunc { .. } => {
            unreachable!()
        }
        tast::Ty::TVar(..) => {
            unreachable!()
        }
    };
    format!("impl_{}_{}_{}", trait_name.0, for_ty_str, method_name)
}
