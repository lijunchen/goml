use crate::{tast, type_encoding::encode_ty};
use ::ast::ast;

pub fn mangle_impl_name(trait_name: &ast::Uident, for_ty: &tast::Ty, method_name: &str) -> String {
    let for_ty_str = encode_ty(for_ty);
    format!("impl_{}_{}_{}", trait_name.0, for_ty_str, method_name)
}
