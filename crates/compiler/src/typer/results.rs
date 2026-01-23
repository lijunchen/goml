use parser::syntax::MySyntaxNodePtr;

use crate::{common, hir, tast};

#[derive(Debug, Clone)]
pub struct TypeckResults {
    package: hir::PackageId,
    expr_tys: Vec<Option<tast::Ty>>,
    pat_tys: Vec<Option<tast::Ty>>,
    local_tys: Vec<Option<tast::Ty>>,
    name_ref_elab: Vec<Option<NameRefElab>>,
    call_elab: Vec<Option<CallElab>>,
    struct_lit_elab: Vec<Option<StructLitElab>>,
    struct_pat_elab: Vec<Option<StructPatElab>>,
    constr_expr: Vec<Option<common::Constructor>>,
    constr_pat: Vec<Option<common::Constructor>>,
    unary_res: Vec<Option<tast::UnaryResolution>>,
    binary_res: Vec<Option<tast::BinaryResolution>>,
    coercions: Vec<Vec<Coercion>>,
    closure_captures: Vec<Option<Vec<(String, tast::Ty)>>>,
}

impl TypeckResults {
    pub fn package(&self) -> hir::PackageId {
        self.package
    }

    pub fn expr_ty(&self, expr: hir::ExprId) -> Option<&tast::Ty> {
        self.expr_tys
            .get(expr.idx as usize)
            .and_then(|t| t.as_ref())
    }

    pub fn pat_ty(&self, pat: hir::PatId) -> Option<&tast::Ty> {
        self.pat_tys.get(pat.idx as usize).and_then(|t| t.as_ref())
    }

    pub fn local_ty(&self, local: hir::LocalId) -> Option<&tast::Ty> {
        self.local_tys
            .get(local.idx as usize)
            .and_then(|t| t.as_ref())
    }

    pub fn name_ref_elab(&self, expr: hir::ExprId) -> Option<&NameRefElab> {
        self.name_ref_elab
            .get(expr.idx as usize)
            .and_then(|e| e.as_ref())
    }

    pub fn call_elab(&self, expr: hir::ExprId) -> Option<&CallElab> {
        self.call_elab
            .get(expr.idx as usize)
            .and_then(|e| e.as_ref())
    }

    pub fn struct_lit_elab(&self, expr: hir::ExprId) -> Option<&StructLitElab> {
        self.struct_lit_elab
            .get(expr.idx as usize)
            .and_then(|e| e.as_ref())
    }

    pub fn struct_pat_elab(&self, pat: hir::PatId) -> Option<&StructPatElab> {
        self.struct_pat_elab
            .get(pat.idx as usize)
            .and_then(|e| e.as_ref())
    }

    pub fn constructor_for_expr(&self, expr: hir::ExprId) -> Option<&common::Constructor> {
        self.constr_expr
            .get(expr.idx as usize)
            .and_then(|e| e.as_ref())
    }

    pub fn constructor_for_pat(&self, pat: hir::PatId) -> Option<&common::Constructor> {
        self.constr_pat
            .get(pat.idx as usize)
            .and_then(|e| e.as_ref())
    }

    pub fn unary_resolution(&self, expr: hir::ExprId) -> Option<&tast::UnaryResolution> {
        self.unary_res
            .get(expr.idx as usize)
            .and_then(|e| e.as_ref())
    }

    pub fn binary_resolution(&self, expr: hir::ExprId) -> Option<&tast::BinaryResolution> {
        self.binary_res
            .get(expr.idx as usize)
            .and_then(|e| e.as_ref())
    }

    pub fn coercions(&self, expr: hir::ExprId) -> &[Coercion] {
        self.coercions.get(expr.idx as usize).map_or(&[], |v| v)
    }

    pub fn closure_captures(&self, expr: hir::ExprId) -> Option<&[(String, tast::Ty)]> {
        self.closure_captures
            .get(expr.idx as usize)
            .and_then(|v| v.as_ref().map(|v| v.as_slice()))
    }
}

#[derive(Debug, Clone)]
pub enum NameRefElab {
    Var {
        name: String,
        ty: tast::Ty,
        astptr: Option<MySyntaxNodePtr>,
    },
    TraitMethod {
        trait_name: tast::TastIdent,
        method_name: tast::TastIdent,
        ty: tast::Ty,
        astptr: Option<MySyntaxNodePtr>,
    },
    DynTraitMethod {
        trait_name: tast::TastIdent,
        method_name: tast::TastIdent,
        ty: tast::Ty,
        astptr: Option<MySyntaxNodePtr>,
    },
    InherentMethod {
        receiver_ty: tast::Ty,
        method_name: tast::TastIdent,
        ty: tast::Ty,
        astptr: Option<MySyntaxNodePtr>,
    },
}

#[derive(Debug, Clone)]
pub struct CallElab {
    pub callee: CalleeElab,
    pub args: Vec<hir::ExprId>,
}

#[derive(Debug, Clone)]
pub enum CalleeElab {
    Expr(hir::ExprId),
    Var {
        name: String,
        ty: tast::Ty,
        astptr: Option<MySyntaxNodePtr>,
    },
    TraitMethod {
        trait_name: tast::TastIdent,
        method_name: tast::TastIdent,
        ty: tast::Ty,
        astptr: Option<MySyntaxNodePtr>,
    },
    DynTraitMethod {
        trait_name: tast::TastIdent,
        method_name: tast::TastIdent,
        ty: tast::Ty,
        astptr: Option<MySyntaxNodePtr>,
    },
    InherentMethod {
        receiver_ty: tast::Ty,
        method_name: tast::TastIdent,
        ty: tast::Ty,
        astptr: Option<MySyntaxNodePtr>,
    },
    Error {
        ty: tast::Ty,
        astptr: Option<MySyntaxNodePtr>,
    },
}

#[derive(Debug, Clone)]
pub struct StructLitElab {
    pub constructor: common::Constructor,
    pub args: Vec<StructLitArgElab>,
}

#[derive(Debug, Clone)]
pub enum StructLitArgElab {
    Expr(hir::ExprId),
    Missing { expected_ty: tast::Ty },
}

#[derive(Debug, Clone)]
pub struct StructPatElab {
    pub constructor: common::Constructor,
    pub args: Vec<StructPatArgElab>,
}

#[derive(Debug, Clone)]
pub enum StructPatArgElab {
    Pat(hir::PatId),
    MissingWild { expected_ty: tast::Ty },
}

#[derive(Debug, Clone)]
pub enum Coercion {
    ToDyn {
        trait_name: tast::TastIdent,
        for_ty: tast::Ty,
        ty: tast::Ty,
        astptr: Option<MySyntaxNodePtr>,
    },
}

pub struct TypeckResultsBuilder {
    results: TypeckResults,
}

impl TypeckResultsBuilder {
    pub fn new(hir_table: &hir::HirTable) -> Self {
        let expr_count = hir_table.expr_count();
        let pat_count = hir_table.pat_count();
        let local_count = hir_table.local_count();
        Self {
            results: TypeckResults {
                package: hir_table.package(),
                expr_tys: vec![None; expr_count],
                pat_tys: vec![None; pat_count],
                local_tys: vec![None; local_count],
                name_ref_elab: vec![None; expr_count],
                call_elab: vec![None; expr_count],
                struct_lit_elab: vec![None; expr_count],
                struct_pat_elab: vec![None; pat_count],
                constr_expr: vec![None; expr_count],
                constr_pat: vec![None; pat_count],
                unary_res: vec![None; expr_count],
                binary_res: vec![None; expr_count],
                coercions: vec![Vec::new(); expr_count],
                closure_captures: vec![None; expr_count],
            },
        }
    }

    pub fn results(&self) -> &TypeckResults {
        &self.results
    }

    pub fn finish(self) -> TypeckResults {
        self.results
    }

    pub fn record_expr_ty(&mut self, expr: hir::ExprId, ty: tast::Ty) {
        if let Some(slot) = self.results.expr_tys.get_mut(expr.idx as usize) {
            *slot = Some(ty);
        }
    }

    pub fn record_pat_ty(&mut self, pat: hir::PatId, ty: tast::Ty) {
        if let Some(slot) = self.results.pat_tys.get_mut(pat.idx as usize) {
            *slot = Some(ty);
        }
    }

    pub fn record_local_ty(&mut self, local: hir::LocalId, ty: tast::Ty) {
        if let Some(slot) = self.results.local_tys.get_mut(local.idx as usize) {
            *slot = Some(ty);
        }
    }

    pub fn record_name_ref_elab(&mut self, expr: hir::ExprId, elab: NameRefElab) {
        if let Some(slot) = self.results.name_ref_elab.get_mut(expr.idx as usize) {
            *slot = Some(elab);
        }
    }

    pub fn record_call_elab(&mut self, expr: hir::ExprId, elab: CallElab) {
        if let Some(slot) = self.results.call_elab.get_mut(expr.idx as usize) {
            *slot = Some(elab);
        }
    }

    pub fn record_struct_lit_elab(&mut self, expr: hir::ExprId, elab: StructLitElab) {
        if let Some(slot) = self.results.struct_lit_elab.get_mut(expr.idx as usize) {
            *slot = Some(elab);
        }
    }

    pub fn record_struct_pat_elab(&mut self, pat: hir::PatId, elab: StructPatElab) {
        if let Some(slot) = self.results.struct_pat_elab.get_mut(pat.idx as usize) {
            *slot = Some(elab);
        }
    }

    pub fn record_constructor_expr(&mut self, expr: hir::ExprId, constructor: common::Constructor) {
        if let Some(slot) = self.results.constr_expr.get_mut(expr.idx as usize) {
            *slot = Some(constructor);
        }
    }

    pub fn record_constructor_pat(&mut self, pat: hir::PatId, constructor: common::Constructor) {
        if let Some(slot) = self.results.constr_pat.get_mut(pat.idx as usize) {
            *slot = Some(constructor);
        }
    }

    pub fn record_unary_resolution(&mut self, expr: hir::ExprId, res: tast::UnaryResolution) {
        if let Some(slot) = self.results.unary_res.get_mut(expr.idx as usize) {
            *slot = Some(res);
        }
    }

    pub fn record_binary_resolution(&mut self, expr: hir::ExprId, res: tast::BinaryResolution) {
        if let Some(slot) = self.results.binary_res.get_mut(expr.idx as usize) {
            *slot = Some(res);
        }
    }

    pub fn push_coercion(&mut self, expr: hir::ExprId, coercion: Coercion) {
        if let Some(slot) = self.results.coercions.get_mut(expr.idx as usize) {
            slot.push(coercion);
        }
    }

    pub fn record_closure_captures(
        &mut self,
        expr: hir::ExprId,
        captures: Vec<(String, tast::Ty)>,
    ) {
        if let Some(slot) = self.results.closure_captures.get_mut(expr.idx as usize) {
            *slot = Some(captures);
        }
    }

    pub fn finalize_types(&mut self, typer: &mut super::Typer) {
        for slot in self.results.expr_tys.iter_mut().filter_map(Option::as_mut) {
            *slot = typer.subst_ty_silent(slot);
        }
        for slot in self.results.pat_tys.iter_mut().filter_map(Option::as_mut) {
            *slot = typer.subst_ty_silent(slot);
        }
        for slot in self.results.local_tys.iter_mut().filter_map(Option::as_mut) {
            *slot = typer.subst_ty_silent(slot);
        }
        for elab in self
            .results
            .name_ref_elab
            .iter_mut()
            .filter_map(Option::as_mut)
        {
            match elab {
                NameRefElab::Var { ty, .. } => *ty = typer.subst_ty_silent(ty),
                NameRefElab::TraitMethod { ty, .. } => *ty = typer.subst_ty_silent(ty),
                NameRefElab::DynTraitMethod { ty, .. } => *ty = typer.subst_ty_silent(ty),
                NameRefElab::InherentMethod {
                    receiver_ty, ty, ..
                } => {
                    *receiver_ty = typer.subst_ty_silent(receiver_ty);
                    *ty = typer.subst_ty_silent(ty);
                }
            }
        }
        for elab in self.results.call_elab.iter_mut().filter_map(Option::as_mut) {
            match &mut elab.callee {
                CalleeElab::Expr(_) => {}
                CalleeElab::Var { ty, .. } => *ty = typer.subst_ty_silent(ty),
                CalleeElab::TraitMethod { ty, .. } => *ty = typer.subst_ty_silent(ty),
                CalleeElab::DynTraitMethod { ty, .. } => *ty = typer.subst_ty_silent(ty),
                CalleeElab::InherentMethod {
                    receiver_ty, ty, ..
                } => {
                    *receiver_ty = typer.subst_ty_silent(receiver_ty);
                    *ty = typer.subst_ty_silent(ty);
                }
                CalleeElab::Error { ty, .. } => *ty = typer.subst_ty_silent(ty),
            }
        }
        for elab in self
            .results
            .struct_lit_elab
            .iter_mut()
            .filter_map(Option::as_mut)
        {
            for arg in elab.args.iter_mut() {
                if let StructLitArgElab::Missing { expected_ty } = arg {
                    *expected_ty = typer.subst_ty_silent(expected_ty);
                }
            }
        }
        for elab in self
            .results
            .struct_pat_elab
            .iter_mut()
            .filter_map(Option::as_mut)
        {
            for arg in elab.args.iter_mut() {
                if let StructPatArgElab::MissingWild { expected_ty } = arg {
                    *expected_ty = typer.subst_ty_silent(expected_ty);
                }
            }
        }
        for coercions in self.results.coercions.iter_mut() {
            for coercion in coercions.iter_mut() {
                match coercion {
                    Coercion::ToDyn { for_ty, ty, .. } => {
                        *for_ty = typer.subst_ty_silent(for_ty);
                        *ty = typer.subst_ty_silent(ty);
                    }
                }
            }
        }
        for captures in self
            .results
            .closure_captures
            .iter_mut()
            .filter_map(Option::as_mut)
        {
            for (_, ty) in captures.iter_mut() {
                *ty = typer.subst_ty_silent(ty);
            }
        }
    }
}
