use std::collections::{HashMap, HashSet};

use cst::cst::CstNode;
use parser::syntax::{MySyntaxNodePtr, MySyntaxToken};
use text_size::TextRange;

use crate::hir;

#[derive(Debug, Clone)]
pub(crate) struct HirResultsIndex {
    expr_by_ptr: HashMap<MySyntaxNodePtr, hir::ExprId>,
    pat_by_ptr: HashMap<MySyntaxNodePtr, hir::PatId>,
    local_by_ptr: HashMap<MySyntaxNodePtr, hir::LocalId>,
}

impl HirResultsIndex {
    pub(crate) fn new(hir_table: &hir::HirTable) -> Self {
        let mut expr_by_ptr = HashMap::new();
        let mut pat_by_ptr = HashMap::new();
        let mut local_by_ptr = HashMap::new();

        for idx in 0..hir_table.expr_count() {
            let expr_id = hir::ExprId {
                pkg: hir_table.package(),
                idx: idx as u32,
            };
            if let Some(ptr) = hir_table.expr_ptr(expr_id) {
                expr_by_ptr.insert(ptr, expr_id);
            }
        }

        for idx in 0..hir_table.pat_count() {
            let pat_id = hir::PatId {
                pkg: hir_table.package(),
                idx: idx as u32,
            };
            if let Some(ptr) = hir_table.pat_ptr(pat_id) {
                pat_by_ptr.insert(ptr, pat_id);
            }
        }

        for (local_id, _) in hir_table.iter_locals() {
            if let Some(ptr) = hir_table.local_origin_ptr(local_id) {
                local_by_ptr.insert(ptr, local_id);
            }
        }

        Self {
            expr_by_ptr,
            pat_by_ptr,
            local_by_ptr,
        }
    }

    pub(crate) fn expr_id(&self, ptr: &MySyntaxNodePtr) -> Option<hir::ExprId> {
        self.expr_by_ptr.get(ptr).copied()
    }

    pub(crate) fn pat_id(&self, ptr: &MySyntaxNodePtr) -> Option<hir::PatId> {
        self.pat_by_ptr.get(ptr).copied()
    }

    pub(crate) fn local_id(&self, ptr: &MySyntaxNodePtr) -> Option<hir::LocalId> {
        self.local_by_ptr.get(ptr).copied()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ClosureParamIndex {
    local_by_ptr: HashMap<MySyntaxNodePtr, hir::LocalId>,
}

impl ClosureParamIndex {
    pub(crate) fn new(hir_table: &hir::HirTable) -> Self {
        let mut local_by_ptr = HashMap::new();
        for idx in 0..hir_table.expr_count() {
            let expr_id = hir::ExprId {
                pkg: hir_table.package(),
                idx: idx as u32,
            };
            if let hir::Expr::EClosure { params, .. } = hir_table.expr(expr_id) {
                for param in params {
                    local_by_ptr.insert(param.astptr, param.name);
                }
            }
        }
        Self { local_by_ptr }
    }

    pub(crate) fn local_id(&self, ptr: &MySyntaxNodePtr) -> Option<hir::LocalId> {
        self.local_by_ptr.get(ptr).copied()
    }
}

pub(crate) fn find_mapped_expr_id_from_token(
    token: &MySyntaxToken,
    index: &HirResultsIndex,
) -> Option<hir::ExprId> {
    let mut current = token.parent();
    while let Some(node) = current {
        if cst::nodes::Expr::can_cast(node.kind()) {
            let ptr = MySyntaxNodePtr::new(&node);
            if let Some(id) = index.expr_id(&ptr) {
                return Some(id);
            }
        }
        current = node.parent();
    }
    None
}

pub(crate) fn find_mapped_pat_id_from_token(
    token: &MySyntaxToken,
    index: &HirResultsIndex,
) -> Option<hir::PatId> {
    let mut current = token.parent();
    while let Some(node) = current {
        if cst::nodes::Pattern::can_cast(node.kind()) {
            let ptr = MySyntaxNodePtr::new(&node);
            if let Some(id) = index.pat_id(&ptr) {
                return Some(id);
            }
        }
        current = node.parent();
    }
    None
}

pub(crate) fn find_mapped_local_id_from_token(
    token: &MySyntaxToken,
    index: &HirResultsIndex,
) -> Option<hir::LocalId> {
    let mut current = token.parent();
    while let Some(node) = current {
        let ptr = MySyntaxNodePtr::new(&node);
        if let Some(id) = index.local_id(&ptr) {
            return Some(id);
        }
        current = node.parent();
    }
    None
}

pub(crate) fn expr_ids_from_token(
    token: &MySyntaxToken,
    index: &HirResultsIndex,
) -> Vec<hir::ExprId> {
    let mut ids = Vec::new();
    let mut seen = HashSet::new();
    let mut current = token.parent();
    while let Some(node) = current {
        let ptr = MySyntaxNodePtr::new(&node);
        if let Some(id) = index.expr_id(&ptr)
            && seen.insert(id.idx)
        {
            ids.push(id);
        }
        current = node.parent();
    }
    ids
}

pub(crate) fn pat_ids_from_token(
    token: &MySyntaxToken,
    index: &HirResultsIndex,
) -> Vec<hir::PatId> {
    let mut ids = Vec::new();
    let mut seen = HashSet::new();
    let mut current = token.parent();
    while let Some(node) = current {
        let ptr = MySyntaxNodePtr::new(&node);
        if let Some(id) = index.pat_id(&ptr)
            && seen.insert(id.idx)
        {
            ids.push(id);
        }
        current = node.parent();
    }
    ids
}

pub(crate) fn local_def_range_from_pats(
    hir_table: &hir::HirTable,
    local: hir::LocalId,
) -> Option<TextRange> {
    for idx in 0..hir_table.pat_count() {
        let pat_id = hir::PatId {
            pkg: hir_table.package(),
            idx: idx as u32,
        };
        if let hir::Pat::PVar { name, astptr } = hir_table.pat(pat_id)
            && *name == local
        {
            return Some(astptr.text_range());
        }
    }
    None
}
