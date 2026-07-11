use std::collections::HashMap;

use ena::unify::{InPlace, Snapshot};

use crate::{
    tast::{self, TypeVar},
    typer::{
        Typer,
        obligations::{InstantiatedScheme, ObligationCause, Predicate, TraitGoal},
    },
};
use diagnostics::{Severity, Stage};
use parser::{Diagnostic, Diagnostics};
use text_size::TextRange;

pub(crate) struct InferenceSnapshot {
    table: Snapshot<InPlace<TypeVar>>,
    array_wildcard_counter: usize,
    array_wildcard_resolutions: HashMap<usize, usize>,
}

fn pat_origin(pat: &tast::Pat) -> Option<TextRange> {
    match pat {
        tast::Pat::PVar { astptr, .. }
        | tast::Pat::PPrim { astptr, .. }
        | tast::Pat::PConstr { astptr, .. }
        | tast::Pat::PTuple { astptr, .. }
        | tast::Pat::PWild { astptr, .. } => astptr.as_ref().map(|ptr| ptr.text_range()),
    }
}

fn expr_origin(expr: &tast::Expr) -> Option<TextRange> {
    match expr {
        tast::Expr::EVar { astptr, .. } | tast::Expr::ECallable { astptr, .. } => {
            astptr.as_ref().map(|ptr| ptr.text_range())
        }
        tast::Expr::EMatch { astptr, expr, .. } => astptr
            .as_ref()
            .map(|ptr| ptr.text_range())
            .or_else(|| expr_origin(expr.as_ref())),
        tast::Expr::EField { astptr, expr, .. } => astptr
            .as_ref()
            .map(|ptr| ptr.text_range())
            .or_else(|| expr_origin(expr.as_ref())),
        tast::Expr::EIndex {
            astptr,
            base,
            index,
            ..
        } => astptr
            .as_ref()
            .map(|ptr| ptr.text_range())
            .or_else(|| expr_origin(base.as_ref()))
            .or_else(|| expr_origin(index.as_ref())),
        tast::Expr::ETraitMethod { astptr, .. } => astptr.as_ref().map(|ptr| ptr.text_range()),
        tast::Expr::EDynTraitMethod { astptr, .. } => astptr.as_ref().map(|ptr| ptr.text_range()),
        tast::Expr::EInherentMethod { astptr, .. } => astptr.as_ref().map(|ptr| ptr.text_range()),
        tast::Expr::EToDyn { astptr, expr, .. } => astptr
            .as_ref()
            .map(|ptr| ptr.text_range())
            .or_else(|| expr_origin(expr.as_ref())),
        tast::Expr::ECall { func, args, .. } => expr_origin(func)
            .or_else(|| args.first().and_then(expr_origin))
            .or_else(|| args.last().and_then(expr_origin)),
        tast::Expr::EBinary { lhs, rhs, .. } => expr_origin(lhs).or_else(|| expr_origin(rhs)),
        tast::Expr::EUnary { expr, .. } => expr_origin(expr),
        tast::Expr::EBlock { block, .. } => block
            .tail
            .as_ref()
            .and_then(|tail| expr_origin(tail.as_ref()))
            .or_else(|| {
                block.stmts.iter().find_map(|stmt| match stmt {
                    tast::Stmt::Let(stmt) => {
                        pat_origin(&stmt.pat).or_else(|| expr_origin(stmt.value.as_ref()))
                    }
                    tast::Stmt::Assign(stmt) => {
                        expr_origin(&stmt.target).or_else(|| expr_origin(&stmt.value))
                    }
                    tast::Stmt::Expr(stmt) => expr_origin(&stmt.expr),
                })
            }),
        tast::Expr::EIf {
            cond,
            then_branch,
            else_branch,
            ..
        } => expr_origin(cond)
            .or_else(|| expr_origin(then_branch))
            .or_else(|| expr_origin(else_branch)),
        tast::Expr::EWhile { cond, body, .. } => expr_origin(cond).or_else(|| expr_origin(body)),
        tast::Expr::EFor {
            pat,
            iterator,
            body,
            ..
        } => pat_origin(pat)
            .or_else(|| expr_origin(iterator))
            .or_else(|| expr_origin(body)),
        tast::Expr::EBreak { .. } | tast::Expr::EContinue { .. } => None,
        tast::Expr::EReturn { expr, .. } => expr.as_deref().and_then(expr_origin),
        tast::Expr::EGo { expr, .. } => expr_origin(expr),
        tast::Expr::EProj { tuple, .. } => expr_origin(tuple),
        tast::Expr::EConstr { args, .. } => args.first().and_then(expr_origin),
        tast::Expr::ETuple { items, .. } => items.first().and_then(expr_origin),
        tast::Expr::EArray { items, .. } => items.first().and_then(expr_origin),
        tast::Expr::EClosure { body, .. } => expr_origin(body),
        tast::Expr::EPrim { .. } => None,
    }
}

fn occurs(
    diagnostics: &mut Diagnostics,
    origin: Option<TextRange>,
    var: TypeVar,
    ty: &tast::Ty,
) -> bool {
    match ty {
        tast::Ty::TVar(v) => {
            if var == *v {
                diagnostics.push(
                    Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type inference failed: recursive type involving {}",
                            super::util::format_ty_for_diag(ty)
                        ),
                    )
                    .with_range(origin),
                );
                return false;
            }
        }
        tast::Ty::TUnit
        | tast::Ty::TBool
        | tast::Ty::TInt8
        | tast::Ty::TInt16
        | tast::Ty::TInt32
        | tast::Ty::TInt64
        | tast::Ty::TUint8
        | tast::Ty::TUint16
        | tast::Ty::TUint32
        | tast::Ty::TUint64
        | tast::Ty::TFloat32
        | tast::Ty::TFloat64
        | tast::Ty::TString
        | tast::Ty::TChar
        | tast::Ty::TParam { .. } => {}
        tast::Ty::TTuple { typs } => {
            for ty in typs.iter() {
                if !occurs(diagnostics, origin, var, ty) {
                    return false;
                }
            }
        }
        tast::Ty::TEnum { .. } | tast::Ty::TStruct { .. } => {}
        tast::Ty::TDyn { .. } => {}
        tast::Ty::TApp { ty, args } => {
            if !occurs(diagnostics, origin, var, ty.as_ref()) {
                return false;
            }
            for arg in args.iter() {
                if !occurs(diagnostics, origin, var, arg) {
                    return false;
                }
            }
        }
        tast::Ty::TArray { elem, .. } => {
            if !occurs(diagnostics, origin, var, elem) {
                return false;
            }
        }
        tast::Ty::TSlice { elem } => {
            if !occurs(diagnostics, origin, var, elem) {
                return false;
            }
        }
        tast::Ty::TVec { elem } => {
            if !occurs(diagnostics, origin, var, elem) {
                return false;
            }
        }
        tast::Ty::TRef { elem } => {
            if !occurs(diagnostics, origin, var, elem) {
                return false;
            }
        }
        tast::Ty::THashMap { key, value } => {
            if !occurs(diagnostics, origin, var, key) {
                return false;
            }
            if !occurs(diagnostics, origin, var, value) {
                return false;
            }
        }
        tast::Ty::TFunc { params, ret_ty } => {
            for param in params.iter() {
                if !occurs(diagnostics, origin, var, param) {
                    return false;
                }
            }
            if !occurs(diagnostics, origin, var, ret_ty) {
                return false;
            }
        }
    }

    true
}

impl Typer {
    pub(crate) fn norm(&mut self, ty: &tast::Ty) -> tast::Ty {
        match ty {
            tast::Ty::TVar(v) => {
                if let Some(value) = self.uni.probe_value(*v) {
                    self.norm(&value)
                } else {
                    tast::Ty::TVar(self.uni.find(*v))
                }
            }
            tast::Ty::TUnit => tast::Ty::TUnit,
            tast::Ty::TBool => tast::Ty::TBool,
            tast::Ty::TInt8 => tast::Ty::TInt8,
            tast::Ty::TInt16 => tast::Ty::TInt16,
            tast::Ty::TInt32 => tast::Ty::TInt32,
            tast::Ty::TInt64 => tast::Ty::TInt64,
            tast::Ty::TUint8 => tast::Ty::TUint8,
            tast::Ty::TUint16 => tast::Ty::TUint16,
            tast::Ty::TUint32 => tast::Ty::TUint32,
            tast::Ty::TUint64 => tast::Ty::TUint64,
            tast::Ty::TFloat32 => tast::Ty::TFloat32,
            tast::Ty::TFloat64 => tast::Ty::TFloat64,
            tast::Ty::TString => tast::Ty::TString,
            tast::Ty::TChar => tast::Ty::TChar,
            tast::Ty::TTuple { typs } => {
                let typs = typs.iter().map(|ty| self.norm(ty)).collect();
                tast::Ty::TTuple { typs }
            }
            tast::Ty::TEnum { name } => tast::Ty::TEnum { name: name.clone() },
            tast::Ty::TStruct { name } => tast::Ty::TStruct { name: name.clone() },
            tast::Ty::TDyn { trait_name } => tast::Ty::TDyn {
                trait_name: trait_name.clone(),
            },
            tast::Ty::TApp { ty, args } => tast::Ty::TApp {
                ty: Box::new(self.norm(ty)),
                args: args.iter().map(|ty| self.norm(ty)).collect(),
            },
            tast::Ty::TArray { len, elem } => tast::Ty::TArray {
                len: self.resolve_array_len(*len),
                elem: Box::new(self.norm(elem)),
            },
            tast::Ty::TSlice { elem } => tast::Ty::TSlice {
                elem: Box::new(self.norm(elem)),
            },
            tast::Ty::TVec { elem } => tast::Ty::TVec {
                elem: Box::new(self.norm(elem)),
            },
            tast::Ty::TRef { elem } => tast::Ty::TRef {
                elem: Box::new(self.norm(elem)),
            },
            tast::Ty::THashMap { key, value } => tast::Ty::THashMap {
                key: Box::new(self.norm(key)),
                value: Box::new(self.norm(value)),
            },
            tast::Ty::TFunc { params, ret_ty } => {
                let params = params.iter().map(|ty| self.norm(ty)).collect();
                let ret_ty = Box::new(self.norm(ret_ty));
                tast::Ty::TFunc { params, ret_ty }
            }
            tast::Ty::TParam { name } => tast::Ty::TParam { name: name.clone() },
        }
    }

    pub(crate) fn try_unify_silent(&mut self, l: &tast::Ty, r: &tast::Ty) -> bool {
        let snapshot = self.snapshot_inference();
        let mut diagnostics = Diagnostics::new();
        if self.unify(&mut diagnostics, l, r, None) {
            self.commit_inference(snapshot);
            true
        } else {
            self.rollback_inference(snapshot);
            false
        }
    }

    pub(crate) fn snapshot_inference(&mut self) -> InferenceSnapshot {
        InferenceSnapshot {
            table: self.uni.snapshot(),
            array_wildcard_counter: self.array_wildcard_counter,
            array_wildcard_resolutions: self.array_wildcard_resolutions.clone(),
        }
    }

    pub(crate) fn commit_inference(&mut self, snapshot: InferenceSnapshot) {
        self.uni.commit(snapshot.table);
    }

    pub(crate) fn rollback_inference(&mut self, snapshot: InferenceSnapshot) {
        self.uni.rollback_to(snapshot.table);
        self.array_wildcard_counter = snapshot.array_wildcard_counter;
        self.array_wildcard_resolutions = snapshot.array_wildcard_resolutions;
    }

    pub(crate) fn equate(
        &mut self,
        diagnostics: &mut Diagnostics,
        left: &tast::Ty,
        right: &tast::Ty,
        origin: Option<TextRange>,
    ) {
        let _ = self.unify(diagnostics, left, right, origin);
    }

    pub(crate) fn unify(
        &mut self,
        diagnostics: &mut Diagnostics,
        l: &tast::Ty,
        r: &tast::Ty,
        origin: Option<TextRange>,
    ) -> bool {
        let l_norm = self.norm(l);
        let r_norm = self.norm(r);
        match (&l_norm, &r_norm) {
            (tast::Ty::TVar(a), tast::Ty::TVar(b)) => {
                if self.uni.unify_var_var(*a, *b).is_err() {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            "Type inference failed while unifying unknown types".to_string(),
                        )
                        .with_range(origin),
                    );

                    return false;
                }
            }
            (tast::Ty::TVar(a), t) | (t, tast::Ty::TVar(a)) => {
                if !occurs(diagnostics, origin, *a, t) {
                    return false;
                }
                if self.uni.unify_var_value(*a, Some(t.clone())).is_err() {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Type inference failed while unifying unknown type with {}",
                                super::util::format_ty_for_diag(t)
                            ),
                        )
                        .with_range(origin),
                    );
                    return false;
                }
            }

            (tast::Ty::TUnit, tast::Ty::TUnit) => {}
            (tast::Ty::TBool, tast::Ty::TBool) => {}
            (tast::Ty::TInt32, tast::Ty::TInt32) => {}
            (tast::Ty::TInt8, tast::Ty::TInt8) => {}
            (tast::Ty::TInt16, tast::Ty::TInt16) => {}
            (tast::Ty::TInt64, tast::Ty::TInt64) => {}
            (tast::Ty::TUint8, tast::Ty::TUint8) => {}
            (tast::Ty::TUint16, tast::Ty::TUint16) => {}
            (tast::Ty::TUint32, tast::Ty::TUint32) => {}
            (tast::Ty::TUint64, tast::Ty::TUint64) => {}
            (tast::Ty::TFloat32, tast::Ty::TFloat32) => {}
            (tast::Ty::TFloat64, tast::Ty::TFloat64) => {}
            (tast::Ty::TString, tast::Ty::TString) => {}
            (tast::Ty::TChar, tast::Ty::TChar) => {}
            (tast::Ty::TTuple { typs: typs1 }, tast::Ty::TTuple { typs: typs2 }) => {
                if typs1.len() != typs2.len() {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Tuple length mismatch: expected {}, found {}",
                                super::util::format_ty_for_diag(&l_norm),
                                super::util::format_ty_for_diag(&r_norm)
                            ),
                        )
                        .with_range(origin),
                    );
                    return false;
                }
                for (ty1, ty2) in typs1.iter().zip(typs2.iter()) {
                    if !self.unify(diagnostics, ty1, ty2, origin) {
                        return false;
                    }
                }
            }
            (
                tast::Ty::TArray {
                    len: len1,
                    elem: elem1,
                },
                tast::Ty::TArray {
                    len: len2,
                    elem: elem2,
                },
            ) => {
                let r1 = self.resolve_array_len(*len1);
                let r2 = self.resolve_array_len(*len2);
                let w1 = self.is_array_wildcard(r1);
                let w2 = self.is_array_wildcard(r2);
                match (w1, w2) {
                    (false, false) if r1 != r2 => {
                        diagnostics.push(
                            Diagnostic::new(
                                Stage::Typer,
                                Severity::Error,
                                format!(
                                    "Array length mismatch: expected {}, found {}",
                                    super::util::format_ty_for_diag(&l_norm),
                                    super::util::format_ty_for_diag(&r_norm)
                                ),
                            )
                            .with_range(origin),
                        );
                        return false;
                    }
                    (true, false) => {
                        self.array_wildcard_resolutions.insert(r1, r2);
                    }
                    (false, true) => {
                        self.array_wildcard_resolutions.insert(r2, r1);
                    }
                    (true, true) if r1 != r2 => {
                        self.array_wildcard_resolutions.insert(r1, r2);
                    }
                    _ => {}
                }
                if !self.unify(diagnostics, elem1, elem2, origin) {
                    return false;
                }
            }
            (tast::Ty::TRef { elem: elem1 }, tast::Ty::TRef { elem: elem2 }) => {
                if !self.unify(diagnostics, elem1, elem2, origin) {
                    return false;
                }
            }
            (tast::Ty::TSlice { elem: elem1 }, tast::Ty::TSlice { elem: elem2 }) => {
                if !self.unify(diagnostics, elem1, elem2, origin) {
                    return false;
                }
            }
            (tast::Ty::TVec { elem: elem1 }, tast::Ty::TVec { elem: elem2 }) => {
                if !self.unify(diagnostics, elem1, elem2, origin) {
                    return false;
                }
            }
            (
                tast::Ty::THashMap {
                    key: key1,
                    value: value1,
                },
                tast::Ty::THashMap {
                    key: key2,
                    value: value2,
                },
            ) => {
                if !self.unify(diagnostics, key1, key2, origin) {
                    return false;
                }
                if !self.unify(diagnostics, value1, value2, origin) {
                    return false;
                }
            }
            (
                tast::Ty::TFunc {
                    params: param1,
                    ret_ty: ret_ty1,
                },
                tast::Ty::TFunc {
                    params: param2,
                    ret_ty: ret_ty2,
                },
            ) => {
                if param1.len() != param2.len() {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Function arity mismatch: expected {}, found {}",
                                super::util::format_ty_for_diag(&l_norm),
                                super::util::format_ty_for_diag(&r_norm)
                            ),
                        )
                        .with_range(origin),
                    );
                    return false;
                }
                for (p1, p2) in param1.iter().zip(param2.iter()) {
                    if !self.unify(diagnostics, p1, p2, origin) {
                        return false;
                    }
                }
                if !self.unify(diagnostics, ret_ty1, ret_ty2, origin) {
                    return false;
                }
            }
            (tast::Ty::TEnum { name: n1 }, tast::Ty::TEnum { name: n2 })
            | (tast::Ty::TStruct { name: n1 }, tast::Ty::TStruct { name: n2 }) => {
                if n1 != n2 {
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type mismatch: expected {}, found {}",
                            super::util::format_ty_for_diag(&l_norm),
                            super::util::format_ty_for_diag(&r_norm)
                        ),
                    ));
                    return false;
                }
            }
            (tast::Ty::TDyn { trait_name: t1 }, tast::Ty::TDyn { trait_name: t2 }) => {
                if t1 != t2 {
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type mismatch: expected {}, found {}",
                            super::util::format_ty_for_diag(&l_norm),
                            super::util::format_ty_for_diag(&r_norm)
                        ),
                    ));
                    return false;
                }
            }
            (
                tast::Ty::TApp {
                    ty: ty1,
                    args: args1,
                },
                tast::Ty::TApp {
                    ty: ty2,
                    args: args2,
                },
            ) => {
                if args1.len() != args2.len() {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Type argument arity mismatch: expected {}, found {}",
                                super::util::format_ty_for_diag(&l_norm),
                                super::util::format_ty_for_diag(&r_norm)
                            ),
                        )
                        .with_range(origin),
                    );
                    return false;
                }
                if !self.unify(diagnostics, ty1.as_ref(), ty2.as_ref(), origin) {
                    return false;
                }
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    if !self.unify(diagnostics, arg1, arg2, origin) {
                        return false;
                    }
                }
            }
            (tast::Ty::TParam { name }, tast::Ty::TParam { name: name2 }) => {
                if name != name2 {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Type mismatch: expected {}, found {}",
                                super::util::format_ty_for_diag(&l_norm),
                                super::util::format_ty_for_diag(&r_norm)
                            ),
                        )
                        .with_range(origin),
                    );
                    return false;
                }
            }
            (tast::Ty::TParam { name }, ty) | (ty, tast::Ty::TParam { name }) => {
                diagnostics.push(
                    Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type mismatch: expected {}, found {}",
                            name,
                            super::util::format_ty_for_diag(ty)
                        ),
                    )
                    .with_range(origin),
                );
                return false;
            }
            _ => {
                diagnostics.push(
                    Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type mismatch: expected {}, found {}",
                            super::util::format_ty_for_diag(&l_norm),
                            super::util::format_ty_for_diag(&r_norm)
                        ),
                    )
                    .with_range(origin),
                );
                return false;
            }
        }
        true
    }

    pub(crate) fn fresh_ty_var(&mut self) -> tast::Ty {
        tast::Ty::TVar(self.uni.new_key(None))
    }

    pub(crate) fn inst_ty(&mut self, ty: &tast::Ty) -> tast::Ty {
        let mut subst: HashMap<String, tast::Ty> = HashMap::new();
        let wildcard_len = self.fresh_array_wildcard();
        self._go_inst_ty(ty, &mut subst, wildcard_len)
    }

    pub(crate) fn instantiate_scheme(
        &mut self,
        scheme: &crate::env::FnScheme,
        cause: ObligationCause,
    ) -> InstantiatedScheme {
        self.instantiate_scheme_with_substitution(scheme, HashMap::new(), cause)
    }

    pub(crate) fn instantiate_scheme_with_substitution(
        &mut self,
        scheme: &crate::env::FnScheme,
        mut substitution: HashMap<String, tast::Ty>,
        cause: ObligationCause,
    ) -> InstantiatedScheme {
        for param in &scheme.type_params {
            if !substitution.contains_key(param) {
                substitution.insert(param.clone(), self.fresh_ty_var());
            }
        }
        let wildcard_len = self.fresh_array_wildcard();
        let ty = self._go_inst_ty(&scheme.ty, &mut substitution, wildcard_len);
        let obligations = scheme
            .constraints
            .iter()
            .filter_map(|constraint| {
                substitution
                    .get(&constraint.type_param)
                    .cloned()
                    .map(|for_ty| {
                        (
                            TraitGoal {
                                trait_name: constraint.trait_name.clone(),
                                for_ty,
                            },
                            cause.clone(),
                        )
                    })
            })
            .collect();
        InstantiatedScheme {
            ty,
            substitution,
            obligations,
        }
    }

    pub(crate) fn register_scheme_obligations(&mut self, instantiated: &InstantiatedScheme) {
        debug_assert!(instantiated.obligations.iter().all(|(goal, _)| {
            instantiated
                .substitution
                .values()
                .any(|ty| ty == &goal.for_ty)
        }));
        for (goal, cause) in &instantiated.obligations {
            self.push_obligation(Predicate::Trait(goal.clone()), cause.clone());
        }
    }

    fn _go_inst_ty(
        &mut self,
        ty: &tast::Ty,
        subst: &mut HashMap<String, tast::Ty>,
        wildcard_len: usize,
    ) -> tast::Ty {
        match ty {
            tast::Ty::TVar(_) => ty.clone(),
            tast::Ty::TUnit => ty.clone(),
            tast::Ty::TBool => ty.clone(),
            tast::Ty::TInt8 => ty.clone(),
            tast::Ty::TInt16 => ty.clone(),
            tast::Ty::TInt32 => ty.clone(),
            tast::Ty::TInt64 => ty.clone(),
            tast::Ty::TUint8 => ty.clone(),
            tast::Ty::TUint16 => ty.clone(),
            tast::Ty::TUint32 => ty.clone(),
            tast::Ty::TUint64 => ty.clone(),
            tast::Ty::TFloat32 => ty.clone(),
            tast::Ty::TFloat64 => ty.clone(),
            tast::Ty::TString => ty.clone(),
            tast::Ty::TChar => ty.clone(),
            tast::Ty::TTuple { typs } => {
                let typs = typs
                    .iter()
                    .map(|ty| self._go_inst_ty(ty, subst, wildcard_len))
                    .collect::<Vec<_>>();
                tast::Ty::TTuple { typs }
            }
            tast::Ty::TEnum { name } => tast::Ty::TEnum { name: name.clone() },
            tast::Ty::TStruct { name } => tast::Ty::TStruct { name: name.clone() },
            tast::Ty::TDyn { trait_name } => tast::Ty::TDyn {
                trait_name: trait_name.clone(),
            },
            tast::Ty::TApp { ty, args } => {
                let ty = self._go_inst_ty(ty, subst, wildcard_len);
                let args = args
                    .iter()
                    .map(|arg| self._go_inst_ty(arg, subst, wildcard_len))
                    .collect::<Vec<_>>();
                tast::Ty::TApp {
                    ty: Box::new(ty),
                    args,
                }
            }
            tast::Ty::TArray { len, elem } => tast::Ty::TArray {
                len: if *len == tast::ARRAY_WILDCARD_LEN {
                    wildcard_len
                } else {
                    *len
                },
                elem: Box::new(self._go_inst_ty(elem, subst, wildcard_len)),
            },
            tast::Ty::TSlice { elem } => tast::Ty::TSlice {
                elem: Box::new(self._go_inst_ty(elem, subst, wildcard_len)),
            },
            tast::Ty::TVec { elem } => tast::Ty::TVec {
                elem: Box::new(self._go_inst_ty(elem, subst, wildcard_len)),
            },
            tast::Ty::TRef { elem } => tast::Ty::TRef {
                elem: Box::new(self._go_inst_ty(elem, subst, wildcard_len)),
            },
            tast::Ty::THashMap { key, value } => tast::Ty::THashMap {
                key: Box::new(self._go_inst_ty(key, subst, wildcard_len)),
                value: Box::new(self._go_inst_ty(value, subst, wildcard_len)),
            },
            tast::Ty::TParam { name } => {
                if let Some(ty) = subst.get(name) {
                    ty.clone()
                } else {
                    let new_ty = self.fresh_ty_var();
                    subst.insert(name.clone(), new_ty.clone());
                    new_ty
                }
            }
            tast::Ty::TFunc { params, ret_ty } => {
                let params = params
                    .iter()
                    .map(|ty| self._go_inst_ty(ty, subst, wildcard_len))
                    .collect::<Vec<_>>();
                let ret_ty = Box::new(self._go_inst_ty(ret_ty, subst, wildcard_len));
                tast::Ty::TFunc { params, ret_ty }
            }
        }
    }

    pub(crate) fn subst_ty(
        &mut self,
        diagnostics: &mut Diagnostics,
        ty: &tast::Ty,
        origin: Option<TextRange>,
    ) -> tast::Ty {
        match ty {
            tast::Ty::TVar(v) => {
                if let Some(value) = self.uni.probe_value(*v) {
                    self.subst_ty(diagnostics, &value, origin)
                } else {
                    let diagnostic_origin = origin
                        .or_else(|| self.origin_for_unresolved_type_var(*v))
                        .or_else(|| diagnostics.iter().filter_map(|d| d.range()).last());
                    if self
                        .reported_unresolved_type_origins
                        .insert(diagnostic_origin.map(|range| range.start()))
                    {
                        diagnostics.push(
                            Diagnostic::new(
                                Stage::Typer,
                                Severity::Error,
                                "Could not infer type".to_string(),
                            )
                            .with_range(diagnostic_origin),
                        );
                    }
                    tast::Ty::TVar(*v)
                }
            }
            tast::Ty::TUnit => tast::Ty::TUnit,
            tast::Ty::TBool => tast::Ty::TBool,
            tast::Ty::TInt8 => tast::Ty::TInt8,
            tast::Ty::TInt16 => tast::Ty::TInt16,
            tast::Ty::TInt32 => tast::Ty::TInt32,
            tast::Ty::TInt64 => tast::Ty::TInt64,
            tast::Ty::TUint8 => tast::Ty::TUint8,
            tast::Ty::TUint16 => tast::Ty::TUint16,
            tast::Ty::TUint32 => tast::Ty::TUint32,
            tast::Ty::TUint64 => tast::Ty::TUint64,
            tast::Ty::TFloat32 => tast::Ty::TFloat32,
            tast::Ty::TFloat64 => tast::Ty::TFloat64,
            tast::Ty::TString => tast::Ty::TString,
            tast::Ty::TChar => tast::Ty::TChar,
            tast::Ty::TTuple { typs } => {
                let typs = typs
                    .iter()
                    .map(|ty| self.subst_ty(diagnostics, ty, origin))
                    .collect();
                tast::Ty::TTuple { typs }
            }
            tast::Ty::TEnum { name } => tast::Ty::TEnum { name: name.clone() },
            tast::Ty::TStruct { name } => tast::Ty::TStruct { name: name.clone() },
            tast::Ty::TDyn { trait_name } => tast::Ty::TDyn {
                trait_name: trait_name.clone(),
            },
            tast::Ty::TApp { ty, args } => tast::Ty::TApp {
                ty: Box::new(self.subst_ty(diagnostics, ty, origin)),
                args: args
                    .iter()
                    .map(|arg| self.subst_ty(diagnostics, arg, origin))
                    .collect(),
            },
            tast::Ty::TArray { len, elem } => tast::Ty::TArray {
                len: self.resolve_array_len(*len),
                elem: Box::new(self.subst_ty(diagnostics, elem, origin)),
            },
            tast::Ty::TSlice { elem } => tast::Ty::TSlice {
                elem: Box::new(self.subst_ty(diagnostics, elem, origin)),
            },
            tast::Ty::TVec { elem } => tast::Ty::TVec {
                elem: Box::new(self.subst_ty(diagnostics, elem, origin)),
            },
            tast::Ty::TRef { elem } => tast::Ty::TRef {
                elem: Box::new(self.subst_ty(diagnostics, elem, origin)),
            },
            tast::Ty::THashMap { key, value } => tast::Ty::THashMap {
                key: Box::new(self.subst_ty(diagnostics, key, origin)),
                value: Box::new(self.subst_ty(diagnostics, value, origin)),
            },
            tast::Ty::TFunc { params, ret_ty } => {
                let params = params
                    .iter()
                    .map(|ty| self.subst_ty(diagnostics, ty, origin))
                    .collect();
                let ret_ty = Box::new(self.subst_ty(diagnostics, ret_ty, origin));
                tast::Ty::TFunc { params, ret_ty }
            }
            tast::Ty::TParam { name } => tast::Ty::TParam { name: name.clone() },
        }
    }

    pub(crate) fn subst_ty_silent(&mut self, ty: &tast::Ty) -> tast::Ty {
        match ty {
            tast::Ty::TVar(v) => self
                .uni
                .probe_value(*v)
                .as_ref()
                .map(|value| self.subst_ty_silent(value))
                .unwrap_or(tast::Ty::TVar(*v)),
            tast::Ty::TUnit => tast::Ty::TUnit,
            tast::Ty::TBool => tast::Ty::TBool,
            tast::Ty::TInt8 => tast::Ty::TInt8,
            tast::Ty::TInt16 => tast::Ty::TInt16,
            tast::Ty::TInt32 => tast::Ty::TInt32,
            tast::Ty::TInt64 => tast::Ty::TInt64,
            tast::Ty::TUint8 => tast::Ty::TUint8,
            tast::Ty::TUint16 => tast::Ty::TUint16,
            tast::Ty::TUint32 => tast::Ty::TUint32,
            tast::Ty::TUint64 => tast::Ty::TUint64,
            tast::Ty::TFloat32 => tast::Ty::TFloat32,
            tast::Ty::TFloat64 => tast::Ty::TFloat64,
            tast::Ty::TString => tast::Ty::TString,
            tast::Ty::TChar => tast::Ty::TChar,
            tast::Ty::TTuple { typs } => tast::Ty::TTuple {
                typs: typs.iter().map(|ty| self.subst_ty_silent(ty)).collect(),
            },
            tast::Ty::TEnum { name } => tast::Ty::TEnum { name: name.clone() },
            tast::Ty::TStruct { name } => tast::Ty::TStruct { name: name.clone() },
            tast::Ty::TDyn { trait_name } => tast::Ty::TDyn {
                trait_name: trait_name.clone(),
            },
            tast::Ty::TApp { ty, args } => tast::Ty::TApp {
                ty: Box::new(self.subst_ty_silent(ty)),
                args: args.iter().map(|arg| self.subst_ty_silent(arg)).collect(),
            },
            tast::Ty::TArray { len, elem } => tast::Ty::TArray {
                len: self.resolve_array_len(*len),
                elem: Box::new(self.subst_ty_silent(elem)),
            },
            tast::Ty::TSlice { elem } => tast::Ty::TSlice {
                elem: Box::new(self.subst_ty_silent(elem)),
            },
            tast::Ty::TVec { elem } => tast::Ty::TVec {
                elem: Box::new(self.subst_ty_silent(elem)),
            },
            tast::Ty::TRef { elem } => tast::Ty::TRef {
                elem: Box::new(self.subst_ty_silent(elem)),
            },
            tast::Ty::THashMap { key, value } => tast::Ty::THashMap {
                key: Box::new(self.subst_ty_silent(key)),
                value: Box::new(self.subst_ty_silent(value)),
            },
            tast::Ty::TFunc { params, ret_ty } => tast::Ty::TFunc {
                params: params.iter().map(|ty| self.subst_ty_silent(ty)).collect(),
                ret_ty: Box::new(self.subst_ty_silent(ret_ty)),
            },
            tast::Ty::TParam { name } => tast::Ty::TParam { name: name.clone() },
        }
    }

    fn subst_pat(&mut self, diagnostics: &mut Diagnostics, p: tast::Pat) -> tast::Pat {
        match p {
            tast::Pat::PVar { name, ty, astptr } => {
                let origin = astptr.as_ref().map(|ptr| ptr.text_range());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                tast::Pat::PVar {
                    name: name.clone(),
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Pat::PPrim { value, ty, astptr } => {
                let origin = astptr.as_ref().map(|ptr| ptr.text_range());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                tast::Pat::PPrim {
                    value,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Pat::PConstr {
                constructor,
                args,
                ty,
                astptr,
            } => {
                let origin = astptr
                    .as_ref()
                    .map(|ptr| ptr.text_range())
                    .or_else(|| args.first().and_then(pat_origin));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let args = args
                    .into_iter()
                    .map(|arg| self.subst_pat(diagnostics, arg))
                    .collect::<Vec<_>>();
                tast::Pat::PConstr {
                    constructor,
                    args,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Pat::PTuple { items, ty, astptr } => {
                let origin = astptr
                    .as_ref()
                    .map(|ptr| ptr.text_range())
                    .or_else(|| items.first().and_then(pat_origin));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let items = items
                    .into_iter()
                    .map(|item| self.subst_pat(diagnostics, item))
                    .collect::<Vec<_>>();
                tast::Pat::PTuple {
                    items,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Pat::PWild { ty, astptr } => {
                let origin = astptr.as_ref().map(|ptr| ptr.text_range());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                tast::Pat::PWild {
                    ty: ty.clone(),
                    astptr,
                }
            }
        }
    }

    pub fn subst_block(
        &mut self,
        diagnostics: &mut Diagnostics,
        block: tast::Block,
    ) -> tast::Block {
        let stmts = block
            .stmts
            .into_iter()
            .map(|stmt| match stmt {
                tast::Stmt::Let(stmt) => tast::Stmt::Let(tast::LetStmt {
                    is_mut: stmt.is_mut,
                    pat: self.subst_pat(diagnostics, stmt.pat),
                    value: Box::new(self.subst(diagnostics, *stmt.value)),
                }),
                tast::Stmt::Assign(stmt) => tast::Stmt::Assign(tast::AssignStmt {
                    target: Box::new(self.subst(diagnostics, *stmt.target)),
                    value: Box::new(self.subst(diagnostics, *stmt.value)),
                }),
                tast::Stmt::Expr(stmt) => tast::Stmt::Expr(tast::ExprStmt {
                    expr: self.subst(diagnostics, stmt.expr),
                }),
            })
            .collect();
        let tail = block
            .tail
            .map(|tail| Box::new(self.subst(diagnostics, *tail)));
        tast::Block { stmts, tail }
    }

    pub fn subst(&mut self, diagnostics: &mut Diagnostics, e: tast::Expr) -> tast::Expr {
        match e {
            tast::Expr::EVar { name, ty, astptr } => {
                let origin = astptr.as_ref().map(|ptr| ptr.text_range());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                tast::Expr::EVar {
                    name,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Expr::ECallable {
                name,
                body,
                ty,
                astptr,
            } => {
                let origin = astptr.as_ref().map(|ptr| ptr.text_range());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                tast::Expr::ECallable {
                    name,
                    body,
                    ty,
                    astptr,
                }
            }
            tast::Expr::EPrim { value, ty } => {
                let ty = self.subst_ty(diagnostics, &ty, None);
                tast::Expr::EPrim { value, ty }
            }
            tast::Expr::EConstr {
                constructor,
                args,
                ty,
            } => {
                let origin = args.first().and_then(expr_origin);
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let args = args
                    .into_iter()
                    .map(|arg| self.subst(diagnostics, arg))
                    .collect::<Vec<_>>();
                tast::Expr::EConstr {
                    constructor,
                    args,
                    ty,
                }
            }
            tast::Expr::ETuple { items, ty } => {
                let origin = items.first().and_then(expr_origin);
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let items = items
                    .into_iter()
                    .map(|item| self.subst(diagnostics, item))
                    .collect::<Vec<_>>();
                tast::Expr::ETuple {
                    items,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EArray { items, ty } => {
                let origin = items.first().and_then(expr_origin);
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let items = items
                    .into_iter()
                    .map(|item| self.subst(diagnostics, item))
                    .collect::<Vec<_>>();
                tast::Expr::EArray {
                    items,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EClosure {
                params,
                body,
                ty,
                captures,
            } => {
                let outer_origin = expr_origin(body.as_ref());
                let ty = self.subst_ty(diagnostics, &ty, outer_origin);
                let params = params
                    .into_iter()
                    .map(|param| tast::ClosureParam {
                        name: param.name,
                        ty: self.subst_ty(
                            diagnostics,
                            &param.ty,
                            param
                                .astptr
                                .as_ref()
                                .map(|ptr| ptr.text_range())
                                .or(outer_origin),
                        ),
                        astptr: param.astptr,
                    })
                    .collect();
                let body = Box::new(self.subst(diagnostics, *body));
                let captures = captures
                    .into_iter()
                    .map(|(name, cap_ty)| (name, self.subst_ty(diagnostics, &cap_ty, outer_origin)))
                    .collect();
                tast::Expr::EClosure {
                    params,
                    body,
                    ty: ty.clone(),
                    captures,
                }
            }
            tast::Expr::EBlock { block, ty } => {
                let origin = block
                    .tail
                    .as_ref()
                    .and_then(|tail| expr_origin(tail.as_ref()));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let block = Box::new(self.subst_block(diagnostics, *block));
                tast::Expr::EBlock {
                    block,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EMatch {
                expr,
                arms,
                ty,
                astptr,
            } => {
                let origin = astptr
                    .as_ref()
                    .map(|ptr| ptr.text_range())
                    .or_else(|| expr_origin(expr.as_ref()));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let expr = Box::new(self.subst(diagnostics, *expr));
                let arms = arms
                    .into_iter()
                    .map(|arm| tast::Arm {
                        pat: self.subst_pat(diagnostics, arm.pat),
                        body: self.subst(diagnostics, arm.body),
                    })
                    .collect::<Vec<_>>();
                tast::Expr::EMatch {
                    expr,
                    arms,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Expr::EIf {
                cond,
                then_branch,
                else_branch,
                ty,
            } => {
                let origin = expr_origin(cond.as_ref())
                    .or_else(|| expr_origin(then_branch.as_ref()))
                    .or_else(|| expr_origin(else_branch.as_ref()));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let cond = Box::new(self.subst(diagnostics, *cond));
                let then_branch = Box::new(self.subst(diagnostics, *then_branch));
                let else_branch = Box::new(self.subst(diagnostics, *else_branch));
                tast::Expr::EIf {
                    cond,
                    then_branch,
                    else_branch,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EWhile { cond, body, ty } => {
                let origin = expr_origin(cond.as_ref()).or_else(|| expr_origin(body.as_ref()));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let cond = Box::new(self.subst(diagnostics, *cond));
                let body = Box::new(self.subst(diagnostics, *body));
                tast::Expr::EWhile {
                    cond,
                    body,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EFor {
                pat,
                iterator,
                body,
                ty,
            } => {
                let origin = pat_origin(&pat)
                    .or_else(|| expr_origin(iterator.as_ref()))
                    .or_else(|| expr_origin(body.as_ref()));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let pat = self.subst_pat(diagnostics, pat);
                let iterator = Box::new(self.subst(diagnostics, *iterator));
                let body = Box::new(self.subst(diagnostics, *body));
                tast::Expr::EFor {
                    pat,
                    iterator,
                    body,
                    ty,
                }
            }
            tast::Expr::EBreak { ty } => {
                let ty = self.subst_ty(diagnostics, &ty, None);
                tast::Expr::EBreak { ty }
            }
            tast::Expr::EContinue { ty } => {
                let ty = self.subst_ty(diagnostics, &ty, None);
                tast::Expr::EContinue { ty }
            }
            tast::Expr::EReturn { expr, ty } => {
                let origin = expr.as_deref().and_then(expr_origin);
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let expr = expr.map(|expr| Box::new(self.subst(diagnostics, *expr)));
                tast::Expr::EReturn { expr, ty }
            }
            tast::Expr::EGo { expr, ty } => {
                let origin = expr_origin(expr.as_ref());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let expr = Box::new(self.subst(diagnostics, *expr));
                tast::Expr::EGo {
                    expr,
                    ty: ty.clone(),
                }
            }
            tast::Expr::ECall { func, args, ty } => {
                let origin = expr_origin(func.as_ref())
                    .or_else(|| args.first().and_then(expr_origin))
                    .or_else(|| args.last().and_then(expr_origin));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let func = Box::new(self.subst(diagnostics, *func));
                let args = args
                    .into_iter()
                    .map(|arg| self.subst(diagnostics, arg))
                    .collect::<Vec<_>>();
                tast::Expr::ECall {
                    func,
                    args,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EUnary {
                op,
                expr,
                ty,
                resolution,
            } => {
                let origin = expr_origin(expr.as_ref());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let expr = Box::new(self.subst(diagnostics, *expr));
                tast::Expr::EUnary {
                    op,
                    expr,
                    ty: ty.clone(),
                    resolution,
                }
            }
            tast::Expr::EBinary {
                op,
                lhs,
                rhs,
                ty,
                resolution,
            } => {
                let origin = expr_origin(lhs.as_ref()).or_else(|| expr_origin(rhs.as_ref()));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let lhs = Box::new(self.subst(diagnostics, *lhs));
                let rhs = Box::new(self.subst(diagnostics, *rhs));
                tast::Expr::EBinary {
                    op,
                    lhs,
                    rhs,
                    ty: ty.clone(),
                    resolution,
                }
            }
            tast::Expr::EProj { tuple, index, ty } => {
                let origin = expr_origin(tuple.as_ref());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let tuple = Box::new(self.subst(diagnostics, *tuple));
                tast::Expr::EProj {
                    tuple,
                    index,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EField {
                expr,
                field_name,
                ty,
                astptr,
            } => {
                let origin = astptr
                    .as_ref()
                    .map(|ptr| ptr.text_range())
                    .or_else(|| expr_origin(expr.as_ref()));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let expr = Box::new(self.subst(diagnostics, *expr));
                tast::Expr::EField {
                    expr,
                    field_name,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Expr::EIndex {
                base,
                index,
                ty,
                astptr,
            } => {
                let origin = astptr
                    .as_ref()
                    .map(|ptr| ptr.text_range())
                    .or_else(|| expr_origin(base.as_ref()))
                    .or_else(|| expr_origin(index.as_ref()));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let base = Box::new(self.subst(diagnostics, *base));
                let index = Box::new(self.subst(diagnostics, *index));
                tast::Expr::EIndex {
                    base,
                    index,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Expr::ETraitMethod {
                trait_name,
                method_name,
                ty,
                astptr,
            } => {
                let origin = astptr.as_ref().map(|ptr| ptr.text_range());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                tast::Expr::ETraitMethod {
                    trait_name,
                    method_name,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Expr::EDynTraitMethod {
                trait_name,
                method_name,
                ty,
                astptr,
            } => {
                let origin = astptr.as_ref().map(|ptr| ptr.text_range());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                tast::Expr::EDynTraitMethod {
                    trait_name,
                    method_name,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Expr::EInherentMethod {
                receiver_ty,
                method_name,
                ty,
                astptr,
            } => {
                let origin = astptr.as_ref().map(|ptr| ptr.text_range());
                let receiver_ty = self.subst_ty(diagnostics, &receiver_ty, origin);
                let ty = self.subst_ty(diagnostics, &ty, origin);
                tast::Expr::EInherentMethod {
                    receiver_ty,
                    method_name,
                    ty,
                    astptr,
                }
            }
            tast::Expr::EToDyn {
                trait_name,
                for_ty,
                expr,
                ty,
                astptr,
            } => {
                let origin = astptr
                    .as_ref()
                    .map(|ptr| ptr.text_range())
                    .or_else(|| expr_origin(expr.as_ref()));
                let for_ty = self.subst_ty(diagnostics, &for_ty, origin);
                let expr = Box::new(self.subst(diagnostics, *expr));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                if let (
                    tast::Ty::TDyn {
                        trait_name: from_trait_name,
                    },
                    tast::Ty::TDyn {
                        trait_name: to_trait_name,
                    },
                ) = (&for_ty, &ty)
                    && from_trait_name == to_trait_name
                    && to_trait_name == &trait_name.0
                {
                    return *expr;
                }
                tast::Expr::EToDyn {
                    trait_name,
                    for_ty,
                    expr,
                    ty,
                    astptr,
                }
            }
        }
    }
}
