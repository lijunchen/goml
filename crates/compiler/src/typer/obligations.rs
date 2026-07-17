use std::collections::{HashMap, HashSet, VecDeque};

use text_size::TextRange;

use crate::{hir, tast};

pub(crate) type ObligationId = usize;

#[derive(Debug, Clone)]
pub(crate) struct Obligation {
    pub id: ObligationId,
    pub predicate: Predicate,
    pub cause: ObligationCause,
}

#[derive(Debug, Clone)]
pub(crate) enum Predicate {
    Trait(TraitGoal),
    TypeEquality(TypeEqualityGoal),
    Method(MethodGoal),
    Projection(ProjectionGoal),
    Coerce(CoercionGoal),
    Operation(OperationGoal),
}

#[derive(Debug, Clone)]
pub(crate) struct TypeEqualityGoal {
    pub lhs: tast::Ty,
    pub rhs: tast::Ty,
}

pub(crate) struct ObligationWorklist {
    ready: VecDeque<ObligationId>,
    queued: HashSet<ObligationId>,
    obligations: HashMap<ObligationId, Obligation>,
    waiting_by_var: HashMap<tast::TypeVar, HashSet<ObligationId>>,
    waiting_on: HashMap<ObligationId, HashSet<tast::TypeVar>>,
}

impl ObligationWorklist {
    pub(crate) fn new(obligations: Vec<Obligation>) -> Self {
        let mut worklist = Self {
            ready: VecDeque::new(),
            queued: HashSet::new(),
            obligations: HashMap::new(),
            waiting_by_var: HashMap::new(),
            waiting_on: HashMap::new(),
        };
        for obligation in obligations {
            worklist.push(obligation);
        }
        worklist
    }

    pub(crate) fn push(&mut self, obligation: Obligation) {
        let id = obligation.id;
        self.obligations.insert(id, obligation);
        if self.queued.insert(id) {
            self.ready.push_back(id);
        }
    }

    pub(crate) fn pop(&mut self) -> Option<Obligation> {
        while let Some(id) = self.ready.pop_front() {
            self.queued.remove(&id);
            if let Some(obligation) = self.obligations.remove(&id) {
                return Some(obligation);
            }
        }
        None
    }

    pub(crate) fn defer(
        &mut self,
        obligation: Obligation,
        variables: impl IntoIterator<Item = tast::TypeVar>,
    ) {
        let id = obligation.id;
        self.clear_waiting(id);
        let variables = variables.into_iter().collect::<HashSet<_>>();
        for variable in &variables {
            self.waiting_by_var.entry(*variable).or_default().insert(id);
        }
        self.waiting_on.insert(id, variables);
        self.obligations.insert(id, obligation);
    }

    pub(crate) fn wake(&mut self, variables: impl IntoIterator<Item = tast::TypeVar>) {
        let mut ids = HashSet::new();
        for variable in variables {
            if let Some(waiting) = self.waiting_by_var.remove(&variable) {
                ids.extend(waiting);
            }
        }
        for id in ids {
            self.clear_waiting(id);
            if self.obligations.contains_key(&id) && self.queued.insert(id) {
                self.ready.push_back(id);
            }
        }
    }

    pub(crate) fn drain_waiting(&mut self) -> Vec<Obligation> {
        self.ready.clear();
        self.queued.clear();
        self.waiting_by_var.clear();
        self.waiting_on.clear();
        let mut obligations = self
            .obligations
            .drain()
            .map(|(_, value)| value)
            .collect::<Vec<_>>();
        obligations.sort_by_key(|obligation| obligation.id);
        obligations
    }

    fn clear_waiting(&mut self, id: ObligationId) {
        let Some(variables) = self.waiting_on.remove(&id) else {
            return;
        };
        for variable in variables {
            let remove_entry = if let Some(waiting) = self.waiting_by_var.get_mut(&variable) {
                waiting.remove(&id);
                waiting.is_empty()
            } else {
                false
            };
            if remove_entry {
                self.waiting_by_var.remove(&variable);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct TraitGoal {
    pub trait_ref: tast::TraitRef,
    pub for_ty: tast::Ty,
}

#[derive(Debug, Clone)]
pub(crate) struct MethodGoal {
    pub call_expr_id: hir::ExprId,
    pub func_expr_id: hir::ExprId,
    pub receiver_expr_id: hir::ExprId,
    pub receiver_ty: tast::Ty,
    pub method: tast::TastIdent,
    pub call_site_type: tast::Ty,
    pub args: Vec<hir::ExprId>,
    pub in_scope_traits: Vec<tast::TastIdent>,
}

#[derive(Debug, Clone)]
pub(crate) enum ProjectionGoal {
    AssociatedType {
        trait_ref: tast::TraitRef,
        for_ty: tast::Ty,
        name: tast::TastIdent,
        result_ty: tast::Ty,
    },
    Field {
        base_ty: tast::Ty,
        field: tast::TastIdent,
        result_ty: tast::Ty,
    },
    Tuple {
        tuple_ty: tast::Ty,
        index: usize,
        result_ty: tast::Ty,
    },
}

#[derive(Debug, Clone, Default)]
pub(crate) struct ParamEnv {
    predicates: Vec<crate::env::TypePredicate>,
}

impl ParamEnv {
    pub(crate) fn from_predicates(predicates: &[crate::env::TypePredicate]) -> Self {
        Self {
            predicates: predicates.to_vec(),
        }
    }

    pub(crate) fn predicates(&self) -> &[crate::env::TypePredicate] {
        &self.predicates
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ObligationCause {
    pub span: Option<TextRange>,
    pub kind: ObligationCauseKind,
    pub parent: Option<ObligationId>,
}

impl ObligationCause {
    pub(crate) fn new(span: Option<TextRange>, kind: ObligationCauseKind) -> Self {
        Self {
            span,
            kind,
            parent: None,
        }
    }

    pub(crate) fn with_parent(mut self, parent: ObligationId) -> Self {
        self.parent = Some(parent);
        self
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ObligationCauseKind {
    FunctionBound,
    ImplBound,
    MethodCall,
    ForLoop,
    Coercion,
    Projection,
    Operation,
}

impl ObligationCauseKind {
    pub(crate) fn description(self) -> &'static str {
        match self {
            Self::FunctionBound => "a function bound",
            Self::ImplBound => "an implementation bound",
            Self::MethodCall => "a method call",
            Self::ForLoop => "a for loop",
            Self::Coercion => "a trait-object coercion",
            Self::Projection => "a projection",
            Self::Operation => "an operator",
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct InstantiatedScheme {
    pub ty: tast::Ty,
    pub obligations: Vec<(Predicate, ObligationCause)>,
}

#[derive(Debug, Clone)]
pub(crate) struct CoercionGoal {
    pub expr_id: hir::ExprId,
    pub from_ty: tast::Ty,
    pub to_ty: tast::Ty,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ArithmeticKind {
    NumericOrString,
    Numeric,
    Integer,
}

#[derive(Debug, Clone)]
pub(crate) enum OperationGoal {
    Arithmetic {
        kind: ArithmeticKind,
        ty: tast::Ty,
        operator: &'static str,
    },
    Comparison {
        operator: common_defs::BinaryOp,
        lhs_ty: tast::Ty,
        rhs_ty: tast::Ty,
    },
}
