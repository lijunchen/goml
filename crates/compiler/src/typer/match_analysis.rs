use std::collections::{HashMap, HashSet};

use crate::common::{Constructor as ValueConstructor, Prim};
use crate::env::PackageTypeEnv;
use crate::tast::{self, Pat, Ty};

use super::type_ops::substitute_ty_params;
use super::util;

const MAX_INHABITANCE_DEPTH: usize = 128;
const MAX_INHABITANCE_STEPS: usize = 1024;

#[derive(Debug, Clone)]
pub(crate) struct MatchAnalysis {
    pub(crate) exhaustive: bool,
    pub(crate) useful_arms: Vec<bool>,
    pub(crate) witnesses: Vec<Witness>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ConstructorKey {
    Unit,
    Bool(bool),
    Enum {
        type_name: String,
        index: usize,
    },
    Tuple(usize),
    Struct(String),
    Array(usize),
    Sequence {
        prefix: usize,
        suffix: usize,
        has_rest: bool,
    },
    SequenceEmpty,
    SequenceCons,
    Scalar {
        kind: ScalarKind,
        lo: i128,
        hi: i128,
    },
    Float32(u32),
    Float64(u64),
    String(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ScalarKind {
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Char,
}

#[derive(Debug, Clone)]
enum ConstructorKind {
    Unit,
    Bool(bool),
    Enum {
        type_name: String,
        variant: String,
    },
    Tuple,
    Struct {
        type_name: String,
        fields: Vec<String>,
    },
    Array,
    SequenceEmpty,
    SequenceCons,
    Literal(String),
}

#[derive(Debug, Clone)]
pub(crate) struct Constructor {
    key: ConstructorKey,
    kind: ConstructorKind,
    field_tys: Vec<Ty>,
}

#[derive(Debug, Clone)]
struct DeconstructedPat {
    constructor: Option<Constructor>,
    fields: Vec<DeconstructedPat>,
    ty: Ty,
}

impl DeconstructedPat {
    fn wild(ty: Ty) -> Self {
        Self {
            constructor: None,
            fields: Vec::new(),
            ty,
        }
    }

    fn from_tast(genv: &PackageTypeEnv, pat: &Pat) -> Self {
        match pat {
            Pat::PVar { ty, .. } | Pat::PWild { ty, .. } => Self::wild(ty.clone()),
            Pat::PPrim { value, ty, .. } => Self {
                constructor: Some(primitive_constructor(value, ty)),
                fields: Vec::new(),
                ty: ty.clone(),
            },
            Pat::PConstr {
                constructor,
                args,
                ty,
                ..
            } => {
                let fields = args
                    .iter()
                    .map(|arg| Self::from_tast(genv, arg))
                    .collect::<Vec<_>>();
                let constructor = value_constructor(genv, constructor, ty, &fields);
                Self {
                    constructor: Some(constructor),
                    fields,
                    ty: ty.clone(),
                }
            }
            Pat::PTuple { items, ty, .. } => {
                let fields = items
                    .iter()
                    .map(|item| Self::from_tast(genv, item))
                    .collect::<Vec<_>>();
                Self {
                    constructor: Some(Constructor {
                        key: ConstructorKey::Tuple(fields.len()),
                        kind: ConstructorKind::Tuple,
                        field_tys: fields.iter().map(|field| field.ty.clone()).collect(),
                    }),
                    fields,
                    ty: ty.clone(),
                }
            }
            Pat::PArray {
                prefix,
                rest,
                suffix,
                ty,
                ..
            } => sequence_pattern(genv, prefix, rest.as_ref(), suffix, ty),
            Pat::PAlias { pat, .. } => Self::from_tast(genv, pat),
            Pat::POr { pats, ty, .. } => pats
                .first()
                .map(|pat| Self::from_tast(genv, pat))
                .unwrap_or_else(|| Self::wild(ty.clone())),
            Pat::PRange {
                start,
                end,
                inclusive,
                ty,
                ..
            } => {
                let display = format!("{}{}{}", start, if *inclusive { "..=" } else { ".." }, end);
                let Some((kind, lo)) = scalar_prim(start) else {
                    return Self::wild(ty.clone());
                };
                let Some((end_kind, mut hi)) = scalar_prim(end) else {
                    return Self::wild(ty.clone());
                };
                if kind != end_kind {
                    return Self::wild(ty.clone());
                }
                if !inclusive {
                    hi -= 1;
                }
                Self {
                    constructor: Some(Constructor {
                        key: ConstructorKey::Scalar { kind, lo, hi },
                        kind: ConstructorKind::Literal(display),
                        field_tys: Vec::new(),
                    }),
                    fields: Vec::new(),
                    ty: ty.clone(),
                }
            }
        }
    }

    fn from_witness(witness: &Witness) -> Self {
        match witness {
            Witness::Wild { ty } => Self::wild(ty.clone()),
            Witness::Constructor {
                constructor,
                fields,
                ty,
            } => Self {
                constructor: Some(constructor.clone()),
                fields: fields.iter().map(Self::from_witness).collect(),
                ty: ty.clone(),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Witness {
    Wild {
        ty: Ty,
    },
    Constructor {
        constructor: Constructor,
        fields: Vec<Witness>,
        ty: Ty,
    },
}

impl std::fmt::Display for Witness {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Witness::Wild { .. } => formatter.write_str("_"),
            Witness::Constructor {
                constructor,
                fields,
                ..
            } => match &constructor.kind {
                ConstructorKind::Unit => formatter.write_str("()"),
                ConstructorKind::Bool(value) => write!(formatter, "{value}"),
                ConstructorKind::Enum { type_name, variant } => {
                    let type_name = short_name(type_name);
                    if fields.is_empty() {
                        write!(formatter, "{type_name}::{variant}")
                    } else {
                        write!(formatter, "{type_name}::{variant}(")?;
                        format_witnesses(formatter, fields)?;
                        formatter.write_str(")")
                    }
                }
                ConstructorKind::Tuple => {
                    formatter.write_str("(")?;
                    format_witnesses(formatter, fields)?;
                    if fields.len() == 1 {
                        formatter.write_str(",")?;
                    }
                    formatter.write_str(")")
                }
                ConstructorKind::Struct {
                    type_name,
                    fields: names,
                } => {
                    write!(formatter, "{} {{ ", short_name(type_name))?;
                    for (index, field) in fields.iter().enumerate() {
                        if index > 0 {
                            formatter.write_str(", ")?;
                        }
                        let name = names
                            .get(index)
                            .cloned()
                            .unwrap_or_else(|| format!("_{index}"));
                        write!(formatter, "{name}: {field}")?;
                    }
                    formatter.write_str(" }")
                }
                ConstructorKind::Array => {
                    formatter.write_str("[")?;
                    format_witnesses(formatter, fields)?;
                    formatter.write_str("]")
                }
                ConstructorKind::SequenceEmpty => formatter.write_str("[]"),
                ConstructorKind::SequenceCons => {
                    formatter.write_str("[")?;
                    if let Some(head) = fields.first() {
                        write!(formatter, "{head}")?;
                    }
                    formatter.write_str(", ..]")
                }
                ConstructorKind::Literal(value) => formatter.write_str(value),
            },
        }
    }
}

impl Witness {
    fn contains_sequence(&self) -> bool {
        match self {
            Self::Wild { .. } => false,
            Self::Constructor {
                constructor,
                fields,
                ..
            } => {
                matches!(constructor.key, ConstructorKey::Sequence { .. })
                    || fields.iter().any(Self::contains_sequence)
            }
        }
    }
}

fn format_witnesses(
    formatter: &mut std::fmt::Formatter<'_>,
    witnesses: &[Witness],
) -> std::fmt::Result {
    for (index, witness) in witnesses.iter().enumerate() {
        if index > 0 {
            formatter.write_str(", ")?;
        }
        write!(formatter, "{witness}")?;
    }
    Ok(())
}

fn short_name(name: &str) -> &str {
    name.rsplit("::").next().unwrap_or(name)
}

pub(crate) fn analyze(
    genv: &PackageTypeEnv,
    scrutinee_ty: &Ty,
    arms: &[tast::Arm],
) -> MatchAnalysis {
    let mut matrix = Vec::<Vec<DeconstructedPat>>::new();
    let mut useful_arms = Vec::with_capacity(arms.len());
    for arm in arms {
        let expanded = expand_pattern(&arm.pat);
        let guard = guard_value(arm.guard.as_ref());
        let useful = guard != Some(false)
            && expanded.iter().any(|pattern| {
                let row = vec![DeconstructedPat::from_tast(genv, pattern)];
                useful(genv, &matrix, &row).is_some()
            });
        useful_arms.push(useful);
        if guard == Some(true) {
            matrix.extend(
                expanded
                    .iter()
                    .map(|pattern| vec![DeconstructedPat::from_tast(genv, pattern)]),
            );
        }
    }

    let mut witness_matrix = matrix.clone();
    let mut witnesses = Vec::new();
    while witnesses.len() < 8 {
        let candidate = vec![DeconstructedPat::wild(scrutinee_ty.clone())];
        let Some(mut found) = useful(genv, &witness_matrix, &candidate) else {
            break;
        };
        let Some(witness) = found.drain(..).next() else {
            break;
        };
        let contains_sequence = witness.contains_sequence();
        witness_matrix.push(vec![DeconstructedPat::from_witness(&witness)]);
        witnesses.push(witness);
        if contains_sequence {
            break;
        }
    }

    MatchAnalysis {
        exhaustive: witnesses.is_empty(),
        useful_arms,
        witnesses,
    }
}

pub(crate) fn is_irrefutable(genv: &PackageTypeEnv, pattern: &Pat) -> bool {
    let arms = [tast::Arm {
        pat: pattern.clone(),
        guard: None,
        body: tast::Expr::EPrim {
            value: Prim::unit(),
            ty: Ty::TUnit,
        },
    }];
    analyze(genv, &pattern.get_ty(), &arms).exhaustive
}

fn guard_value(guard: Option<&tast::Expr>) -> Option<bool> {
    match guard {
        None => Some(true),
        Some(tast::Expr::EPrim {
            value: Prim::Bool { value },
            ..
        }) => Some(*value),
        Some(_) => None,
    }
}

fn expand_pattern(pattern: &Pat) -> Vec<Pat> {
    match pattern {
        Pat::POr { pats, .. } => pats.iter().flat_map(expand_pattern).collect(),
        Pat::PConstr {
            constructor,
            args,
            ty,
            astptr,
        } => expand_pattern_lists(args)
            .into_iter()
            .map(|args| Pat::PConstr {
                constructor: constructor.clone(),
                args,
                ty: ty.clone(),
                astptr: *astptr,
            })
            .collect(),
        Pat::PTuple { items, ty, astptr } => expand_pattern_lists(items)
            .into_iter()
            .map(|items| Pat::PTuple {
                items,
                ty: ty.clone(),
                astptr: *astptr,
            })
            .collect(),
        Pat::PArray {
            prefix,
            rest,
            suffix,
            ty,
            astptr,
        } => {
            let prefixes = expand_pattern_lists(prefix);
            let suffixes = expand_pattern_lists(suffix);
            prefixes
                .into_iter()
                .flat_map(|prefix| {
                    suffixes.iter().cloned().map(move |suffix| Pat::PArray {
                        prefix: prefix.clone(),
                        rest: rest.clone(),
                        suffix,
                        ty: ty.clone(),
                        astptr: *astptr,
                    })
                })
                .collect()
        }
        Pat::PAlias {
            name,
            pat,
            ty,
            astptr,
        } => expand_pattern(pat)
            .into_iter()
            .map(|pat| Pat::PAlias {
                name: name.clone(),
                pat: Box::new(pat),
                ty: ty.clone(),
                astptr: *astptr,
            })
            .collect(),
        _ => vec![pattern.clone()],
    }
}

fn expand_pattern_lists(patterns: &[Pat]) -> Vec<Vec<Pat>> {
    let mut rows = vec![Vec::new()];
    for pattern in patterns {
        let alternatives = expand_pattern(pattern);
        rows = rows
            .into_iter()
            .flat_map(|row| {
                alternatives.iter().cloned().map(move |alternative| {
                    let mut next = row.clone();
                    next.push(alternative);
                    next
                })
            })
            .collect();
    }
    rows
}

fn sequence_pattern(
    genv: &PackageTypeEnv,
    prefix: &[Pat],
    rest: Option<&tast::ArrayPatRest>,
    suffix: &[Pat],
    ty: &Ty,
) -> DeconstructedPat {
    match ty {
        Ty::TArray { len, elem } => {
            let mut fields = (0..*len)
                .map(|_| DeconstructedPat::wild(elem.as_ref().clone()))
                .collect::<Vec<_>>();
            for (index, pat) in prefix.iter().enumerate().take(*len) {
                fields[index] = DeconstructedPat::from_tast(genv, pat);
            }
            for (offset, pat) in suffix.iter().rev().enumerate().take(*len) {
                fields[*len - offset - 1] = DeconstructedPat::from_tast(genv, pat);
            }
            DeconstructedPat {
                constructor: Some(Constructor {
                    key: ConstructorKey::Array(*len),
                    kind: ConstructorKind::Array,
                    field_tys: fields.iter().map(|field| field.ty.clone()).collect(),
                }),
                fields,
                ty: ty.clone(),
            }
        }
        Ty::TVec { .. } | Ty::TSlice { .. } => {
            let fields = prefix
                .iter()
                .chain(suffix.iter())
                .map(|pat| DeconstructedPat::from_tast(genv, pat))
                .collect::<Vec<_>>();
            DeconstructedPat {
                constructor: Some(Constructor {
                    key: ConstructorKey::Sequence {
                        prefix: prefix.len(),
                        suffix: suffix.len(),
                        has_rest: rest.is_some(),
                    },
                    kind: ConstructorKind::Array,
                    field_tys: fields.iter().map(|field| field.ty.clone()).collect(),
                }),
                fields,
                ty: ty.clone(),
            }
        }
        _ => DeconstructedPat::wild(ty.clone()),
    }
}

fn useful(
    genv: &PackageTypeEnv,
    matrix: &[Vec<DeconstructedPat>],
    candidate: &[DeconstructedPat],
) -> Option<Vec<Witness>> {
    if candidate.is_empty() {
        return matrix.is_empty().then(Vec::new);
    }

    let head = &candidate[0];
    let tail = &candidate[1..];
    if let Some(elem) = sequence_element_ty(&head.ty) {
        return useful_sequence(genv, matrix, candidate, elem);
    }
    if let Some((kind, domains)) = scalar_domains(&head.ty) {
        return useful_scalar(genv, matrix, candidate, kind, &domains);
    }
    if let Some(constructor) = &head.constructor {
        let specialized = specialize_matrix(matrix, constructor);
        let mut specialized_candidate = head.fields.clone();
        specialized_candidate.extend_from_slice(tail);
        let witnesses = useful(genv, &specialized, &specialized_candidate)?;
        let field_count = constructor.field_tys.len();
        let fields = witnesses[..field_count].to_vec();
        let mut result = vec![Witness::Constructor {
            constructor: constructor.clone(),
            fields,
            ty: head.ty.clone(),
        }];
        result.extend_from_slice(&witnesses[field_count..]);
        return Some(result);
    }

    let present = constructors_in_column(matrix);
    let domain = constructors_for_type(genv, &head.ty).map(|constructors| {
        constructors
            .into_iter()
            .filter(|constructor| constructor_is_inhabited(genv, constructor))
            .collect::<Vec<_>>()
    });
    if let Some(domain) = domain.as_ref()
        && domain
            .iter()
            .all(|constructor| present.contains(&constructor.key))
    {
        for constructor in domain.iter().cloned() {
            let specialized = specialize_matrix(matrix, &constructor);
            let mut specialized_candidate = constructor
                .field_tys
                .iter()
                .cloned()
                .map(DeconstructedPat::wild)
                .collect::<Vec<_>>();
            specialized_candidate.extend_from_slice(tail);
            if let Some(witnesses) = useful(genv, &specialized, &specialized_candidate) {
                let field_count = constructor.field_tys.len();
                let fields = witnesses[..field_count].to_vec();
                let mut result = vec![Witness::Constructor {
                    constructor,
                    fields,
                    ty: head.ty.clone(),
                }];
                result.extend_from_slice(&witnesses[field_count..]);
                return Some(result);
            }
        }
        return None;
    }

    let default = default_matrix(matrix);
    let tail_witnesses = useful(genv, &default, tail)?;
    let head_witness = domain
        .and_then(|domain| {
            domain
                .into_iter()
                .find(|constructor| !present.contains(&constructor.key))
        })
        .map(|constructor| Witness::Constructor {
            fields: constructor
                .field_tys
                .iter()
                .cloned()
                .map(|ty| Witness::Wild { ty })
                .collect(),
            constructor,
            ty: head.ty.clone(),
        })
        .unwrap_or_else(|| Witness::Wild {
            ty: head.ty.clone(),
        });
    let mut result = vec![head_witness];
    result.extend(tail_witnesses);
    Some(result)
}

fn useful_sequence(
    genv: &PackageTypeEnv,
    matrix: &[Vec<DeconstructedPat>],
    candidate: &[DeconstructedPat],
    elem: &Ty,
) -> Option<Vec<Witness>> {
    let head = &candidate[0];
    let tail = &candidate[1..];
    let candidate_shape = sequence_shape(head);
    let mut max_prefix = 0;
    let mut max_suffix = 0;
    let mut exact_lengths = HashSet::new();
    for pattern in matrix
        .iter()
        .filter_map(|row| row.first())
        .chain(std::iter::once(head))
    {
        let Some((prefix, suffix, has_rest)) = sequence_shape(pattern) else {
            continue;
        };
        if has_rest {
            max_prefix = max_prefix.max(prefix);
            max_suffix = max_suffix.max(suffix);
        } else {
            exact_lengths.insert(prefix + suffix);
        }
    }

    let stable_length = max_prefix + max_suffix;
    let mut large_length = stable_length + 1;
    while exact_lengths.contains(&large_length) {
        large_length += 1;
    }
    let mut lengths = (0..=stable_length).collect::<Vec<_>>();
    lengths.push(large_length);
    lengths.extend(exact_lengths);
    lengths.sort_unstable();
    lengths.dedup();

    if !type_is_inhabited_fresh(genv, elem) {
        lengths.retain(|len| *len == 0);
    }

    for len in lengths {
        if !sequence_matches_length(candidate_shape, len) {
            continue;
        }
        let specialized = matrix
            .iter()
            .filter_map(|row| specialize_sequence_row(row, len, elem))
            .collect::<Vec<_>>();
        let mut specialized_candidate = specialize_sequence_pattern(head, len, elem)?;
        specialized_candidate.extend_from_slice(tail);
        let Some(witnesses) = useful(genv, &specialized, &specialized_candidate) else {
            continue;
        };
        let fields = witnesses[..len].to_vec();
        let mut result = vec![Witness::Constructor {
            constructor: Constructor {
                key: ConstructorKey::Sequence {
                    prefix: len,
                    suffix: 0,
                    has_rest: false,
                },
                kind: ConstructorKind::Array,
                field_tys: (0..len).map(|_| elem.clone()).collect(),
            },
            fields,
            ty: head.ty.clone(),
        }];
        result.extend_from_slice(&witnesses[len..]);
        return Some(result);
    }
    None
}

fn sequence_element_ty(ty: &Ty) -> Option<&Ty> {
    match ty {
        Ty::TVec { elem } | Ty::TSlice { elem } => Some(elem),
        _ => None,
    }
}

fn sequence_shape(pattern: &DeconstructedPat) -> Option<(usize, usize, bool)> {
    match pattern
        .constructor
        .as_ref()
        .map(|constructor| &constructor.key)
    {
        Some(ConstructorKey::Sequence {
            prefix,
            suffix,
            has_rest,
        }) => Some((*prefix, *suffix, *has_rest)),
        _ => None,
    }
}

fn sequence_matches_length(shape: Option<(usize, usize, bool)>, len: usize) -> bool {
    let Some((prefix, suffix, has_rest)) = shape else {
        return true;
    };
    if has_rest {
        len >= prefix + suffix
    } else {
        len == prefix + suffix
    }
}

fn specialize_sequence_row(
    row: &[DeconstructedPat],
    len: usize,
    elem: &Ty,
) -> Option<Vec<DeconstructedPat>> {
    let head = row.first()?;
    let mut specialized = specialize_sequence_pattern(head, len, elem)?;
    specialized.extend_from_slice(&row[1..]);
    Some(specialized)
}

fn specialize_sequence_pattern(
    pattern: &DeconstructedPat,
    len: usize,
    elem: &Ty,
) -> Option<Vec<DeconstructedPat>> {
    let shape = sequence_shape(pattern);
    if !sequence_matches_length(shape, len) {
        return None;
    }
    let mut fields = (0..len)
        .map(|_| DeconstructedPat::wild(elem.clone()))
        .collect::<Vec<_>>();
    let Some((prefix, suffix, _)) = shape else {
        return Some(fields);
    };
    for (index, field) in pattern.fields.iter().take(prefix).enumerate() {
        fields[index] = field.clone();
    }
    for (offset, field) in pattern.fields[prefix..suffix + prefix]
        .iter()
        .rev()
        .enumerate()
    {
        fields[len - offset - 1] = field.clone();
    }
    Some(fields)
}

fn constructor_is_inhabited(genv: &PackageTypeEnv, constructor: &Constructor) -> bool {
    let mut visiting = HashSet::new();
    let mut steps = MAX_INHABITANCE_STEPS;
    constructor
        .field_tys
        .iter()
        .all(|ty| type_is_inhabited(genv, ty, &mut visiting, 0, &mut steps))
}

fn type_is_inhabited_fresh(genv: &PackageTypeEnv, ty: &Ty) -> bool {
    let mut visiting = HashSet::new();
    let mut steps = MAX_INHABITANCE_STEPS;
    type_is_inhabited(genv, ty, &mut visiting, 0, &mut steps)
}

fn type_is_inhabited(
    genv: &PackageTypeEnv,
    ty: &Ty,
    visiting: &mut HashSet<Ty>,
    depth: usize,
    steps: &mut usize,
) -> bool {
    if depth >= MAX_INHABITANCE_DEPTH || *steps == 0 {
        return true;
    }
    *steps -= 1;
    if !visiting.insert(ty.clone()) {
        return false;
    }
    let inhabited = constructors_for_type(genv, ty)
        .map(|constructors| {
            constructors.iter().any(|constructor| {
                constructor
                    .field_tys
                    .iter()
                    .all(|field| type_is_inhabited(genv, field, visiting, depth + 1, steps))
            })
        })
        .unwrap_or(true);
    visiting.remove(ty);
    inhabited
}

fn useful_scalar(
    genv: &PackageTypeEnv,
    matrix: &[Vec<DeconstructedPat>],
    candidate: &[DeconstructedPat],
    kind: ScalarKind,
    domains: &[(i128, i128)],
) -> Option<Vec<Witness>> {
    let head = &candidate[0];
    let tail = &candidate[1..];
    let candidate_intervals = match head
        .constructor
        .as_ref()
        .map(|constructor| &constructor.key)
    {
        None => domains.to_vec(),
        Some(ConstructorKey::Scalar {
            kind: candidate_kind,
            lo,
            hi,
        }) if *candidate_kind == kind => domains
            .iter()
            .filter_map(|(domain_lo, domain_hi)| {
                let lo = (*lo).max(*domain_lo);
                let hi = (*hi).min(*domain_hi);
                (lo <= hi).then_some((lo, hi))
            })
            .collect(),
        Some(_) => return None,
    };

    for (candidate_lo, candidate_hi) in candidate_intervals {
        let mut boundaries = vec![candidate_lo, candidate_hi + 1];
        for row in matrix {
            let Some(ConstructorKey::Scalar {
                kind: row_kind,
                lo,
                hi,
            }) = row
                .first()
                .and_then(|head| head.constructor.as_ref())
                .map(|constructor| &constructor.key)
            else {
                continue;
            };
            if *row_kind != kind {
                continue;
            }
            let lo = (*lo).max(candidate_lo);
            let hi = (*hi).min(candidate_hi);
            if lo <= hi {
                boundaries.push(lo);
                boundaries.push(hi + 1);
            }
        }
        boundaries.sort_unstable();
        boundaries.dedup();

        for bounds in boundaries.windows(2) {
            let atom_lo = bounds[0];
            let atom_hi = bounds[1] - 1;
            if atom_lo > atom_hi {
                continue;
            }
            let specialized = matrix
                .iter()
                .filter_map(|row| {
                    let row_head = row.first()?;
                    let matches = match row_head
                        .constructor
                        .as_ref()
                        .map(|constructor| &constructor.key)
                    {
                        None => true,
                        Some(ConstructorKey::Scalar {
                            kind: row_kind,
                            lo,
                            hi,
                        }) => *row_kind == kind && *lo <= atom_lo && *hi >= atom_hi,
                        Some(_) => false,
                    };
                    matches.then(|| row[1..].to_vec())
                })
                .collect::<Vec<_>>();
            if let Some(tail_witnesses) = useful(genv, &specialized, tail) {
                let mut result = vec![Witness::Constructor {
                    constructor: scalar_constructor(kind, atom_lo, atom_hi),
                    fields: Vec::new(),
                    ty: head.ty.clone(),
                }];
                result.extend(tail_witnesses);
                return Some(result);
            }
        }
    }
    None
}

fn scalar_domains(ty: &Ty) -> Option<(ScalarKind, Vec<(i128, i128)>)> {
    let (kind, domains) = match ty {
        Ty::TInt8 => (ScalarKind::Int8, vec![(i8::MIN as i128, i8::MAX as i128)]),
        Ty::TInt16 => (
            ScalarKind::Int16,
            vec![(i16::MIN as i128, i16::MAX as i128)],
        ),
        Ty::TInt32 => (
            ScalarKind::Int32,
            vec![(i32::MIN as i128, i32::MAX as i128)],
        ),
        Ty::TInt64 => (
            ScalarKind::Int64,
            vec![(i64::MIN as i128, i64::MAX as i128)],
        ),
        Ty::TUint8 => (ScalarKind::Uint8, vec![(0, u8::MAX as i128)]),
        Ty::TUint16 => (ScalarKind::Uint16, vec![(0, u16::MAX as i128)]),
        Ty::TUint32 => (ScalarKind::Uint32, vec![(0, u32::MAX as i128)]),
        Ty::TUint64 => (ScalarKind::Uint64, vec![(0, u64::MAX as i128)]),
        Ty::TChar => (
            ScalarKind::Char,
            vec![(0, 0xD7FF), (0xE000, char::MAX as u32 as i128)],
        ),
        _ => return None,
    };
    Some((kind, domains))
}

fn specialize_matrix(
    matrix: &[Vec<DeconstructedPat>],
    constructor: &Constructor,
) -> Vec<Vec<DeconstructedPat>> {
    matrix
        .iter()
        .filter_map(|row| {
            let head = row.first()?;
            let mut specialized = if let Some(row_constructor) = &head.constructor {
                if row_constructor.key != constructor.key {
                    return None;
                }
                head.fields.clone()
            } else {
                constructor
                    .field_tys
                    .iter()
                    .cloned()
                    .map(DeconstructedPat::wild)
                    .collect()
            };
            specialized.extend_from_slice(&row[1..]);
            Some(specialized)
        })
        .collect()
}

fn default_matrix(matrix: &[Vec<DeconstructedPat>]) -> Vec<Vec<DeconstructedPat>> {
    matrix
        .iter()
        .filter_map(|row| {
            row.first()
                .filter(|head| head.constructor.is_none())
                .map(|_| row[1..].to_vec())
        })
        .collect()
}

fn constructors_in_column(matrix: &[Vec<DeconstructedPat>]) -> HashSet<ConstructorKey> {
    matrix
        .iter()
        .filter_map(|row| row.first()?.constructor.as_ref())
        .map(|constructor| constructor.key.clone())
        .collect()
}

fn primitive_constructor(value: &Prim, _ty: &Ty) -> Constructor {
    if let Some((kind, scalar)) = scalar_prim(value) {
        return scalar_constructor(kind, scalar, scalar);
    }
    let (key, kind) = match value {
        Prim::Unit { .. } => (ConstructorKey::Unit, ConstructorKind::Unit),
        Prim::Bool { value } => (ConstructorKey::Bool(*value), ConstructorKind::Bool(*value)),
        Prim::Float32 { value } => {
            let bits = if *value == 0.0 { 0 } else { value.to_bits() };
            (
                ConstructorKey::Float32(bits),
                ConstructorKind::Literal(value.to_string()),
            )
        }
        Prim::Float64 { value } => {
            let bits = if *value == 0.0 { 0 } else { value.to_bits() };
            (
                ConstructorKey::Float64(bits),
                ConstructorKind::Literal(value.to_string()),
            )
        }
        Prim::String { value } => (
            ConstructorKey::String(value.clone()),
            ConstructorKind::Literal(format!("{value:?}")),
        ),
        Prim::Int8 { .. }
        | Prim::Int16 { .. }
        | Prim::Int32 { .. }
        | Prim::Int64 { .. }
        | Prim::UInt8 { .. }
        | Prim::UInt16 { .. }
        | Prim::UInt32 { .. }
        | Prim::UInt64 { .. }
        | Prim::Char { .. } => unreachable!(),
    };
    Constructor {
        key,
        kind,
        field_tys: Vec::new(),
    }
}

fn scalar_prim(value: &Prim) -> Option<(ScalarKind, i128)> {
    match value {
        Prim::Int8 { value } => Some((ScalarKind::Int8, *value as i128)),
        Prim::Int16 { value } => Some((ScalarKind::Int16, *value as i128)),
        Prim::Int32 { value } => Some((ScalarKind::Int32, *value as i128)),
        Prim::Int64 { value } => Some((ScalarKind::Int64, *value as i128)),
        Prim::UInt8 { value } => Some((ScalarKind::Uint8, *value as i128)),
        Prim::UInt16 { value } => Some((ScalarKind::Uint16, *value as i128)),
        Prim::UInt32 { value } => Some((ScalarKind::Uint32, *value as i128)),
        Prim::UInt64 { value } => Some((ScalarKind::Uint64, *value as i128)),
        Prim::Char { value } => Some((ScalarKind::Char, *value as u32 as i128)),
        _ => None,
    }
}

fn scalar_constructor(kind: ScalarKind, lo: i128, hi: i128) -> Constructor {
    Constructor {
        key: ConstructorKey::Scalar { kind, lo, hi },
        kind: ConstructorKind::Literal(format_scalar_interval(kind, lo, hi)),
        field_tys: Vec::new(),
    }
}

fn format_scalar_interval(kind: ScalarKind, lo: i128, hi: i128) -> String {
    let format_value = |value: i128| match kind {
        ScalarKind::Char => char::from_u32(value as u32)
            .map(format_char_pattern)
            .unwrap_or_else(|| "_".to_string()),
        _ => value.to_string(),
    };
    if lo == hi {
        format_value(lo)
    } else {
        format!("{}..={}", format_value(lo), format_value(hi))
    }
}

fn format_char_pattern(value: char) -> String {
    match value {
        '\'' => "'\\\''".to_string(),
        '\\' => "'\\\\'".to_string(),
        '\n' => "'\\n'".to_string(),
        '\r' => "'\\r'".to_string(),
        '\t' => "'\\t'".to_string(),
        '\u{0008}' => "'\\b'".to_string(),
        '\u{000C}' => "'\\f'".to_string(),
        value if value.is_control() && (value as u32) <= 0xFFFF => {
            format!("'\\u{:04X}'", value as u32)
        }
        value => format!("'{value}'"),
    }
}

fn value_constructor(
    genv: &PackageTypeEnv,
    constructor: &ValueConstructor,
    ty: &Ty,
    fields: &[DeconstructedPat],
) -> Constructor {
    let field_tys = fields.iter().map(|field| field.ty.clone()).collect();
    match constructor {
        ValueConstructor::Enum(constructor) => Constructor {
            key: ConstructorKey::Enum {
                type_name: constructor.type_name.0.clone(),
                index: constructor.index,
            },
            kind: ConstructorKind::Enum {
                type_name: constructor.type_name.0.clone(),
                variant: constructor.variant.0.clone(),
            },
            field_tys,
        },
        ValueConstructor::Struct(constructor) => {
            let field_names = constructors_for_type(genv, ty)
                .and_then(|constructors| constructors.into_iter().next())
                .and_then(|constructor| match constructor.kind {
                    ConstructorKind::Struct { fields, .. } => Some(fields),
                    _ => None,
                })
                .unwrap_or_default();
            Constructor {
                key: ConstructorKey::Struct(constructor.type_name.0.clone()),
                kind: ConstructorKind::Struct {
                    type_name: constructor.type_name.0.clone(),
                    fields: field_names,
                },
                field_tys,
            }
        }
    }
}

fn constructors_for_type(genv: &PackageTypeEnv, ty: &Ty) -> Option<Vec<Constructor>> {
    match ty {
        Ty::TUnit => Some(vec![Constructor {
            key: ConstructorKey::Unit,
            kind: ConstructorKind::Unit,
            field_tys: Vec::new(),
        }]),
        Ty::TBool => Some(vec![
            Constructor {
                key: ConstructorKey::Bool(false),
                kind: ConstructorKind::Bool(false),
                field_tys: Vec::new(),
            },
            Constructor {
                key: ConstructorKey::Bool(true),
                kind: ConstructorKind::Bool(true),
                field_tys: Vec::new(),
            },
        ]),
        Ty::TTuple { typs } => Some(vec![Constructor {
            key: ConstructorKey::Tuple(typs.len()),
            kind: ConstructorKind::Tuple,
            field_tys: typs.clone(),
        }]),
        Ty::TArray { len, elem } => Some(vec![Constructor {
            key: ConstructorKey::Array(*len),
            kind: ConstructorKind::Array,
            field_tys: (0..*len).map(|_| elem.as_ref().clone()).collect(),
        }]),
        Ty::TVec { elem } | Ty::TSlice { elem } => Some(vec![
            Constructor {
                key: ConstructorKey::SequenceEmpty,
                kind: ConstructorKind::SequenceEmpty,
                field_tys: Vec::new(),
            },
            Constructor {
                key: ConstructorKey::SequenceCons,
                kind: ConstructorKind::SequenceCons,
                field_tys: vec![elem.as_ref().clone(), ty.clone()],
            },
        ]),
        _ => nominal_constructors(genv, ty),
    }
}

fn nominal_constructors(genv: &PackageTypeEnv, ty: &Ty) -> Option<Vec<Constructor>> {
    let (name, type_args, is_enum) = decompose_nominal_type(ty)?;
    let (_, env) = util::resolve_type_name(genv, &name);
    let ident = tast::TastIdent::new(&name);
    if is_enum {
        let definition = env.enums().get(&ident)?;
        let substitution = definition
            .generics
            .iter()
            .zip(type_args.iter())
            .map(|(param, arg)| (param.0.clone(), arg.clone()))
            .collect::<HashMap<_, _>>();
        return Some(
            definition
                .variants
                .iter()
                .enumerate()
                .map(|(index, variant)| Constructor {
                    key: ConstructorKey::Enum {
                        type_name: definition.name.0.clone(),
                        index,
                    },
                    kind: ConstructorKind::Enum {
                        type_name: definition.name.0.clone(),
                        variant: variant.name.0.clone(),
                    },
                    field_tys: variant
                        .fields
                        .types()
                        .into_iter()
                        .map(|field| substitute_ty_params(field, &substitution))
                        .collect(),
                })
                .collect(),
        );
    }

    let definition = env.structs().get(&ident)?;
    let substitution = definition
        .generics
        .iter()
        .zip(type_args.iter())
        .map(|(param, arg)| (param.0.clone(), arg.clone()))
        .collect::<HashMap<_, _>>();
    Some(vec![Constructor {
        key: ConstructorKey::Struct(definition.name.0.clone()),
        kind: ConstructorKind::Struct {
            type_name: definition.name.0.clone(),
            fields: definition
                .fields
                .iter()
                .map(|(name, _)| name.0.clone())
                .collect(),
        },
        field_tys: definition
            .fields
            .iter()
            .map(|(_, field)| substitute_ty_params(field, &substitution))
            .collect(),
    }])
}

fn decompose_nominal_type(ty: &Ty) -> Option<(String, Vec<Ty>, bool)> {
    match ty {
        Ty::TEnum { name } => Some((name.clone(), Vec::new(), true)),
        Ty::TStruct { name } => Some((name.clone(), Vec::new(), false)),
        Ty::TApp { ty, args } => {
            let (name, mut collected, is_enum) = decompose_nominal_type(ty)?;
            collected.extend(args.iter().cloned());
            Some((name, collected, is_enum))
        }
        _ => None,
    }
}
