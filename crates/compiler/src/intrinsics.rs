#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
#[serde(rename_all = "snake_case")]
pub enum IntrinsicId {
    ArrayGet,
    ArraySet,
    RefNew,
    RefGet,
    RefSet,
    RefPtrEq,
    VecNew,
    VecPush,
    VecGet,
    VecSet,
    VecLen,
    SliceNew,
    SliceGet,
    SliceLen,
    SliceSub,
    HashMapNew,
    HashMapGet,
    HashMapSet,
    HashMapRemove,
    HashMapLen,
    HashMapContains,
    Missing,
}

impl IntrinsicId {
    pub const ALL: [Self; 22] = [
        Self::ArrayGet,
        Self::ArraySet,
        Self::RefNew,
        Self::RefGet,
        Self::RefSet,
        Self::RefPtrEq,
        Self::VecNew,
        Self::VecPush,
        Self::VecGet,
        Self::VecSet,
        Self::VecLen,
        Self::SliceNew,
        Self::SliceGet,
        Self::SliceLen,
        Self::SliceSub,
        Self::HashMapNew,
        Self::HashMapGet,
        Self::HashMapSet,
        Self::HashMapRemove,
        Self::HashMapLen,
        Self::HashMapContains,
        Self::Missing,
    ];

    pub const fn key(self) -> &'static str {
        match self {
            Self::ArrayGet => "array.get",
            Self::ArraySet => "array.set",
            Self::RefNew => "ref.new",
            Self::RefGet => "ref.get",
            Self::RefSet => "ref.set",
            Self::RefPtrEq => "ref.ptr_eq",
            Self::VecNew => "vec.new",
            Self::VecPush => "vec.push",
            Self::VecGet => "vec.get",
            Self::VecSet => "vec.set",
            Self::VecLen => "vec.len",
            Self::SliceNew => "slice.new",
            Self::SliceGet => "slice.get",
            Self::SliceLen => "slice.len",
            Self::SliceSub => "slice.sub",
            Self::HashMapNew => "hashmap.new",
            Self::HashMapGet => "hashmap.get",
            Self::HashMapSet => "hashmap.set",
            Self::HashMapRemove => "hashmap.remove",
            Self::HashMapLen => "hashmap.len",
            Self::HashMapContains => "hashmap.contains",
            Self::Missing => "compiler.missing",
        }
    }

    pub const fn source_name(self) -> &'static str {
        match self {
            Self::ArrayGet => "array_get",
            Self::ArraySet => "array_set",
            Self::RefNew => "ref",
            Self::RefGet => "ref_get",
            Self::RefSet => "ref_set",
            Self::RefPtrEq => "ptr_eq",
            Self::VecNew => "vec_new",
            Self::VecPush => "vec_push",
            Self::VecGet => "vec_get",
            Self::VecSet => "vec_set",
            Self::VecLen => "vec_len",
            Self::SliceNew => "slice",
            Self::SliceGet => "slice_get",
            Self::SliceLen => "slice_len",
            Self::SliceSub => "slice_sub",
            Self::HashMapNew => "hashmap_new",
            Self::HashMapGet => "hashmap_get",
            Self::HashMapSet => "hashmap_set",
            Self::HashMapRemove => "hashmap_remove",
            Self::HashMapLen => "hashmap_len",
            Self::HashMapContains => "hashmap_contains",
            Self::Missing => "missing",
        }
    }

    pub fn from_key(key: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|id| id.key() == key)
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
#[serde(rename_all = "snake_case")]
pub enum RuntimeHookId {
    UnitToString,
    BoolToString,
    StringLen,
    StringGet,
    StringByteSlice,
    StringPrint,
    StringPrintln,
    CharToString,
    Int8ToString,
    Int16ToString,
    Int32ToString,
    Int64ToString,
    Uint8ToString,
    Uint16ToString,
    Uint32ToString,
    Uint64ToString,
    Float32ToString,
    Float64ToString,
    Int8Hash,
    Int16Hash,
    Int32Hash,
    Int64Hash,
    Uint8Hash,
    Uint16Hash,
    Uint32Hash,
    Float32Hash,
    Float64Hash,
    StringHash,
    CharHash,
    StdEnvArgs,
    StdFsReadFile,
    StdFsWriteFile,
    StdFsCreateDirAll,
    StdFsFileExists,
    StdFsReadDir,
    StdIoPrint,
    StdIoPrintln,
    StdIoEprint,
    StdProcessExit,
}

impl RuntimeHookId {
    pub const ALL: [Self; 39] = [
        Self::UnitToString,
        Self::BoolToString,
        Self::StringLen,
        Self::StringGet,
        Self::StringByteSlice,
        Self::StringPrint,
        Self::StringPrintln,
        Self::CharToString,
        Self::Int8ToString,
        Self::Int16ToString,
        Self::Int32ToString,
        Self::Int64ToString,
        Self::Uint8ToString,
        Self::Uint16ToString,
        Self::Uint32ToString,
        Self::Uint64ToString,
        Self::Float32ToString,
        Self::Float64ToString,
        Self::Int8Hash,
        Self::Int16Hash,
        Self::Int32Hash,
        Self::Int64Hash,
        Self::Uint8Hash,
        Self::Uint16Hash,
        Self::Uint32Hash,
        Self::Float32Hash,
        Self::Float64Hash,
        Self::StringHash,
        Self::CharHash,
        Self::StdEnvArgs,
        Self::StdFsReadFile,
        Self::StdFsWriteFile,
        Self::StdFsCreateDirAll,
        Self::StdFsFileExists,
        Self::StdFsReadDir,
        Self::StdIoPrint,
        Self::StdIoPrintln,
        Self::StdIoEprint,
        Self::StdProcessExit,
    ];

    pub const fn key(self) -> &'static str {
        match self {
            Self::UnitToString => "core.unit_to_string",
            Self::BoolToString => "core.bool_to_string",
            Self::StringLen => "core.string_len",
            Self::StringGet => "core.string_get",
            Self::StringByteSlice => "core.string_byte_slice",
            Self::StringPrint => "core.string_print",
            Self::StringPrintln => "core.string_println",
            Self::CharToString => "core.char_to_string",
            Self::Int8ToString => "core.int8_to_string",
            Self::Int16ToString => "core.int16_to_string",
            Self::Int32ToString => "core.int32_to_string",
            Self::Int64ToString => "core.int64_to_string",
            Self::Uint8ToString => "core.uint8_to_string",
            Self::Uint16ToString => "core.uint16_to_string",
            Self::Uint32ToString => "core.uint32_to_string",
            Self::Uint64ToString => "core.uint64_to_string",
            Self::Float32ToString => "core.float32_to_string",
            Self::Float64ToString => "core.float64_to_string",
            Self::Int8Hash => "core.int8_hash",
            Self::Int16Hash => "core.int16_hash",
            Self::Int32Hash => "core.int32_hash",
            Self::Int64Hash => "core.int64_hash",
            Self::Uint8Hash => "core.uint8_hash",
            Self::Uint16Hash => "core.uint16_hash",
            Self::Uint32Hash => "core.uint32_hash",
            Self::Float32Hash => "core.float32_hash",
            Self::Float64Hash => "core.float64_hash",
            Self::StringHash => "core.string_hash",
            Self::CharHash => "core.char_hash",
            Self::StdEnvArgs => "std.env.args",
            Self::StdFsReadFile => "std.fs.read_file",
            Self::StdFsWriteFile => "std.fs.write_file",
            Self::StdFsCreateDirAll => "std.fs.create_dir_all",
            Self::StdFsFileExists => "std.fs.file_exists",
            Self::StdFsReadDir => "std.fs.read_dir",
            Self::StdIoPrint => "std.io.print",
            Self::StdIoPrintln => "std.io.println",
            Self::StdIoEprint => "std.io.eprint",
            Self::StdProcessExit => "std.process.exit",
        }
    }

    pub fn from_key(key: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|id| id.key() == key)
    }

    pub fn is_core(self) -> bool {
        matches!(
            self,
            Self::UnitToString
                | Self::BoolToString
                | Self::StringLen
                | Self::StringGet
                | Self::StringByteSlice
                | Self::StringPrint
                | Self::StringPrintln
                | Self::CharToString
                | Self::Int8ToString
                | Self::Int16ToString
                | Self::Int32ToString
                | Self::Int64ToString
                | Self::Uint8ToString
                | Self::Uint16ToString
                | Self::Uint32ToString
                | Self::Uint64ToString
                | Self::Float32ToString
                | Self::Float64ToString
                | Self::Int8Hash
                | Self::Int16Hash
                | Self::Int32Hash
                | Self::Int64Hash
                | Self::Uint8Hash
                | Self::Uint16Hash
                | Self::Uint32Hash
                | Self::Float32Hash
                | Self::Float64Hash
                | Self::StringHash
                | Self::CharHash
        )
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, Default, serde::Serialize, serde::Deserialize,
)]
#[serde(tag = "kind", content = "id", rename_all = "snake_case")]
pub enum CallableBody {
    #[default]
    Goml,
    Intrinsic(IntrinsicId),
    Runtime(RuntimeHookId),
}

impl CallableBody {
    pub fn display(self) -> String {
        match self {
            Self::Goml => "goml".to_string(),
            Self::Intrinsic(id) => format!("intrinsic {}", id.key()),
            Self::Runtime(id) => format!("runtime {}", id.key()),
        }
    }

    pub fn ir_name(self) -> String {
        match self {
            Self::Goml => "@goml".to_string(),
            Self::Intrinsic(id) => format!("@intrinsic({})", id.key()),
            Self::Runtime(id) => format!("@runtime({})", id.key()),
        }
    }
}

pub fn callable_body_from_attributes<'a>(
    attributes: impl IntoIterator<Item = &'a str>,
) -> Result<CallableBody, String> {
    let mut body = None;
    for attribute in attributes {
        let Some((name, key)) = parse_callable_attribute(attribute)? else {
            continue;
        };
        let parsed = match name {
            "intrinsic" => IntrinsicId::from_key(&key)
                .map(CallableBody::Intrinsic)
                .ok_or_else(|| format!("unknown intrinsic {key}"))?,
            "runtime" => RuntimeHookId::from_key(&key)
                .map(CallableBody::Runtime)
                .ok_or_else(|| format!("unknown runtime hook {key}"))?,
            _ => unreachable!(),
        };
        if body.replace(parsed).is_some() {
            return Err("extern declaration has multiple implementation attributes".to_string());
        }
    }
    body.ok_or_else(|| {
        "extern declaration requires `#[intrinsic(\"...\")]` or `#[runtime(\"...\")]`".to_string()
    })
}

fn parse_callable_attribute(attribute: &str) -> Result<Option<(&str, String)>, String> {
    let trimmed = attribute.trim();
    let Some(inner) = trimmed
        .strip_prefix("#[")
        .and_then(|value| value.strip_suffix(']'))
        .map(str::trim)
    else {
        return Ok(None);
    };
    let name_end = inner.find('(').unwrap_or(inner.len());
    let name = inner[..name_end]
        .trim()
        .split("::")
        .last()
        .unwrap_or_default();
    if name != "intrinsic" && name != "runtime" {
        return Ok(None);
    }
    let argument = inner[name_end..]
        .trim()
        .strip_prefix('(')
        .and_then(|value| value.strip_suffix(')'))
        .map(str::trim)
        .and_then(|value| value.strip_prefix('"'))
        .and_then(|value| value.strip_suffix('"'))
        .filter(|value| !value.is_empty())
        .ok_or_else(|| format!("malformed {name} attribute"))?;
    Ok(Some((name, argument.to_string())))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExternCapability {
    None,
    Core,
    StandardLibrary,
}

impl ExternCapability {
    pub fn permits(self, body: CallableBody) -> bool {
        match (self, body) {
            (Self::Core, CallableBody::Intrinsic(_)) => true,
            (Self::Core, CallableBody::Runtime(id)) => id.is_core(),
            (Self::StandardLibrary, CallableBody::Runtime(id)) => !id.is_core(),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallEffect {
    Pure,
    MutatesArgument(usize),
    Host,
    Diverges,
}

impl IntrinsicId {
    pub fn effect(self) -> CallEffect {
        match self {
            Self::ArrayGet
            | Self::ArraySet
            | Self::RefGet
            | Self::RefPtrEq
            | Self::VecGet
            | Self::VecLen
            | Self::SliceNew
            | Self::SliceGet
            | Self::SliceLen
            | Self::SliceSub
            | Self::HashMapGet
            | Self::HashMapLen
            | Self::HashMapContains => CallEffect::Pure,
            Self::RefSet
            | Self::VecPush
            | Self::VecSet
            | Self::HashMapSet
            | Self::HashMapRemove => CallEffect::MutatesArgument(0),
            Self::RefNew | Self::VecNew | Self::HashMapNew => CallEffect::Pure,
            Self::Missing => CallEffect::Diverges,
        }
    }
}

impl RuntimeHookId {
    pub fn effect(self) -> CallEffect {
        match self {
            Self::StringPrint
            | Self::StringPrintln
            | Self::StdEnvArgs
            | Self::StdFsReadFile
            | Self::StdFsWriteFile
            | Self::StdFsCreateDirAll
            | Self::StdFsFileExists
            | Self::StdFsReadDir
            | Self::StdIoPrint
            | Self::StdIoPrintln
            | Self::StdIoEprint => CallEffect::Host,
            Self::StdProcessExit => CallEffect::Diverges,
            _ => CallEffect::Pure,
        }
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
#[serde(rename_all = "snake_case")]
pub enum LangItemId {
    Vec,
    Slice,
    Ref,
    HashMap,
    Iterator,
    IntoIterator,
    Eq,
    Hash,
    ToString,
    Option,
    Result,
}

#[derive(Debug, Clone, Default)]
pub struct LangItemTable {
    items: indexmap::IndexMap<LangItemId, crate::tast::TastIdent>,
}

impl LangItemTable {
    pub fn with_builtin_types() -> Self {
        let mut table = Self::default();
        for id in [
            LangItemId::Vec,
            LangItemId::Slice,
            LangItemId::Ref,
            LangItemId::HashMap,
        ] {
            table
                .items
                .insert(id, crate::tast::TastIdent::new(id.source_name()));
        }
        table
    }

    pub fn insert(
        &mut self,
        id: LangItemId,
        name: crate::tast::TastIdent,
    ) -> Result<(), crate::tast::TastIdent> {
        if let Some(existing) = self.items.get(&id) {
            return Err(existing.clone());
        }
        self.items.insert(id, name);
        Ok(())
    }

    pub fn get(&self, id: LangItemId) -> Option<&crate::tast::TastIdent> {
        self.items.get(&id)
    }
}

impl LangItemId {
    pub const ALL: [Self; 11] = [
        Self::Vec,
        Self::Slice,
        Self::Ref,
        Self::HashMap,
        Self::Iterator,
        Self::IntoIterator,
        Self::Eq,
        Self::Hash,
        Self::ToString,
        Self::Option,
        Self::Result,
    ];

    pub const fn key(self) -> &'static str {
        match self {
            Self::Vec => "vec",
            Self::Slice => "slice",
            Self::Ref => "ref",
            Self::HashMap => "hashmap",
            Self::Iterator => "iterator",
            Self::IntoIterator => "into_iterator",
            Self::Eq => "eq",
            Self::Hash => "hash",
            Self::ToString => "to_string",
            Self::Option => "option",
            Self::Result => "result",
        }
    }

    pub fn from_key(key: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|id| id.key() == key)
    }

    pub const fn source_name(self) -> &'static str {
        match self {
            Self::Vec => "Vec",
            Self::Slice => "Slice",
            Self::Ref => "Ref",
            Self::HashMap => "HashMap",
            Self::Iterator => "Iterator",
            Self::IntoIterator => "IntoIterator",
            Self::Eq => "Eq",
            Self::Hash => "Hash",
            Self::ToString => "ToString",
            Self::Option => "Option",
            Self::Result => "Result",
        }
    }
}

pub fn lang_item_from_attributes<'a>(
    attributes: impl IntoIterator<Item = &'a str>,
) -> Result<Option<LangItemId>, String> {
    let mut item = None;
    for attribute in attributes {
        let trimmed = attribute.trim();
        let Some(inner) = trimmed
            .strip_prefix("#[")
            .and_then(|value| value.strip_suffix(']'))
            .map(str::trim)
        else {
            continue;
        };
        let name_end = inner.find('(').unwrap_or(inner.len());
        let name = inner[..name_end]
            .trim()
            .split("::")
            .last()
            .unwrap_or_default();
        if name != "lang" {
            continue;
        }
        let key = inner[name_end..]
            .trim()
            .strip_prefix('(')
            .and_then(|value| value.strip_suffix(')'))
            .map(str::trim)
            .and_then(|value| value.strip_prefix('"'))
            .and_then(|value| value.strip_suffix('"'))
            .filter(|value| !value.is_empty())
            .ok_or_else(|| "malformed lang attribute".to_string())?;
        let parsed = LangItemId::from_key(key).ok_or_else(|| format!("unknown lang item {key}"))?;
        if item.replace(parsed).is_some() {
            return Err("declaration has multiple lang attributes".to_string());
        }
    }
    Ok(item)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallableSignature {
    pub type_params: Vec<String>,
    pub constraints: Vec<(String, String)>,
    pub ty: crate::tast::Ty,
}

fn tparam(name: &str) -> crate::tast::Ty {
    crate::tast::Ty::TParam {
        name: name.to_string(),
    }
}

fn func(params: Vec<crate::tast::Ty>, ret: crate::tast::Ty) -> crate::tast::Ty {
    crate::tast::Ty::TFunc {
        params,
        ret_ty: Box::new(ret),
    }
}

fn signature(params: Vec<crate::tast::Ty>, ret: crate::tast::Ty) -> CallableSignature {
    CallableSignature {
        type_params: Vec::new(),
        constraints: Vec::new(),
        ty: func(params, ret),
    }
}

fn generic_signature(
    type_params: &[&str],
    constraints: &[(&str, LangItemId)],
    params: Vec<crate::tast::Ty>,
    ret: crate::tast::Ty,
) -> CallableSignature {
    CallableSignature {
        type_params: type_params.iter().map(|name| (*name).to_string()).collect(),
        constraints: constraints
            .iter()
            .map(|(param, item)| ((*param).to_string(), item.source_name().to_string()))
            .collect(),
        ty: func(params, ret),
    }
}

fn vec_ty(elem: crate::tast::Ty) -> crate::tast::Ty {
    crate::tast::Ty::TVec {
        elem: Box::new(elem),
    }
}

fn slice_ty(elem: crate::tast::Ty) -> crate::tast::Ty {
    crate::tast::Ty::TSlice {
        elem: Box::new(elem),
    }
}

fn ref_ty(elem: crate::tast::Ty) -> crate::tast::Ty {
    crate::tast::Ty::TRef {
        elem: Box::new(elem),
    }
}

fn hashmap_ty(key: crate::tast::Ty, value: crate::tast::Ty) -> crate::tast::Ty {
    crate::tast::Ty::THashMap {
        key: Box::new(key),
        value: Box::new(value),
    }
}

fn tuple_ty(typs: Vec<crate::tast::Ty>) -> crate::tast::Ty {
    crate::tast::Ty::TTuple { typs }
}

fn option_ty(value: crate::tast::Ty) -> crate::tast::Ty {
    crate::tast::Ty::TApp {
        ty: Box::new(crate::tast::Ty::TEnum {
            name: LangItemId::Option.source_name().to_string(),
        }),
        args: vec![value],
    }
}

impl IntrinsicId {
    pub fn signature(self) -> CallableSignature {
        let t = tparam("T");
        let k = tparam("K");
        let v = tparam("V");
        let array = crate::tast::Ty::TArray {
            len: crate::tast::ARRAY_WILDCARD_LEN,
            elem: Box::new(t.clone()),
        };
        let map_constraints = [("K", LangItemId::Hash), ("K", LangItemId::Eq)];
        match self {
            Self::ArrayGet => {
                generic_signature(&["T"], &[], vec![array, crate::tast::Ty::TInt32], t)
            }
            Self::ArraySet => generic_signature(
                &["T"],
                &[],
                vec![array.clone(), crate::tast::Ty::TInt32, t],
                array,
            ),
            Self::RefNew => generic_signature(&["T"], &[], vec![t.clone()], ref_ty(t)),
            Self::RefGet => generic_signature(&["T"], &[], vec![ref_ty(t.clone())], t),
            Self::RefSet => generic_signature(
                &["T"],
                &[],
                vec![ref_ty(t.clone()), t],
                crate::tast::Ty::TUnit,
            ),
            Self::RefPtrEq => generic_signature(
                &["T"],
                &[],
                vec![ref_ty(t.clone()), ref_ty(t)],
                crate::tast::Ty::TBool,
            ),
            Self::VecNew => generic_signature(&["T"], &[], vec![], vec_ty(t)),
            Self::VecPush => generic_signature(
                &["T"],
                &[],
                vec![vec_ty(t.clone()), t],
                crate::tast::Ty::TUnit,
            ),
            Self::VecGet => generic_signature(
                &["T"],
                &[],
                vec![vec_ty(t.clone()), crate::tast::Ty::TInt32],
                t,
            ),
            Self::VecSet => generic_signature(
                &["T"],
                &[],
                vec![vec_ty(t.clone()), crate::tast::Ty::TInt32, t],
                crate::tast::Ty::TUnit,
            ),
            Self::VecLen => {
                generic_signature(&["T"], &[], vec![vec_ty(t)], crate::tast::Ty::TInt32)
            }
            Self::SliceNew => generic_signature(
                &["T"],
                &[],
                vec![
                    vec_ty(t.clone()),
                    crate::tast::Ty::TInt32,
                    crate::tast::Ty::TInt32,
                ],
                slice_ty(t),
            ),
            Self::SliceGet => generic_signature(
                &["T"],
                &[],
                vec![slice_ty(t.clone()), crate::tast::Ty::TInt32],
                t,
            ),
            Self::SliceLen => {
                generic_signature(&["T"], &[], vec![slice_ty(t)], crate::tast::Ty::TInt32)
            }
            Self::SliceSub => generic_signature(
                &["T"],
                &[],
                vec![
                    slice_ty(t.clone()),
                    crate::tast::Ty::TInt32,
                    crate::tast::Ty::TInt32,
                ],
                slice_ty(t),
            ),
            Self::HashMapNew => {
                generic_signature(&["K", "V"], &map_constraints, vec![], hashmap_ty(k, v))
            }
            Self::HashMapGet => generic_signature(
                &["K", "V"],
                &map_constraints,
                vec![hashmap_ty(k.clone(), v.clone()), k],
                option_ty(v),
            ),
            Self::HashMapSet => generic_signature(
                &["K", "V"],
                &map_constraints,
                vec![hashmap_ty(k.clone(), v.clone()), k, v],
                crate::tast::Ty::TUnit,
            ),
            Self::HashMapRemove => generic_signature(
                &["K", "V"],
                &map_constraints,
                vec![hashmap_ty(k.clone(), v), k],
                crate::tast::Ty::TUnit,
            ),
            Self::HashMapLen => generic_signature(
                &["K", "V"],
                &map_constraints,
                vec![hashmap_ty(k, v)],
                crate::tast::Ty::TInt32,
            ),
            Self::HashMapContains => generic_signature(
                &["K", "V"],
                &map_constraints,
                vec![hashmap_ty(k.clone(), v), k],
                crate::tast::Ty::TBool,
            ),
            Self::Missing => generic_signature(&["T"], &[], vec![crate::tast::Ty::TString], t),
        }
    }
}

impl RuntimeHookId {
    pub fn signature(self) -> CallableSignature {
        use crate::tast::Ty;
        match self {
            Self::UnitToString => signature(vec![Ty::TUnit], Ty::TString),
            Self::BoolToString => signature(vec![Ty::TBool], Ty::TString),
            Self::StringLen => signature(vec![Ty::TString], Ty::TInt32),
            Self::StringGet => signature(vec![Ty::TString, Ty::TInt32], Ty::TChar),
            Self::StringByteSlice => {
                signature(vec![Ty::TString, Ty::TInt32, Ty::TInt32], Ty::TString)
            }
            Self::StringPrint | Self::StringPrintln => signature(vec![Ty::TString], Ty::TUnit),
            Self::CharToString => signature(vec![Ty::TChar], Ty::TString),
            Self::Int8ToString => signature(vec![Ty::TInt8], Ty::TString),
            Self::Int16ToString => signature(vec![Ty::TInt16], Ty::TString),
            Self::Int32ToString => signature(vec![Ty::TInt32], Ty::TString),
            Self::Int64ToString => signature(vec![Ty::TInt64], Ty::TString),
            Self::Uint8ToString => signature(vec![Ty::TUint8], Ty::TString),
            Self::Uint16ToString => signature(vec![Ty::TUint16], Ty::TString),
            Self::Uint32ToString => signature(vec![Ty::TUint32], Ty::TString),
            Self::Uint64ToString => signature(vec![Ty::TUint64], Ty::TString),
            Self::Float32ToString => signature(vec![Ty::TFloat32], Ty::TString),
            Self::Float64ToString => signature(vec![Ty::TFloat64], Ty::TString),
            Self::Int8Hash => signature(vec![Ty::TInt8], Ty::TUint64),
            Self::Int16Hash => signature(vec![Ty::TInt16], Ty::TUint64),
            Self::Int32Hash => signature(vec![Ty::TInt32], Ty::TUint64),
            Self::Int64Hash => signature(vec![Ty::TInt64], Ty::TUint64),
            Self::Uint8Hash => signature(vec![Ty::TUint8], Ty::TUint64),
            Self::Uint16Hash => signature(vec![Ty::TUint16], Ty::TUint64),
            Self::Uint32Hash => signature(vec![Ty::TUint32], Ty::TUint64),
            Self::Float32Hash => signature(vec![Ty::TFloat32], Ty::TUint64),
            Self::Float64Hash => signature(vec![Ty::TFloat64], Ty::TUint64),
            Self::StringHash => signature(vec![Ty::TString], Ty::TUint64),
            Self::CharHash => signature(vec![Ty::TChar], Ty::TUint64),
            Self::StdEnvArgs => signature(vec![], vec_ty(Ty::TString)),
            Self::StdFsReadFile => signature(
                vec![Ty::TString],
                tuple_ty(vec![Ty::TBool, Ty::TString, Ty::TString]),
            ),
            Self::StdFsWriteFile => signature(
                vec![Ty::TString, Ty::TString],
                tuple_ty(vec![Ty::TBool, Ty::TString]),
            ),
            Self::StdFsCreateDirAll => {
                signature(vec![Ty::TString], tuple_ty(vec![Ty::TBool, Ty::TString]))
            }
            Self::StdFsFileExists => signature(vec![Ty::TString], Ty::TBool),
            Self::StdFsReadDir => signature(
                vec![Ty::TString],
                tuple_ty(vec![Ty::TBool, vec_ty(Ty::TString), Ty::TString]),
            ),
            Self::StdIoPrint | Self::StdIoPrintln | Self::StdIoEprint => {
                signature(vec![Ty::TString], Ty::TUnit)
            }
            Self::StdProcessExit => signature(vec![Ty::TInt32], Ty::TUnit),
        }
    }
}

impl CallableBody {
    pub fn signature(self) -> Option<CallableSignature> {
        match self {
            Self::Goml => None,
            Self::Intrinsic(id) => Some(id.signature()),
            Self::Runtime(id) => Some(id.signature()),
        }
    }
}

pub fn validate_callable_signature(
    body: CallableBody,
    type_params: &[String],
    constraints: &[(String, String)],
    ty: &crate::tast::Ty,
) -> Result<(), String> {
    let Some(mut expected) = body.signature() else {
        return Err("ordinary GoML function cannot be declared extern".to_string());
    };
    let mut actual_constraints = constraints.to_vec();
    expected.constraints.sort();
    actual_constraints.sort();
    if type_params == expected.type_params
        && actual_constraints == expected.constraints
        && ty == &expected.ty
    {
        return Ok(());
    }
    Err(format!(
        "{} has signature {:?} with bounds {:?}, expected {:?} with bounds {:?}",
        body.display(),
        ty,
        actual_constraints,
        expected.ty,
        expected.constraints
    ))
}
