use ast::ast;
use diagnostics::Diagnostics;
use la_arena::{Arena, Idx};
use parser::syntax::MySyntaxNodePtr;
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct SourceFileAst {
    pub path: PathBuf,
    pub ast: ast::File,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageName(pub String);

impl PackageName {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PackageId(pub u32);

#[derive(Debug, Clone)]
pub struct ProjectHir {
    pub packages: Vec<PackageHir>,
    pub package_index: HashMap<PackageName, PackageId>,
}

#[derive(Debug, Clone)]
pub struct PackageHir {
    pub id: PackageId,
    pub name: PackageName,
    pub imports: Vec<PackageName>,
    pub files: Vec<SourceFileHir>,
    pub toplevels: Vec<DefId>,
}

#[derive(Debug, Clone)]
pub struct SourceFileHir {
    pub path: String,
    pub package: PackageName,
    pub imports: Vec<PackageName>,
    pub toplevels: Vec<DefId>,
}

#[derive(Debug, Clone)]
pub struct ResolvedHir {
    pub files: Vec<SourceFileHir>,
    pub toplevels: Vec<DefId>,
}

#[derive(Debug, Clone)]
pub struct ProjectHirTable {
    packages: HashMap<PackageId, HirTable>,
}

impl ProjectHirTable {
    pub fn new() -> Self {
        Self {
            packages: HashMap::new(),
        }
    }

    pub fn insert(&mut self, package: PackageId, table: HirTable) {
        self.packages.insert(package, table);
    }

    pub fn package(&self, package: PackageId) -> Option<&HirTable> {
        self.packages.get(&package)
    }
}

impl Default for ProjectHirTable {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct PackageInterface {
    pub id: PackageId,
    pub name: PackageName,
    pub exports: HashMap<String, DefId>,
    pub enum_variants: HashMap<String, Vec<String>>,
}

impl PackageInterface {
    pub fn from_hir(package: &PackageHir, table: &HirTable) -> Self {
        let mut exports = HashMap::new();
        let mut enum_variants = HashMap::new();

        for &def_id in package.toplevels.iter() {
            match table.def(def_id) {
                Def::EnumDef(enum_def) => {
                    let full = enum_def.name.to_ident_name();
                    exports.insert(full.clone(), def_id);
                    let key = full.rsplit("::").next().unwrap_or(&full).to_string();
                    let variants = enum_def
                        .variants
                        .iter()
                        .map(|(name, _)| name.to_ident_name())
                        .collect();
                    enum_variants.insert(key, variants);
                }
                Def::StructDef(struct_def) => {
                    exports.insert(struct_def.name.to_ident_name(), def_id);
                }
                Def::TraitDef(trait_def) => {
                    exports.insert(trait_def.name.to_ident_name(), def_id);
                }
                Def::Fn(func) => {
                    exports.insert(func.name.clone(), def_id);
                }
                Def::ExternGo(ext) => {
                    exports.insert(ext.goml_name.to_ident_name(), def_id);
                }
                Def::ExternType(ext) => {
                    exports.insert(ext.goml_name.to_ident_name(), def_id);
                }
                Def::ExternBuiltin(ext) => {
                    exports.insert(ext.name.to_ident_name(), def_id);
                }
                Def::ImplBlock(_) => {}
            }
        }

        PackageInterface {
            id: package.id,
            name: package.name.clone(),
            exports,
            enum_variants,
        }
    }
}

pub fn lower_to_hir_files(files: Vec<SourceFileAst>) -> (PackageHir, HirTable, Diagnostics) {
    let deps = HashMap::new();
    let package_name = files
        .first()
        .map(|file| file.ast.package.0.as_str())
        .unwrap_or("Main");
    let package_id = match package_name {
        "Builtin" => PackageId(0),
        "Main" => PackageId(1),
        _ => PackageId(2),
    };
    lower_to_hir_files_with_env(package_id, files, &deps)
}

pub fn lower_to_hir_files_with_env(
    package_id: PackageId,
    files: Vec<SourceFileAst>,
    deps: &HashMap<String, PackageInterface>,
) -> (PackageHir, HirTable, Diagnostics) {
    use crate::typer::name_resolution::NameResolution;
    let (resolved, mut hir_table, diagnostics) =
        NameResolution::default().resolve_files_with_env(package_id, files, deps);
    let _ctor_errors = resolve_constructors(&mut hir_table);
    let package_name = resolved
        .files
        .first()
        .map(|file| file.package.clone())
        .unwrap_or_else(|| PackageName("Main".to_string()));
    let mut imports: Vec<PackageName> = resolved
        .files
        .iter()
        .flat_map(|file| file.imports.iter().cloned())
        .collect();
    imports.sort_by(|a, b| a.0.cmp(&b.0));
    imports.dedup_by(|a, b| a.0 == b.0);
    (
        PackageHir {
            id: package_id,
            name: package_name,
            imports,
            files: resolved.files,
            toplevels: resolved.toplevels,
        },
        hir_table,
        diagnostics,
    )
}

pub fn lower_to_project_hir_files(
    files: Vec<SourceFileAst>,
) -> (ProjectHir, ProjectHirTable, Diagnostics) {
    let deps = HashMap::new();
    lower_to_project_hir_files_with_env(files, &deps)
}

pub fn lower_to_project_hir_files_with_env(
    files: Vec<SourceFileAst>,
    deps: &HashMap<String, PackageInterface>,
) -> (ProjectHir, ProjectHirTable, Diagnostics) {
    let mut grouped: HashMap<PackageName, Vec<SourceFileAst>> = HashMap::new();
    for file in files {
        grouped
            .entry(PackageName(file.ast.package.0.clone()))
            .or_default()
            .push(file);
    }

    let mut other_packages: Vec<PackageName> = grouped
        .keys()
        .filter(|name| name.as_str() != "Main")
        .cloned()
        .collect();
    other_packages.sort_by(|a, b| a.0.cmp(&b.0));

    let mut package_order = Vec::new();
    if grouped.contains_key(&PackageName("Main".to_string())) {
        package_order.push(PackageName("Main".to_string()));
    }
    package_order.extend(other_packages);

    let mut package_index = HashMap::new();
    package_index.insert(PackageName("Builtin".to_string()), PackageId(0));
    if package_order.iter().any(|name| name.as_str() == "Main") {
        package_index.insert(PackageName("Main".to_string()), PackageId(1));
    }
    let mut next_id = 2u32;
    for name in package_order.iter() {
        if name.as_str() == "Main" {
            continue;
        }
        package_index.insert(name.clone(), PackageId(next_id));
        next_id += 1;
    }

    let mut project_table = ProjectHirTable::new();
    let mut package_hirs = Vec::new();
    let mut diagnostics = Diagnostics::new();

    for name in package_order {
        let Some(files) = grouped.remove(&name) else {
            continue;
        };
        let package_id = *package_index.get(&name).unwrap_or(&PackageId(2));
        let (package_hir, hir_table, mut package_diagnostics) =
            lower_to_hir_files_with_env(package_id, files, deps);
        diagnostics.append(&mut package_diagnostics);
        project_table.insert(package_id, hir_table);
        package_hirs.push(package_hir);
    }

    (
        ProjectHir {
            packages: package_hirs,
            package_index,
        },
        project_table,
        diagnostics,
    )
}

pub fn lower_to_hir(ast: ast::File) -> (PackageHir, HirTable, Diagnostics) {
    lower_to_hir_files(vec![SourceFileAst {
        path: PathBuf::from("<unknown>"),
        ast,
    }])
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefKind {
    Fn,
    EnumDef,
    StructDef,
    TraitDef,
    ImplBlock,
    ExternGo,
    ExternType,
    ExternBuiltin,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefKey {
    pub path: Path,
    pub kind: DefKind,
    pub disamb: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId {
    pub pkg: PackageId,
    pub idx: u32,
}

impl DefId {
    pub fn to_debug_string(self) -> String {
        format!("def/{}/{}", self.pkg.0, self.idx)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LocalKey {
    AstBinder {
        owner: DefId,
        ptr: MySyntaxNodePtr,
    },
    Synthetic {
        owner: DefId,
        serial: u32,
        hint: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId {
    pub pkg: PackageId,
    pub idx: u32,
}

impl LocalId {
    pub fn to_debug_string(self) -> String {
        format!("local/{}/{}", self.pkg.0, self.idx)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId {
    pub pkg: PackageId,
    pub idx: u32,
}

impl ExprId {
    pub fn to_debug_string(self) -> String {
        format!("expr/{}/{}", self.pkg.0, self.idx)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PatId {
    pub pkg: PackageId,
    pub idx: u32,
}

impl PatId {
    pub fn to_debug_string(self) -> String {
        format!("pat/{}/{}", self.pkg.0, self.idx)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConstructorId {
    EnumVariant { enum_def: DefId, variant_idx: u32 },
}

impl ConstructorId {
    pub fn to_debug_string(self) -> String {
        match self {
            ConstructorId::EnumVariant {
                enum_def,
                variant_idx,
            } => {
                format!("ctor({}::v{})", enum_def.to_debug_string(), variant_idx)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstructorRef {
    Unresolved(Path),
    Resolved(ConstructorId),
    Ambiguous {
        path: Path,
        candidates: Vec<ConstructorId>,
    },
}

impl ConstructorRef {
    pub fn display(&self, _hir_table: &ProjectHirTable) -> String {
        match self {
            ConstructorRef::Unresolved(path) => path.display(),
            ConstructorRef::Resolved(id) => id.to_debug_string(),
            ConstructorRef::Ambiguous { path, .. } => path.display(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct HirTable {
    package: PackageId,
    def_interner: HashMap<DefKey, DefId>,
    def_data: Vec<Def>,
    def_paths: Vec<Path>,
    local_interner: HashMap<LocalKey, LocalId>,
    local_info: Vec<LocalInfo>,
    local_counter: u32,
    exprs: Arena<Expr>,
    expr_ptrs: Vec<Option<MySyntaxNodePtr>>,
    pats: Arena<Pat>,
    pat_ptrs: Vec<Option<MySyntaxNodePtr>>,
    dummy_expr: ExprId,
    dummy_pat: PatId,
    current_owner: Option<DefId>,
}

#[derive(Debug, Clone)]
pub struct LocalInfo {
    pub hint: String,
    pub origin: LocalKey,
}

impl HirTable {
    pub fn new(package: PackageId) -> Self {
        let mut exprs = Arena::new();
        let dummy_expr = ExprId {
            pkg: package,
            idx: exprs.alloc(Expr::EUnit).into_raw().into_u32(),
        };
        let mut pats = Arena::new();
        let dummy_pat = PatId {
            pkg: package,
            idx: pats.alloc(Pat::PWild).into_raw().into_u32(),
        };
        Self {
            package,
            def_interner: HashMap::new(),
            def_data: Vec::new(),
            def_paths: Vec::new(),
            local_interner: HashMap::new(),
            local_info: Vec::new(),
            local_counter: 0,
            exprs,
            expr_ptrs: vec![None],
            pats,
            pat_ptrs: vec![None],
            dummy_expr,
            dummy_pat,
            current_owner: None,
        }
    }

    pub fn package(&self) -> PackageId {
        self.package
    }

    pub fn expr_count(&self) -> usize {
        self.exprs.len()
    }

    pub fn pat_count(&self) -> usize {
        self.pats.len()
    }

    pub fn local_count(&self) -> usize {
        self.local_info.len()
    }

    pub fn dummy_expr(&self) -> ExprId {
        self.dummy_expr
    }

    pub fn dummy_pat(&self) -> PatId {
        self.dummy_pat
    }

    pub fn set_current_owner(&mut self, owner: DefId) {
        self.current_owner = Some(owner);
    }

    fn fallback_owner(&self) -> DefId {
        DefId {
            pkg: self.package,
            idx: u32::MAX,
        }
    }

    pub fn fresh_local(&mut self, hint: &str) -> LocalId {
        let owner = self.current_owner.unwrap_or_else(|| self.fallback_owner());
        let serial = self.local_counter;
        self.local_counter += 1;
        let key = LocalKey::Synthetic {
            owner,
            serial,
            hint: hint.to_string(),
        };
        let id = LocalId {
            pkg: self.package,
            idx: self.local_info.len() as u32,
        };
        self.local_interner.insert(key.clone(), id);
        self.local_info.push(LocalInfo {
            hint: hint.to_string(),
            origin: key,
        });
        id
    }

    pub fn alloc_ast_local(&mut self, ptr: MySyntaxNodePtr, hint: &str) -> LocalId {
        let owner = self.current_owner.unwrap_or_else(|| self.fallback_owner());
        let key = LocalKey::AstBinder { owner, ptr };
        if let Some(&id) = self.local_interner.get(&key) {
            return id;
        }
        let id = LocalId {
            pkg: self.package,
            idx: self.local_info.len() as u32,
        };
        self.local_interner.insert(key.clone(), id);
        self.local_info.push(LocalInfo {
            hint: hint.to_string(),
            origin: key,
        });
        id
    }

    pub fn local_hint(&self, id: LocalId) -> &str {
        assert_eq!(id.pkg, self.package);
        &self.local_info[id.idx as usize].hint
    }

    pub fn local_ident_name(&self, id: LocalId) -> String {
        format!("{}/{}", self.local_hint(id), id.idx)
    }

    pub fn iter_locals(&self) -> impl Iterator<Item = (LocalId, &LocalInfo)> {
        self.local_info.iter().enumerate().map(|(idx, info)| {
            (
                LocalId {
                    pkg: self.package,
                    idx: idx as u32,
                },
                info,
            )
        })
    }

    pub fn local_origin_ptr(&self, id: LocalId) -> Option<MySyntaxNodePtr> {
        assert_eq!(id.pkg, self.package);
        match &self.local_info[id.idx as usize].origin {
            LocalKey::AstBinder { ptr, .. } => Some(*ptr),
            LocalKey::Synthetic { .. } => None,
        }
    }

    pub fn alloc_def(&mut self, name: String, kind: DefKind, def: Def) -> DefId {
        let path = Path::from_ident(name);
        let key = DefKey {
            path: path.clone(),
            kind,
            disamb: 0,
        };

        let id = DefId {
            pkg: self.package,
            idx: self.def_data.len() as u32,
        };
        self.def_interner.insert(key, id);
        self.def_data.push(def);
        self.def_paths.push(path);
        id
    }

    pub fn alloc_def_with_path(&mut self, path: Path, kind: DefKind, def: Def) -> DefId {
        let key = DefKey {
            path: path.clone(),
            kind,
            disamb: 0,
        };

        let id = DefId {
            pkg: self.package,
            idx: self.def_data.len() as u32,
        };
        self.def_interner.insert(key, id);
        self.def_data.push(def);
        self.def_paths.push(path);
        id
    }

    pub fn def(&self, id: DefId) -> &Def {
        assert_eq!(id.pkg, self.package);
        &self.def_data[id.idx as usize]
    }

    pub fn def_mut(&mut self, id: DefId) -> &mut Def {
        assert_eq!(id.pkg, self.package);
        &mut self.def_data[id.idx as usize]
    }

    pub fn def_path(&self, id: DefId) -> &Path {
        assert_eq!(id.pkg, self.package);
        &self.def_paths[id.idx as usize]
    }

    pub fn alloc_expr(&mut self, expr: Expr) -> ExprId {
        let idx = self.exprs.alloc(expr);
        let raw = idx.into_raw().into_u32();
        if raw as usize >= self.expr_ptrs.len() {
            self.expr_ptrs.resize(raw as usize + 1, None);
        }
        ExprId {
            pkg: self.package,
            idx: raw,
        }
    }

    pub fn expr(&self, id: ExprId) -> &Expr {
        assert_eq!(id.pkg, self.package);
        let idx = Idx::from_raw(la_arena::RawIdx::from_u32(id.idx));
        &self.exprs[idx]
    }

    pub fn set_expr_ptr(&mut self, id: ExprId, ptr: Option<MySyntaxNodePtr>) {
        assert_eq!(id.pkg, self.package);
        if id.idx as usize >= self.expr_ptrs.len() {
            self.expr_ptrs.resize(id.idx as usize + 1, None);
        }
        self.expr_ptrs[id.idx as usize] = ptr;
    }

    pub fn expr_ptr(&self, id: ExprId) -> Option<MySyntaxNodePtr> {
        assert_eq!(id.pkg, self.package);
        self.expr_ptrs
            .get(id.idx as usize)
            .and_then(|ptr| ptr.as_ref())
            .copied()
    }

    pub fn alloc_pat(&mut self, pat: Pat) -> PatId {
        let idx = self.pats.alloc(pat);
        let raw = idx.into_raw().into_u32();
        if raw as usize >= self.pat_ptrs.len() {
            self.pat_ptrs.resize(raw as usize + 1, None);
        }
        PatId {
            pkg: self.package,
            idx: raw,
        }
    }

    pub fn pat(&self, id: PatId) -> &Pat {
        assert_eq!(id.pkg, self.package);
        let idx = Idx::from_raw(la_arena::RawIdx::from_u32(id.idx));
        &self.pats[idx]
    }

    pub fn set_pat_ptr(&mut self, id: PatId, ptr: Option<MySyntaxNodePtr>) {
        assert_eq!(id.pkg, self.package);
        if id.idx as usize >= self.pat_ptrs.len() {
            self.pat_ptrs.resize(id.idx as usize + 1, None);
        }
        self.pat_ptrs[id.idx as usize] = ptr;
    }

    pub fn pat_ptr(&self, id: PatId) -> Option<MySyntaxNodePtr> {
        assert_eq!(id.pkg, self.package);
        self.pat_ptrs
            .get(id.idx as usize)
            .and_then(|ptr| ptr.as_ref())
            .copied()
    }

    pub fn iter_defs(&self) -> impl Iterator<Item = (DefId, &Def)> {
        self.def_data.iter().enumerate().map(|(i, def)| {
            (
                DefId {
                    pkg: self.package,
                    idx: i as u32,
                },
                def,
            )
        })
    }
}

#[derive(Debug, Clone)]
pub enum HirIdent {
    Name(String),
    Fresh { id: u32 },
}

impl HirIdent {
    pub fn name(s: impl Into<String>) -> Self {
        HirIdent::Name(s.into())
    }

    pub fn fresh(id: u32) -> Self {
        HirIdent::Fresh { id }
    }

    pub fn to_ident_name(&self) -> String {
        match self {
            HirIdent::Name(s) => s.clone(),
            HirIdent::Fresh { id } => id.to_string(),
        }
    }
}

impl PartialEq for HirIdent {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (HirIdent::Name(a), HirIdent::Name(b)) => a == b,
            (HirIdent::Fresh { id: a }, HirIdent::Fresh { id: b }) => a == b,
            _ => false,
        }
    }
}

impl Eq for HirIdent {}

impl std::hash::Hash for HirIdent {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            HirIdent::Name(s) => {
                0u8.hash(state);
                s.hash(state);
            }
            HirIdent::Fresh { id } => {
                1u8.hash(state);
                id.hash(state);
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinId {
    ArrayGet,
    ArraySet,
    Ref,
    RefGet,
    RefSet,
    VecNew,
    VecPush,
    VecGet,
    VecLen,
    Named(u32),
}

impl BuiltinId {
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "array_get" => Some(BuiltinId::ArrayGet),
            "array_set" => Some(BuiltinId::ArraySet),
            "ref" => Some(BuiltinId::Ref),
            "ref_get" => Some(BuiltinId::RefGet),
            "ref_set" => Some(BuiltinId::RefSet),
            "vec_new" => Some(BuiltinId::VecNew),
            "vec_push" => Some(BuiltinId::VecPush),
            "vec_get" => Some(BuiltinId::VecGet),
            "vec_len" => Some(BuiltinId::VecLen),
            _ => None,
        }
    }

    pub fn to_name(&self) -> String {
        match self {
            BuiltinId::ArrayGet => "array_get".to_string(),
            BuiltinId::ArraySet => "array_set".to_string(),
            BuiltinId::Ref => "ref".to_string(),
            BuiltinId::RefGet => "ref_get".to_string(),
            BuiltinId::RefSet => "ref_set".to_string(),
            BuiltinId::VecNew => "vec_new".to_string(),
            BuiltinId::VecPush => "vec_push".to_string(),
            BuiltinId::VecGet => "vec_get".to_string(),
            BuiltinId::VecLen => "vec_len".to_string(),
            BuiltinId::Named(id) => format!("builtin/{}", id),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NameRef {
    Local(LocalId),
    Def(DefId),
    Builtin(BuiltinId),
    Unresolved(Path),
}

impl NameRef {
    pub fn display(&self, hir_table: &ProjectHirTable) -> String {
        match self {
            NameRef::Local(id) => hir_table
                .package(id.pkg)
                .unwrap_or_else(|| panic!("missing HIR table for package {:?}", id.pkg))
                .local_ident_name(*id),
            NameRef::Def(id) => id.to_debug_string(),
            NameRef::Builtin(id) => id.to_name(),
            NameRef::Unresolved(path) => path.display(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathSegment {
    pub seg: String,
}

impl PathSegment {
    pub fn new(seg: String) -> Self {
        Self { seg }
    }

    pub fn seg(&self) -> &String {
        &self.seg
    }
}

impl From<&ast::PathSegment> for PathSegment {
    fn from(seg: &ast::PathSegment) -> Self {
        PathSegment {
            seg: seg.ident.0.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub segments: Vec<PathSegment>,
}

impl Path {
    pub fn new(segments: Vec<PathSegment>) -> Self {
        Self { segments }
    }

    pub fn from_idents(idents: Vec<String>) -> Self {
        let segments = idents.into_iter().map(PathSegment::new).collect();
        Self { segments }
    }

    pub fn from_ident(ident: String) -> Self {
        Self {
            segments: vec![PathSegment::new(ident)],
        }
    }

    pub fn segments(&self) -> &[PathSegment] {
        &self.segments
    }

    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn len(&self) -> usize {
        self.segments.len()
    }

    pub fn last(&self) -> Option<&PathSegment> {
        self.segments.last()
    }

    pub fn last_ident(&self) -> Option<&String> {
        self.last().map(|segment| segment.seg())
    }

    pub fn namespace_segments(&self) -> &[PathSegment] {
        if self.segments.len() > 1 {
            &self.segments[..self.segments.len() - 1]
        } else {
            &[]
        }
    }

    pub fn parent_ident(&self) -> Option<&String> {
        if self.segments.len() > 1 {
            Some(self.segments[self.segments.len() - 2].seg())
        } else {
            None
        }
    }

    pub fn display(&self) -> String {
        self.segments
            .iter()
            .map(|segment| segment.seg.clone())
            .collect::<Vec<_>>()
            .join("::")
    }
}

impl From<&ast::Path> for Path {
    fn from(path: &ast::Path) -> Self {
        Path {
            segments: path.segments.iter().map(|s| s.into()).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct QualifiedPath {
    pub package: Option<PackageName>,
    pub path: Path,
}

impl QualifiedPath {
    pub fn len(&self) -> usize {
        self.path.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn last_ident(&self) -> Option<&String> {
        self.path.last_ident()
    }

    pub fn display(&self) -> String {
        match &self.package {
            Some(package) => format!("{}::{}", package.0, self.path.display()),
            None => self.path.display(),
        }
    }
}

impl From<&ast::Path> for QualifiedPath {
    fn from(path: &ast::Path) -> Self {
        if path.segments.len() <= 1 {
            return QualifiedPath {
                package: None,
                path: path.into(),
            };
        }
        let package = path.segments[0].ident.0.clone();
        let segments = path.segments[1..].iter().map(|seg| seg.into()).collect();
        QualifiedPath {
            package: Some(PackageName(package)),
            path: Path::new(segments),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
    TUnit,
    TBool,
    TInt8,
    TInt16,
    TInt32,
    TInt64,
    TUint8,
    TUint16,
    TUint32,
    TUint64,
    TFloat32,
    TFloat64,
    TString,
    TTuple {
        typs: Vec<TypeExpr>,
    },
    TCon {
        path: QualifiedPath,
    },
    TDyn {
        trait_path: QualifiedPath,
    },
    TApp {
        ty: Box<TypeExpr>,
        args: Vec<TypeExpr>,
    },
    TArray {
        len: usize,
        elem: Box<TypeExpr>,
    },
    TFunc {
        params: Vec<TypeExpr>,
        ret_ty: Box<TypeExpr>,
    },
}

impl From<&ast::TypeExpr> for TypeExpr {
    fn from(ty: &ast::TypeExpr) -> Self {
        match ty {
            ast::TypeExpr::TUnit => TypeExpr::TUnit,
            ast::TypeExpr::TBool => TypeExpr::TBool,
            ast::TypeExpr::TInt8 => TypeExpr::TInt8,
            ast::TypeExpr::TInt16 => TypeExpr::TInt16,
            ast::TypeExpr::TInt32 => TypeExpr::TInt32,
            ast::TypeExpr::TInt64 => TypeExpr::TInt64,
            ast::TypeExpr::TUint8 => TypeExpr::TUint8,
            ast::TypeExpr::TUint16 => TypeExpr::TUint16,
            ast::TypeExpr::TUint32 => TypeExpr::TUint32,
            ast::TypeExpr::TUint64 => TypeExpr::TUint64,
            ast::TypeExpr::TFloat32 => TypeExpr::TFloat32,
            ast::TypeExpr::TFloat64 => TypeExpr::TFloat64,
            ast::TypeExpr::TString => TypeExpr::TString,
            ast::TypeExpr::TTuple { typs } => TypeExpr::TTuple {
                typs: typs.iter().map(|t| t.into()).collect(),
            },
            ast::TypeExpr::TCon { path } => TypeExpr::TCon { path: path.into() },
            ast::TypeExpr::TDyn { trait_path } => TypeExpr::TDyn {
                trait_path: trait_path.into(),
            },
            ast::TypeExpr::TApp { ty, args } => TypeExpr::TApp {
                ty: Box::new(ty.as_ref().into()),
                args: args.iter().map(|a| a.into()).collect(),
            },
            ast::TypeExpr::TArray { len, elem } => TypeExpr::TArray {
                len: *len,
                elem: Box::new(elem.as_ref().into()),
            },
            ast::TypeExpr::TFunc { params, ret_ty } => TypeExpr::TFunc {
                params: params.iter().map(|p| p.into()).collect(),
                ret_ty: Box::new(ret_ty.as_ref().into()),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct ClosureParam {
    pub name: LocalId,
    pub ty: Option<TypeExpr>,
    pub astptr: MySyntaxNodePtr,
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub ast: MySyntaxNodePtr,
    pub text: String,
}

impl From<&ast::Attribute> for Attribute {
    fn from(attr: &ast::Attribute) -> Self {
        Attribute {
            ast: attr.ast,
            text: attr.text.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct File {
    pub toplevels: Vec<DefId>,
}

#[derive(Debug, Clone)]
pub enum Def {
    EnumDef(EnumDef),
    StructDef(StructDef),
    TraitDef(TraitDef),
    ImplBlock(ImplBlock),
    Fn(Fn),
    ExternGo(ExternGo),
    ExternType(ExternType),
    ExternBuiltin(ExternBuiltin),
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub attrs: Vec<Attribute>,
    pub name: String,
    pub generics: Vec<HirIdent>,
    pub generic_bounds: Vec<(HirIdent, Vec<Path>)>,
    pub params: Vec<(LocalId, TypeExpr)>,
    pub ret_ty: Option<TypeExpr>,
    pub body: ExprId,
}

#[derive(Debug, Clone)]
pub struct ExternGo {
    pub attrs: Vec<Attribute>,
    pub package_path: String,
    pub go_symbol: String,
    pub goml_name: HirIdent,
    pub explicit_go_symbol: bool,
    pub params: Vec<(HirIdent, TypeExpr)>,
    pub ret_ty: Option<TypeExpr>,
}

impl From<&ast::ExternGo> for ExternGo {
    fn from(ext: &ast::ExternGo) -> Self {
        ExternGo {
            attrs: ext.attrs.iter().map(|a| a.into()).collect(),
            package_path: ext.package_path.clone(),
            go_symbol: ext.go_symbol.clone(),
            goml_name: HirIdent::name(&ext.goml_name.0),
            explicit_go_symbol: ext.explicit_go_symbol,
            params: ext
                .params
                .iter()
                .map(|(i, t)| (HirIdent::name(&i.0), t.into()))
                .collect(),
            ret_ty: ext.ret_ty.as_ref().map(|t| t.into()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternType {
    pub attrs: Vec<Attribute>,
    pub goml_name: HirIdent,
}

impl From<&ast::ExternType> for ExternType {
    fn from(ext: &ast::ExternType) -> Self {
        ExternType {
            attrs: ext.attrs.iter().map(|a| a.into()).collect(),
            goml_name: HirIdent::name(&ext.goml_name.0),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternBuiltin {
    pub attrs: Vec<Attribute>,
    pub name: HirIdent,
    pub params: Vec<(HirIdent, TypeExpr)>,
    pub ret_ty: Option<TypeExpr>,
}

impl From<&ast::ExternBuiltin> for ExternBuiltin {
    fn from(ext: &ast::ExternBuiltin) -> Self {
        ExternBuiltin {
            attrs: ext.attrs.iter().map(|a| a.into()).collect(),
            name: HirIdent::name(&ext.name.0),
            params: ext
                .params
                .iter()
                .map(|(i, t)| (HirIdent::name(&i.0), t.into()))
                .collect(),
            ret_ty: ext.ret_ty.as_ref().map(|t| t.into()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub attrs: Vec<Attribute>,
    pub name: HirIdent,
    pub generics: Vec<HirIdent>,
    pub variants: Vec<(HirIdent, Vec<TypeExpr>)>,
}

impl From<&ast::EnumDef> for EnumDef {
    fn from(e: &ast::EnumDef) -> Self {
        EnumDef {
            attrs: e.attrs.iter().map(|a| a.into()).collect(),
            name: HirIdent::name(&e.name.0),
            generics: e.generics.iter().map(|g| HirIdent::name(&g.0)).collect(),
            variants: e
                .variants
                .iter()
                .map(|(i, tys)| (HirIdent::name(&i.0), tys.iter().map(|t| t.into()).collect()))
                .collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub attrs: Vec<Attribute>,
    pub name: HirIdent,
    pub generics: Vec<HirIdent>,
    pub fields: Vec<(HirIdent, TypeExpr)>,
}

impl From<&ast::StructDef> for StructDef {
    fn from(s: &ast::StructDef) -> Self {
        StructDef {
            attrs: s.attrs.iter().map(|a| a.into()).collect(),
            name: HirIdent::name(&s.name.0),
            generics: s.generics.iter().map(|g| HirIdent::name(&g.0)).collect(),
            fields: s
                .fields
                .iter()
                .map(|(i, t)| (HirIdent::name(&i.0), t.into()))
                .collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TraitDef {
    pub attrs: Vec<Attribute>,
    pub name: HirIdent,
    pub method_sigs: Vec<TraitMethodSignature>,
}

impl From<&ast::TraitDef> for TraitDef {
    fn from(t: &ast::TraitDef) -> Self {
        TraitDef {
            attrs: t.attrs.iter().map(|a| a.into()).collect(),
            name: HirIdent::name(&t.name.0),
            method_sigs: t.method_sigs.iter().map(|m| m.into()).collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TraitMethodSignature {
    pub name: HirIdent,
    pub params: Vec<TypeExpr>,
    pub ret_ty: TypeExpr,
}

impl From<&ast::TraitMethodSignature> for TraitMethodSignature {
    fn from(m: &ast::TraitMethodSignature) -> Self {
        TraitMethodSignature {
            name: HirIdent::name(&m.name.0),
            params: m.params.iter().map(|p| p.into()).collect(),
            ret_ty: (&m.ret_ty).into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImplBlock {
    pub attrs: Vec<Attribute>,
    pub generics: Vec<HirIdent>,
    pub trait_name: Option<HirIdent>,
    pub for_type: TypeExpr,
    pub methods: Vec<DefId>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    ENameRef {
        res: NameRef,
        hint: String,
        astptr: Option<MySyntaxNodePtr>,
    },
    EStaticMember {
        path: Path,
        astptr: Option<MySyntaxNodePtr>,
    },
    EUnit,
    EBool {
        value: bool,
    },
    EInt {
        value: String,
    },
    EInt8 {
        value: String,
    },
    EInt16 {
        value: String,
    },
    EInt32 {
        value: String,
    },
    EInt64 {
        value: String,
    },
    EUInt8 {
        value: String,
    },
    EUInt16 {
        value: String,
    },
    EUInt32 {
        value: String,
    },
    EUInt64 {
        value: String,
    },
    EFloat {
        value: f64,
    },
    EFloat32 {
        value: String,
    },
    EFloat64 {
        value: String,
    },
    EString {
        value: String,
    },
    EConstr {
        constructor: ConstructorRef,
        args: Vec<ExprId>,
    },
    EStructLiteral {
        name: QualifiedPath,
        fields: Vec<(HirIdent, ExprId)>,
    },
    ETuple {
        items: Vec<ExprId>,
    },
    EArray {
        items: Vec<ExprId>,
    },
    ELet {
        pat: PatId,
        annotation: Option<TypeExpr>,
        value: ExprId,
    },
    EClosure {
        params: Vec<ClosureParam>,
        body: ExprId,
    },
    EMatch {
        expr: ExprId,
        arms: Vec<Arm>,
    },
    EIf {
        cond: ExprId,
        then_branch: ExprId,
        else_branch: ExprId,
    },
    EWhile {
        cond: ExprId,
        body: ExprId,
    },
    EGo {
        expr: ExprId,
    },
    ECall {
        func: ExprId,
        args: Vec<ExprId>,
    },
    EUnary {
        op: common_defs::UnaryOp,
        expr: ExprId,
    },
    EBinary {
        op: common_defs::BinaryOp,
        lhs: ExprId,
        rhs: ExprId,
    },
    EProj {
        tuple: ExprId,
        index: usize,
    },
    EField {
        expr: ExprId,
        field: HirIdent,
    },
    EBlock {
        exprs: Vec<ExprId>,
    },
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub pat: PatId,
    pub body: ExprId,
}

#[derive(Debug, Clone)]
pub enum Pat {
    PVar {
        name: LocalId,
        astptr: MySyntaxNodePtr,
    },
    PUnit,
    PBool {
        value: bool,
    },
    PInt {
        value: String,
    },
    PInt8 {
        value: String,
    },
    PInt16 {
        value: String,
    },
    PInt32 {
        value: String,
    },
    PInt64 {
        value: String,
    },
    PUInt8 {
        value: String,
    },
    PUInt16 {
        value: String,
    },
    PUInt32 {
        value: String,
    },
    PUInt64 {
        value: String,
    },
    PString {
        value: String,
    },
    PConstr {
        constructor: ConstructorRef,
        args: Vec<PatId>,
    },
    PStruct {
        name: QualifiedPath,
        fields: Vec<(HirIdent, PatId)>,
    },
    PTuple {
        pats: Vec<PatId>,
    },
    PWild,
}

#[derive(Debug, Clone)]
pub struct ConstructorResolutionError {
    pub path: Path,
    pub kind: ConstructorResolutionErrorKind,
}

#[derive(Debug, Clone)]
pub enum ConstructorResolutionErrorKind {
    NotFound,
    Ambiguous(Vec<ConstructorId>),
}

pub fn resolve_constructors(hir_table: &mut HirTable) -> Vec<ConstructorResolutionError> {
    let mut errors = Vec::new();
    let mut full_name_index: HashMap<String, ConstructorId> = HashMap::new();
    let mut short_name_index: HashMap<String, Vec<ConstructorId>> = HashMap::new();

    for (def_id, def) in hir_table.iter_defs() {
        if let Def::EnumDef(enum_def) = def {
            let enum_name = hir_table.def_path(def_id).display();
            for (variant_idx, (variant_name, _)) in enum_def.variants.iter().enumerate() {
                let ctor_id = ConstructorId::EnumVariant {
                    enum_def: def_id,
                    variant_idx: variant_idx as u32,
                };
                let variant_ident = variant_name.to_ident_name();
                let full_name = format!("{}::{}", enum_name, variant_ident);
                full_name_index.insert(full_name, ctor_id);
                short_name_index
                    .entry(variant_ident)
                    .or_default()
                    .push(ctor_id);
            }
        }
    }

    let expr_count = hir_table.exprs.len();
    for i in 0..expr_count {
        let expr_id = ExprId {
            pkg: hir_table.package(),
            idx: i as u32,
        };
        let expr = hir_table.expr(expr_id).clone();
        if let Expr::EConstr { constructor, args } = expr
            && let ConstructorRef::Unresolved(path) = &constructor
        {
            let resolved =
                resolve_constructor_path(path, &full_name_index, &short_name_index, &mut errors);
            let new_expr = Expr::EConstr {
                constructor: resolved,
                args,
            };
            let idx = la_arena::Idx::from_raw(la_arena::RawIdx::from_u32(expr_id.idx));
            hir_table.exprs[idx] = new_expr;
        }
    }

    let pat_count = hir_table.pats.len();
    for i in 0..pat_count {
        let pat_id = PatId {
            pkg: hir_table.package(),
            idx: i as u32,
        };
        let pat = hir_table.pat(pat_id).clone();
        if let Pat::PConstr { constructor, args } = pat
            && let ConstructorRef::Unresolved(path) = &constructor
        {
            let resolved =
                resolve_constructor_path(path, &full_name_index, &short_name_index, &mut errors);
            let new_pat = Pat::PConstr {
                constructor: resolved,
                args,
            };
            let idx = la_arena::Idx::from_raw(la_arena::RawIdx::from_u32(pat_id.idx));
            hir_table.pats[idx] = new_pat;
        }
    }

    errors
}

fn resolve_constructor_path(
    path: &Path,
    full_name_index: &HashMap<String, ConstructorId>,
    short_name_index: &HashMap<String, Vec<ConstructorId>>,
    errors: &mut Vec<ConstructorResolutionError>,
) -> ConstructorRef {
    let path_str = path.display();

    if let Some(&ctor_id) = full_name_index.get(&path_str) {
        return ConstructorRef::Resolved(ctor_id);
    }

    if path.segments.len() >= 2 {
        let suffix = format!("::{}", path_str);
        let matches: Vec<ConstructorId> = full_name_index
            .iter()
            .filter_map(|(key, ctor)| key.ends_with(&suffix).then_some(*ctor))
            .collect();
        if matches.len() == 1 {
            return ConstructorRef::Resolved(matches[0]);
        }
        if matches.len() > 1 {
            let candidates = matches.clone();
            errors.push(ConstructorResolutionError {
                path: path.clone(),
                kind: ConstructorResolutionErrorKind::Ambiguous(matches),
            });
            return ConstructorRef::Ambiguous {
                path: path.clone(),
                candidates,
            };
        }
    }

    if path.segments.len() == 1 {
        let short_name = &path.segments[0].seg;
        if let Some(ctors) = short_name_index.get(short_name) {
            match ctors.len() {
                0 => {}
                1 => return ConstructorRef::Resolved(ctors[0]),
                _ => {
                    errors.push(ConstructorResolutionError {
                        path: path.clone(),
                        kind: ConstructorResolutionErrorKind::Ambiguous(ctors.clone()),
                    });
                    return ConstructorRef::Ambiguous {
                        path: path.clone(),
                        candidates: ctors.clone(),
                    };
                }
            }
        }
    }

    errors.push(ConstructorResolutionError {
        path: path.clone(),
        kind: ConstructorResolutionErrorKind::NotFound,
    });
    ConstructorRef::Unresolved(path.clone())
}
