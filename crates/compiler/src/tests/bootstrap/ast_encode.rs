use std::fmt::Write;
use std::path::Path as FsPath;

use ast::ast::{
    Arm, ArrayPatRest, AstIdent, Attribute, Block, Expr, File, Fn, ImplBlock, Item, Pat, Path,
    Predicate, Stmt, TraitDef, TraitRef, TypeExpr, Visibility,
};
use cst::cst::CstNode;
use parser::syntax::{MySyntaxNode, MySyntaxNodePtr};

struct Encoder {
    output: String,
}

impl Encoder {
    fn new() -> Self {
        Self {
            output: String::new(),
        }
    }

    fn open(&mut self, tag: &str) {
        writeln!(self.output, "O\t{tag}").unwrap();
    }

    fn close(&mut self) {
        self.output.push_str("C\n");
    }

    fn string(&mut self, value: &str) {
        self.output.push_str("S\t");
        for byte in value.bytes() {
            write!(self.output, "{byte:02x}").unwrap();
        }
        self.output.push('\n');
    }

    fn boolean(&mut self, value: bool) {
        self.output
            .push_str(if value { "B\t1\n" } else { "B\t0\n" });
    }

    fn integer(&mut self, value: usize) {
        writeln!(self.output, "I\t{value}").unwrap();
    }

    fn list(&mut self, length: usize) {
        writeln!(self.output, "L\t{length}").unwrap();
    }

    fn none(&mut self) {
        self.output.push_str("N\n");
    }

    fn some(&mut self) {
        self.output.push_str("Y\n");
    }

    fn range(&mut self, range: text_size::TextRange) {
        writeln!(
            self.output,
            "R\t{}\t{}",
            u32::from(range.start()),
            u32::from(range.end())
        )
        .unwrap();
    }

    fn ptr(&mut self, ptr: &MySyntaxNodePtr) {
        self.range(ptr.text_range());
    }

    fn path(&mut self, path: &Path) {
        self.open("Path");
        self.list(path.segments.len());
        for segment in &path.segments {
            self.open("Segment");
            self.string(&segment.ident.0);
            self.range(segment.range.unwrap_or_default());
            self.close();
        }
        self.close();
    }

    fn ty(&mut self, ty: &TypeExpr) {
        match ty {
            TypeExpr::TUnit => self.open("Type.Unit"),
            TypeExpr::TBool => self.open("Type.Bool"),
            TypeExpr::TInt8 => self.open("Type.Int8"),
            TypeExpr::TInt16 => self.open("Type.Int16"),
            TypeExpr::TInt32 => self.open("Type.Int32"),
            TypeExpr::TInt64 => self.open("Type.Int64"),
            TypeExpr::TUint8 => self.open("Type.Uint8"),
            TypeExpr::TUint16 => self.open("Type.Uint16"),
            TypeExpr::TUint32 => self.open("Type.Uint32"),
            TypeExpr::TUint64 => self.open("Type.Uint64"),
            TypeExpr::TFloat32 => self.open("Type.Float32"),
            TypeExpr::TFloat64 => self.open("Type.Float64"),
            TypeExpr::TString => self.open("Type.String"),
            TypeExpr::TChar => self.open("Type.Char"),
            TypeExpr::TTuple { typs } => {
                self.open("Type.Tuple");
                self.types(typs);
            }
            TypeExpr::TCon { path } => {
                self.open("Type.Con");
                self.path(path);
            }
            TypeExpr::TDyn { trait_path } => {
                self.open("Type.Dyn");
                self.path(trait_path);
            }
            TypeExpr::TApp { ty, args } => {
                self.open("Type.App");
                self.ty(ty);
                self.types(args);
            }
            TypeExpr::TArray { len, elem } => {
                self.open("Type.Array");
                self.string(&len.to_string());
                self.ty(elem);
            }
            TypeExpr::TFunc { params, ret_ty } => {
                self.open("Type.Func");
                self.types(params);
                self.ty(ret_ty);
            }
        }
        self.close();
    }

    fn types(&mut self, values: &[TypeExpr]) {
        self.list(values.len());
        for value in values {
            self.ty(value);
        }
    }

    fn attribute(&mut self, value: &Attribute) {
        self.open("Attribute");
        self.ptr(&value.ast);
        self.string(&value.text);
        self.close();
    }

    fn attributes(&mut self, values: &[Attribute]) {
        self.list(values.len());
        for value in values {
            self.attribute(value);
        }
    }

    fn visibility(&mut self, value: Visibility) {
        self.string(match value {
            Visibility::Private => "Private",
            Visibility::Public => "Public",
        });
    }

    fn trait_ref(&mut self, value: &TraitRef) {
        self.open("TraitRef");
        self.path(&value.path);
        self.types(&value.args);
        self.close();
    }

    fn trait_refs(&mut self, values: &[TraitRef]) {
        self.list(values.len());
        for value in values {
            self.trait_ref(value);
        }
    }

    fn generics(&mut self, values: &[AstIdent]) {
        self.list(values.len());
        for value in values {
            self.string(&value.0);
        }
    }

    fn generic_bounds(&mut self, values: &[(AstIdent, Vec<TraitRef>)]) {
        self.list(values.len());
        for (name, bounds) in values {
            self.open("GenericBound");
            self.string(&name.0);
            self.trait_refs(bounds);
            self.close();
        }
    }

    fn predicates(&mut self, values: &[Predicate]) {
        self.list(values.len());
        for value in values {
            match value {
                Predicate::Trait { ty, trait_ref } => {
                    self.open("Predicate.Trait");
                    self.ty(ty);
                    self.trait_ref(trait_ref);
                }
                Predicate::Equality { lhs, rhs } => {
                    self.open("Predicate.Equality");
                    self.ty(lhs);
                    self.ty(rhs);
                }
            }
            self.close();
        }
    }

    fn params(&mut self, values: &[(AstIdent, TypeExpr)]) {
        self.list(values.len());
        for (name, ty) in values {
            self.open("Param");
            self.string(&name.0);
            self.ty(ty);
            self.close();
        }
    }

    fn optional_type(&mut self, value: Option<&TypeExpr>) {
        if let Some(value) = value {
            self.some();
            self.ty(value);
        } else {
            self.none();
        }
    }

    fn function(&mut self, value: &Fn) {
        self.open("Function");
        self.attributes(&value.attrs);
        self.visibility(value.visibility);
        self.string(&value.name.0);
        self.generics(&value.generics);
        self.generic_bounds(&value.generic_bounds);
        self.predicates(&value.predicates);
        self.params(&value.params);
        self.optional_type(value.ret_ty.as_ref());
        self.block(&value.body);
        self.close();
    }

    fn extern_function(&mut self, value: &ast::ast::ExternFn) {
        self.open("ExternFunction");
        self.attributes(&value.attrs);
        self.visibility(value.visibility);
        self.string(&value.name.0);
        self.generics(&value.generics);
        self.generic_bounds(&value.generic_bounds);
        self.predicates(&value.predicates);
        self.params(&value.params);
        self.optional_type(value.ret_ty.as_ref());
        self.close();
    }

    fn enum_def(&mut self, value: &ast::ast::EnumDef) {
        self.open("Enum");
        self.attributes(&value.attrs);
        self.visibility(value.visibility);
        self.string(&value.name.0);
        self.generics(&value.generics);
        self.list(value.variants.len());
        for variant in &value.variants {
            self.open("Variant");
            self.string(&variant.name.0);
            match &variant.fields {
                ast::ast::EnumVariantFields::Unit => self.open("Unit"),
                ast::ast::EnumVariantFields::Tuple(types) => {
                    self.open("Tuple");
                    self.types(types);
                }
                ast::ast::EnumVariantFields::Struct(fields) => {
                    self.open("Struct");
                    self.list(fields.len());
                    for (name, ty) in fields {
                        self.open("Field");
                        self.string(&name.0);
                        self.ty(ty);
                        self.close();
                    }
                }
            }
            self.close();
            self.close();
        }
        self.close();
    }

    fn struct_def(&mut self, value: &ast::ast::StructDef) {
        self.open("Struct");
        self.attributes(&value.attrs);
        self.visibility(value.visibility);
        self.string(&value.name.0);
        self.generics(&value.generics);
        self.list(value.fields.len());
        for (name, ty) in &value.fields {
            self.open("Field");
            self.string(&name.0);
            self.ty(ty);
            self.close();
        }
        self.list(value.public_fields.len());
        for field in &value.public_fields {
            self.string(&field.0);
        }
        self.close();
    }

    fn trait_def(&mut self, value: &TraitDef) {
        self.open("Trait");
        self.attributes(&value.attrs);
        self.visibility(value.visibility);
        self.string(&value.name.0);
        self.generics(&value.generics);
        self.generic_bounds(&value.generic_bounds);
        self.predicates(&value.predicates);
        self.trait_refs(&value.supertraits);
        self.list(value.associated_types.len());
        for associated in &value.associated_types {
            self.open("AssociatedType");
            self.string(&associated.name.0);
            self.trait_refs(&associated.bounds);
            self.close();
        }
        self.list(value.method_sigs.len());
        for method in &value.method_sigs {
            self.open("TraitMethod");
            self.string(&method.name.0);
            self.list(method.params.len());
            for (name, ty) in &method.params {
                self.string(&name.0);
                self.ty(ty);
            }
            self.ty(&method.ret_ty);
            self.close();
        }
        self.close();
    }

    fn impl_block(&mut self, value: &ImplBlock) {
        self.open("Impl");
        self.attributes(&value.attrs);
        self.generics(&value.generics);
        self.generic_bounds(&value.generic_bounds);
        self.predicates(&value.predicates);
        self.list(value.associated_types.len());
        for (name, ty) in &value.associated_types {
            self.open("ImplAssociatedType");
            self.string(&name.0);
            self.ty(ty);
            self.close();
        }
        if let Some(trait_ref) = &value.trait_ref {
            self.some();
            self.trait_ref(trait_ref);
        } else {
            self.none();
        }
        self.ty(&value.for_type);
        self.list(value.methods.len());
        for method in &value.methods {
            self.function(method);
        }
        self.close();
    }

    fn item(&mut self, value: &Item) {
        match value {
            Item::EnumDef(value) => self.enum_def(value),
            Item::StructDef(value) => self.struct_def(value),
            Item::TraitDef(value) => self.trait_def(value),
            Item::ImplBlock(value) => self.impl_block(value),
            Item::Fn(value) => self.function(value),
            Item::ExternFn(value) => self.extern_function(value),
        }
    }

    fn block(&mut self, value: &Block) {
        self.open("Block");
        self.ptr(&value.astptr);
        self.list(value.stmts.len());
        for stmt in &value.stmts {
            self.stmt(stmt);
        }
        if let Some(tail) = &value.tail {
            self.some();
            self.expr(tail);
        } else {
            self.none();
        }
        self.close();
    }

    fn stmt(&mut self, value: &Stmt) {
        match value {
            Stmt::Let(value) => {
                self.open("Stmt.Let");
                self.ptr(&value.astptr);
                self.boolean(value.is_mut);
                self.pat(&value.pat);
                self.optional_type(value.annotation.as_ref());
                self.expr(&value.value);
            }
            Stmt::Assign(value) => {
                self.open("Stmt.Assign");
                self.ptr(&value.astptr);
                self.expr(&value.target);
                self.expr(&value.value);
            }
            Stmt::Expr(value) => {
                self.open("Stmt.Expr");
                self.ptr(&value.astptr);
                self.expr(&value.expr);
            }
        }
        self.close();
    }

    fn file(&mut self, value: &File) {
        self.open("File");
        self.string(&value.package.0);
        self.boolean(value.package_explicit);
        self.list(value.uses.len());
        for use_decl in &value.uses {
            self.open("Use");
            self.path(&use_decl.path);
            if let Some(alias) = &use_decl.alias {
                self.some();
                self.string(&alias.0);
            } else {
                self.none();
            }
            self.close();
        }
        self.list(value.toplevels.len());
        for item in &value.toplevels {
            self.item(item);
        }
        self.close();
    }

    fn expressions(&mut self, values: &[Expr]) {
        self.list(values.len());
        for value in values {
            self.expr(value);
        }
    }

    fn patterns(&mut self, values: &[Pat]) {
        self.list(values.len());
        for value in values {
            self.pat(value);
        }
    }

    fn arm(&mut self, value: &Arm) {
        self.open("Arm");
        self.pat(&value.pat);
        if let Some(guard) = &value.guard {
            self.some();
            self.expr(guard);
        } else {
            self.none();
        }
        self.expr(&value.body);
        self.close();
    }

    fn literal_expr(&mut self, tag: &str, value: &str, ptr: &MySyntaxNodePtr) {
        self.open(&format!("Expr.{tag}"));
        self.ptr(ptr);
        self.string(value);
    }

    fn expr(&mut self, value: &Expr) {
        match value {
            Expr::EPath {
                path,
                type_args,
                astptr,
            } => {
                self.open("Expr.Path");
                self.ptr(astptr);
                self.path(path);
                self.types(type_args);
            }
            Expr::EUnit { astptr } => {
                self.open("Expr.Unit");
                self.ptr(astptr);
            }
            Expr::EBool { value, astptr } => {
                self.open("Expr.Bool");
                self.ptr(astptr);
                self.boolean(*value);
            }
            Expr::EInt { value, astptr } => self.literal_expr("Int", value, astptr),
            Expr::EInt8 { value, astptr } => self.literal_expr("Int8", value, astptr),
            Expr::EInt16 { value, astptr } => self.literal_expr("Int16", value, astptr),
            Expr::EInt32 { value, astptr } => self.literal_expr("Int32", value, astptr),
            Expr::EInt64 { value, astptr } => self.literal_expr("Int64", value, astptr),
            Expr::EUInt8 { value, astptr } => self.literal_expr("UInt8", value, astptr),
            Expr::EUInt16 { value, astptr } => self.literal_expr("UInt16", value, astptr),
            Expr::EUInt32 { value, astptr } => self.literal_expr("UInt32", value, astptr),
            Expr::EUInt64 { value, astptr } => self.literal_expr("UInt64", value, astptr),
            Expr::EFloat { value, astptr } => {
                self.literal_expr("Float", &value.to_bits().to_string(), astptr);
            }
            Expr::EFloat32 { value, astptr } => self.literal_expr("Float32", value, astptr),
            Expr::EFloat64 { value, astptr } => self.literal_expr("Float64", value, astptr),
            Expr::EString { value, astptr } => self.literal_expr("String", value, astptr),
            Expr::EChar { value, astptr } => self.literal_expr("Char", value, astptr),
            Expr::EConstr {
                constructor,
                args,
                astptr,
            } => {
                self.open("Expr.Constr");
                self.ptr(astptr);
                self.path(constructor);
                self.expressions(args);
            }
            Expr::EStructLiteral {
                name,
                fields,
                astptr,
            } => {
                self.open("Expr.StructLiteral");
                self.ptr(astptr);
                self.path(name);
                self.list(fields.len());
                for (name, expr) in fields {
                    self.open("ExprField");
                    self.string(&name.0);
                    self.expr(expr);
                    self.close();
                }
            }
            Expr::ETuple { items, astptr } => {
                self.open("Expr.Tuple");
                self.ptr(astptr);
                self.expressions(items);
            }
            Expr::EArray { items, astptr } => {
                self.open("Expr.Array");
                self.ptr(astptr);
                self.expressions(items);
            }
            Expr::EClosure {
                params,
                body,
                astptr,
            } => {
                self.open("Expr.Closure");
                self.ptr(astptr);
                self.list(params.len());
                for param in params {
                    self.open("ClosureParam");
                    self.ptr(&param.astptr);
                    self.string(&param.name.0);
                    self.optional_type(param.ty.as_ref());
                    self.close();
                }
                self.expr(body);
            }
            Expr::EMatch { expr, arms, astptr } => {
                self.open("Expr.Match");
                self.ptr(astptr);
                self.expr(expr);
                self.list(arms.len());
                for arm in arms {
                    self.arm(arm);
                }
            }
            Expr::EIf {
                cond,
                then_branch,
                else_branch,
                astptr,
            } => {
                self.open("Expr.If");
                self.ptr(astptr);
                self.expr(cond);
                self.expr(then_branch);
                self.expr(else_branch);
            }
            Expr::EWhile { cond, body, astptr } => {
                self.open("Expr.While");
                self.ptr(astptr);
                self.expr(cond);
                self.expr(body);
            }
            Expr::EFor {
                pat,
                iterator,
                body,
                astptr,
            } => {
                self.open("Expr.For");
                self.ptr(astptr);
                self.pat(pat);
                self.expr(iterator);
                self.expr(body);
            }
            Expr::EBreak { astptr } => {
                self.open("Expr.Break");
                self.ptr(astptr);
            }
            Expr::EContinue { astptr } => {
                self.open("Expr.Continue");
                self.ptr(astptr);
            }
            Expr::EReturn { expr, astptr } => {
                self.open("Expr.Return");
                self.ptr(astptr);
                if let Some(expr) = expr {
                    self.some();
                    self.expr(expr);
                } else {
                    self.none();
                }
            }
            Expr::EGo { expr, astptr } => {
                self.open("Expr.Go");
                self.ptr(astptr);
                self.expr(expr);
            }
            Expr::ECall { func, args, astptr } => {
                self.open("Expr.Call");
                self.ptr(astptr);
                self.expr(func);
                self.expressions(args);
            }
            Expr::EUnary { op, expr, astptr } => {
                self.open("Expr.Unary");
                self.ptr(astptr);
                self.string(&format!("{op:?}"));
                self.expr(expr);
            }
            Expr::ECast { expr, ty, astptr } => {
                self.open("Expr.Cast");
                self.ptr(astptr);
                self.expr(expr);
                self.ty(ty);
            }
            Expr::ETry { expr, astptr } => {
                self.open("Expr.Try");
                self.ptr(astptr);
                self.expr(expr);
            }
            Expr::EBinary {
                op,
                lhs,
                rhs,
                astptr,
            } => {
                self.open("Expr.Binary");
                self.ptr(astptr);
                self.string(&format!("{op:?}"));
                self.expr(lhs);
                self.expr(rhs);
            }
            Expr::EProj {
                tuple,
                index,
                astptr,
            } => {
                self.open("Expr.Proj");
                self.ptr(astptr);
                self.expr(tuple);
                self.integer(*index);
            }
            Expr::EField {
                expr,
                field,
                astptr,
            } => {
                self.open("Expr.Field");
                self.ptr(astptr);
                self.expr(expr);
                self.string(&field.0);
            }
            Expr::EIndex {
                base,
                index,
                astptr,
            } => {
                self.open("Expr.Index");
                self.ptr(astptr);
                self.expr(base);
                self.expr(index);
            }
            Expr::EBlock { block, astptr } => {
                self.open("Expr.Block");
                self.ptr(astptr);
                self.block(block);
            }
        }
        self.close();
    }

    fn literal_pat(&mut self, tag: &str, value: &str, ptr: &MySyntaxNodePtr) {
        self.open(&format!("Pat.{tag}"));
        self.ptr(ptr);
        self.string(value);
    }

    fn pat_rest(&mut self, value: &ArrayPatRest) {
        self.open("PatRest");
        self.ptr(&value.astptr);
        if let Some(binding) = &value.binding {
            self.some();
            self.string(&binding.0);
        } else {
            self.none();
        }
        self.close();
    }

    fn pat(&mut self, value: &Pat) {
        match value {
            Pat::PVar { name, astptr } => self.literal_pat("Var", &name.0, astptr),
            Pat::PUnit { astptr } => {
                self.open("Pat.Unit");
                self.ptr(astptr);
            }
            Pat::PBool { value, astptr } => {
                self.open("Pat.Bool");
                self.ptr(astptr);
                self.boolean(*value);
            }
            Pat::PInt { value, astptr } => self.literal_pat("Int", value, astptr),
            Pat::PInt8 { value, astptr } => self.literal_pat("Int8", value, astptr),
            Pat::PInt16 { value, astptr } => self.literal_pat("Int16", value, astptr),
            Pat::PInt32 { value, astptr } => self.literal_pat("Int32", value, astptr),
            Pat::PInt64 { value, astptr } => self.literal_pat("Int64", value, astptr),
            Pat::PUInt8 { value, astptr } => self.literal_pat("UInt8", value, astptr),
            Pat::PUInt16 { value, astptr } => self.literal_pat("UInt16", value, astptr),
            Pat::PUInt32 { value, astptr } => self.literal_pat("UInt32", value, astptr),
            Pat::PUInt64 { value, astptr } => self.literal_pat("UInt64", value, astptr),
            Pat::PFloat { value, astptr } => self.literal_pat("Float", value, astptr),
            Pat::PFloat32 { value, astptr } => self.literal_pat("Float32", value, astptr),
            Pat::PFloat64 { value, astptr } => self.literal_pat("Float64", value, astptr),
            Pat::PString { value, astptr } => self.literal_pat("String", value, astptr),
            Pat::PChar { value, astptr } => self.literal_pat("Char", value, astptr),
            Pat::PConstr {
                constructor,
                args,
                astptr,
            } => {
                self.open("Pat.Constr");
                self.ptr(astptr);
                self.path(constructor);
                self.patterns(args);
            }
            Pat::PStruct {
                name,
                fields,
                has_rest,
                astptr,
            } => {
                self.open("Pat.Struct");
                self.ptr(astptr);
                self.path(name);
                self.list(fields.len());
                for (name, pat) in fields {
                    self.open("PatField");
                    self.string(&name.0);
                    self.pat(pat);
                    self.close();
                }
                self.boolean(*has_rest);
            }
            Pat::PTuple { pats, astptr } => {
                self.open("Pat.Tuple");
                self.ptr(astptr);
                self.patterns(pats);
            }
            Pat::PArray {
                prefix,
                rest,
                suffix,
                astptr,
            } => {
                self.open("Pat.Array");
                self.ptr(astptr);
                self.patterns(prefix);
                if let Some(rest) = rest {
                    self.some();
                    self.pat_rest(rest);
                } else {
                    self.none();
                }
                self.patterns(suffix);
            }
            Pat::PAlias { name, pat, astptr } => {
                self.open("Pat.Alias");
                self.ptr(astptr);
                self.string(&name.0);
                self.pat(pat);
            }
            Pat::POr { pats, astptr } => {
                self.open("Pat.Or");
                self.ptr(astptr);
                self.patterns(pats);
            }
            Pat::PRange {
                start,
                end,
                inclusive,
                astptr,
            } => {
                self.open("Pat.Range");
                self.ptr(astptr);
                self.pat(start);
                self.pat(end);
                self.boolean(*inclusive);
            }
            Pat::PWild { astptr } => {
                self.open("Pat.Wild");
                self.ptr(astptr);
            }
        }
        self.close();
    }

    fn diagnostic(&mut self, diagnostic: &parser::Diagnostic) {
        self.open("Diagnostic");
        self.range(diagnostic.range().unwrap_or_default());
        self.string(diagnostic.message());
        self.close();
    }
}

pub fn encode_ast(path: &FsPath, source: &str) -> String {
    let path = path.to_owned();
    let source = source.to_owned();
    std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(move || encode_ast_inner(&path, &source))
        .unwrap()
        .join()
        .unwrap()
}

fn encode_ast_inner(path: &FsPath, source: &str) -> String {
    let parsed = parser::parse(path, source);
    if !parsed.diagnostics.is_empty() {
        let mut encoder = Encoder::new();
        encoder.none();
        encoder.list(parsed.diagnostics.len());
        for diagnostic in &parsed.diagnostics {
            encoder.diagnostic(diagnostic);
        }
        return encoder.output;
    }

    let root = MySyntaxNode::new_root(parsed.green_node);
    let file = cst::cst::File::cast(root).unwrap();
    let lowered = ast::lower::lower(file);
    let (file, diagnostics) = lowered.into_parts();
    let mut encoder = Encoder::new();
    if let Some(file) = &file {
        encoder.some();
        encoder.file(file);
    } else {
        encoder.none();
    }
    encoder.list(diagnostics.len());
    for diagnostic in &diagnostics {
        encoder.diagnostic(diagnostic);
    }
    encoder.output
}
