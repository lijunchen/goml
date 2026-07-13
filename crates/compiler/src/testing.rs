use std::collections::HashSet;
use std::path::Path;

use ::ast::ast::{self, Attribute, Item};
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use text_size::TextRange;

use crate::artifact::{PackageExports, TestDescriptor};
use crate::hir::SourceFileAst;
use crate::package_names::is_special_unqualified_package;
use crate::tast::Ty;

#[derive(Debug, Clone)]
pub struct TestCandidate {
    pub name: String,
    pub source_path: String,
    pub range: TextRange,
    pub ignored: bool,
    pub ignore_reason: Option<String>,
}

struct ParsedAttribute<'a> {
    name: &'a str,
    args: Option<&'a str>,
}

pub fn collect_test_candidates(files: &[SourceFileAst]) -> (Vec<TestCandidate>, Diagnostics) {
    let mut candidates = Vec::new();
    let mut diagnostics = Diagnostics::new();
    for file in files {
        collect_file_tests(file, &mut candidates, &mut diagnostics);
    }
    (candidates, diagnostics)
}

pub fn validate_test_candidates(
    package: &str,
    candidates: Vec<TestCandidate>,
    exports: &PackageExports,
    diagnostics: &mut Diagnostics,
) -> Vec<TestDescriptor> {
    let mut descriptors = Vec::new();
    let mut ids = HashSet::new();
    for candidate in candidates {
        diagnostics.set_source(&candidate.source_path);
        let symbol = if is_special_unqualified_package(package) {
            candidate.name.clone()
        } else {
            format!("{package}::{}", candidate.name)
        };
        let Some(scheme) = exports.value_env.funcs.get(&symbol) else {
            diagnostics.push(
                test_diagnostic(format!(
                    "test function `{}` was not resolved",
                    candidate.name
                ))
                .with_range(candidate.range),
            );
            continue;
        };
        let mut valid = true;
        if candidate.name == crate::package_names::ENTRY_FUNCTION {
            diagnostics.push(
                test_diagnostic("the main function cannot be a test").with_range(candidate.range),
            );
            valid = false;
        }
        if !scheme.type_params.is_empty() {
            diagnostics.push(
                test_diagnostic(format!(
                    "test function `{}` must not have type parameters",
                    candidate.name
                ))
                .with_range(candidate.range),
            );
            valid = false;
        }
        match &scheme.ty {
            Ty::TFunc { params, ret_ty } => {
                if !params.is_empty() {
                    diagnostics.push(
                        test_diagnostic(format!(
                            "test function `{}` must not have parameters",
                            candidate.name
                        ))
                        .with_range(candidate.range),
                    );
                    valid = false;
                }
                if ret_ty.as_ref() != &Ty::TUnit {
                    diagnostics.push(
                        test_diagnostic(format!(
                            "test function `{}` must return unit, found {}",
                            candidate.name,
                            crate::typer::format_ty_for_diag(ret_ty)
                        ))
                        .with_range(candidate.range),
                    );
                    valid = false;
                }
            }
            other => {
                diagnostics.push(
                    test_diagnostic(format!(
                        "test item `{}` must be a function, found {}",
                        candidate.name,
                        crate::typer::format_ty_for_diag(other)
                    ))
                    .with_range(candidate.range),
                );
                valid = false;
            }
        }
        if !valid {
            continue;
        }
        let id = symbol.clone();
        if !ids.insert(id.clone()) {
            diagnostics.push(
                test_diagnostic(format!("duplicate test id `{id}`")).with_range(candidate.range),
            );
            continue;
        }
        descriptors.push(TestDescriptor {
            id: id.clone(),
            package: package.to_string(),
            symbol,
            display_name: id,
            source_path: candidate.source_path,
            start: u32::from(candidate.range.start()),
            end: u32::from(candidate.range.end()),
            ignored: candidate.ignored,
            ignore_reason: candidate.ignore_reason,
        });
    }
    diagnostics.clear_source();
    descriptors.sort_by(|left, right| left.id.cmp(&right.id));
    descriptors
}

fn collect_file_tests(
    file: &SourceFileAst,
    candidates: &mut Vec<TestCandidate>,
    diagnostics: &mut Diagnostics,
) {
    diagnostics.set_source(&file.path);
    for item in &file.ast.toplevels {
        match item {
            Item::Fn(function) => {
                collect_function_test(&file.path, function, candidates, diagnostics)
            }
            Item::ImplBlock(block) => {
                reject_test_attributes(&block.attrs, "impl block", diagnostics);
                for method in &block.methods {
                    reject_test_attributes(&method.attrs, "impl method", diagnostics);
                }
            }
            Item::ExternFn(function) => {
                reject_test_attributes(&function.attrs, "extern function", diagnostics)
            }
            Item::EnumDef(definition) => {
                reject_test_attributes(&definition.attrs, "enum", diagnostics)
            }
            Item::StructDef(definition) => {
                reject_test_attributes(&definition.attrs, "struct", diagnostics)
            }
            Item::TraitDef(definition) => {
                reject_test_attributes(&definition.attrs, "trait", diagnostics)
            }
        }
    }
}

fn collect_function_test(
    path: &Path,
    function: &ast::Fn,
    candidates: &mut Vec<TestCandidate>,
    diagnostics: &mut Diagnostics,
) {
    let test_attrs = function
        .attrs
        .iter()
        .filter(|attribute| attribute_name(attribute) == Some("test"))
        .collect::<Vec<_>>();
    let ignore_attrs = function
        .attrs
        .iter()
        .filter(|attribute| attribute_name(attribute) == Some("ignore"))
        .collect::<Vec<_>>();

    if test_attrs.len() > 1 {
        for attribute in test_attrs.iter().skip(1) {
            diagnostics.push(
                attribute_diagnostic("duplicate `#[test]` attribute")
                    .with_range(attribute.ast.text_range()),
            );
        }
    }
    if ignore_attrs.len() > 1 {
        for attribute in ignore_attrs.iter().skip(1) {
            diagnostics.push(
                attribute_diagnostic("duplicate `#[ignore]` attribute")
                    .with_range(attribute.ast.text_range()),
            );
        }
    }
    let Some(test_attr) = test_attrs.first() else {
        for attribute in ignore_attrs {
            diagnostics.push(
                attribute_diagnostic("`#[ignore]` requires `#[test]`")
                    .with_range(attribute.ast.text_range()),
            );
        }
        return;
    };
    let Some(parsed_test) = parse_attribute(test_attr) else {
        return;
    };
    if parsed_test.args.is_some() {
        diagnostics.push(
            attribute_diagnostic("`#[test]` does not accept arguments")
                .with_range(test_attr.ast.text_range()),
        );
        return;
    }

    let mut ignore_reason = None;
    if let Some(ignore_attr) = ignore_attrs.first() {
        let Some(parsed_ignore) = parse_attribute(ignore_attr) else {
            return;
        };
        if let Some(args) = parsed_ignore.args {
            match parse_string_argument(args) {
                Some(reason) => ignore_reason = Some(reason),
                None => diagnostics.push(
                    attribute_diagnostic(
                        "`#[ignore]` accepts either no arguments or one string reason",
                    )
                    .with_range(ignore_attr.ast.text_range()),
                ),
            }
        }
    }

    candidates.push(TestCandidate {
        name: function.name.0.clone(),
        source_path: path.display().to_string(),
        range: test_attr.ast.text_range(),
        ignored: !ignore_attrs.is_empty(),
        ignore_reason,
    });
}

fn reject_test_attributes(attrs: &[Attribute], target: &str, diagnostics: &mut Diagnostics) {
    for attribute in attrs {
        match attribute_name(attribute) {
            Some("test") => diagnostics.push(
                attribute_diagnostic(format!(
                    "`#[test]` can only be applied to top-level functions, not {target}"
                ))
                .with_range(attribute.ast.text_range()),
            ),
            Some("ignore") => diagnostics.push(
                attribute_diagnostic("`#[ignore]` requires a top-level `#[test]` function")
                    .with_range(attribute.ast.text_range()),
            ),
            _ => {}
        }
    }
}

fn attribute_name(attribute: &Attribute) -> Option<&str> {
    parse_attribute(attribute).map(|parsed| parsed.name)
}

fn parse_attribute(attribute: &Attribute) -> Option<ParsedAttribute<'_>> {
    let text = attribute.text.trim();
    let inner = text.strip_prefix("#[")?.strip_suffix(']')?.trim();
    if let Some(open) = inner.find('(') {
        let name = inner[..open].trim();
        let args = inner[open + 1..].strip_suffix(')')?.trim();
        return Some(ParsedAttribute {
            name,
            args: Some(args),
        });
    }
    Some(ParsedAttribute {
        name: inner,
        args: None,
    })
}

fn parse_string_argument(args: &str) -> Option<String> {
    let value = args.trim();
    serde_json::from_str(value).ok()
}

fn attribute_diagnostic(message: impl Into<String>) -> Diagnostic {
    Diagnostic::new(Stage::other("attribute"), Severity::Error, message)
}

fn test_diagnostic(message: impl Into<String>) -> Diagnostic {
    Diagnostic::new(Stage::Typer, Severity::Error, message)
}
