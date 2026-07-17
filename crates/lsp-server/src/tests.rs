use std::collections::{BTreeSet, HashMap};
use std::path::{Path, PathBuf};
use std::sync::{Mutex, OnceLock};

use expect_test::{Expect, expect};
use tempfile::tempdir;
use tower_lsp::lsp_types::*;

use crate::{Document, handlers};

fn test_module_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("compiler/src/tests/module")
}

fn env_lock() -> &'static Mutex<()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
}

fn with_goml_home<T>(home: &Path, f: impl FnOnce() -> T) -> T {
    let _guard = env_lock()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    let previous = std::env::var_os("GOML_HOME");
    unsafe {
        std::env::set_var("GOML_HOME", home);
    }
    let result = f();
    match previous {
        Some(value) => unsafe {
            std::env::set_var("GOML_HOME", value);
        },
        None => unsafe {
            std::env::remove_var("GOML_HOME");
        },
    }
    result
}

fn write_cached_registry(home: &Path) {
    let registry = home.join("cache/registry");
    std::fs::create_dir_all(registry.join("alice/http/1.2.0/client")).unwrap();
    std::fs::write(
        registry.join("index.toml"),
        r#"[modules."alice::http"]
latest = "1.2.0"
versions = ["1.2.0"]
"#,
    )
    .unwrap();
    std::fs::write(
        registry.join("alice/http/1.2.0/goml.toml"),
        r#"[module]
path = "alice::http"
"#,
    )
    .unwrap();
    std::fs::write(
        registry.join("alice/http/1.2.0/lib.gom"),
        r#"
package http;

use alice::http::client;

pub fn make_client() -> client::Client {
    client::Client { name: "alice" }
}
"#,
    )
    .unwrap();
    std::fs::write(
        registry.join("alice/http/1.2.0/client/client.gom"),
        r#"
package client;

pub struct Client {
    name: string,
}

pub fn tag() -> string {
    "client"
}
"#,
    )
    .unwrap();
}

fn write_minimal_project(root: &Path, src: &str) -> PathBuf {
    std::fs::write(
        root.join("goml.toml"),
        r#"[module]
path = "demo"
"#,
    )
    .unwrap();
    let main_path = root.join("main.gom");
    std::fs::write(&main_path, src).unwrap();
    main_path
}

mod project_test_support_tests {
    use super::*;

    #[test]
    fn lsp_distinguishes_normal_internal_and_external_analysis() {
        let dir = tempdir().unwrap();
        let root = dir.path();
        std::fs::write(root.join("goml.toml"), "[module]\npath = \"demo\"\n").unwrap();
        std::fs::create_dir_all(root.join("math/tests/api")).unwrap();
        let math_path = root.join("math/math.gom");
        let sibling_path = root.join("math/sibling.gom");
        let white_path = root.join("math/math_test.gom");
        let helper_path = root.join("math/helper_test.gom");
        let external_path = root.join("math/tests/api/api_test.gom");
        let math_src = r#"package math;

fn private_value() -> int32 {
    41
}

pub fn public_value() -> int32 {
    sibling_value()
}
"#;
        let sibling_src = r#"package math;

fn sibling_value() -> int32 {
    42
}
"#;
        let white_src = r#"package math;

#[test]
fn white_box() -> unit {
    let value: string = test_helper();
    let _ = private_value();
    let _ = value;
    ()
}
"#;
        let helper_src = r#"package math;

fn test_helper() -> int32 {
    1
}
"#;
        let helper_override = r#"package math;

fn test_helper() -> string {
    "ok"
}
"#;
        let external_src = r#"package api;

use demo::math;

#[test]
fn black_box() -> unit {
    let _ = math::public_value();
    ()
}
"#;
        std::fs::write(&math_path, math_src).unwrap();
        std::fs::write(&sibling_path, sibling_src).unwrap();
        std::fs::write(&white_path, white_src).unwrap();
        std::fs::write(&helper_path, helper_src).unwrap();
        std::fs::write(&external_path, external_src).unwrap();

        let math_doc = Document::new(math_src.to_string());
        assert!(handlers::get_diagnostics(&math_path, math_src, &math_doc).is_empty());
        let hover = handlers::hover(
            &math_path,
            math_src,
            Position {
                line: 7,
                character: 6,
            },
        );
        assert!(format_hover(hover).contains("int32"));

        let white_doc = Document::new(white_src.to_string());
        assert!(!handlers::get_diagnostics(&white_path, white_src, &white_doc).is_empty());
        let overrides = HashMap::from([(helper_path.clone(), helper_override.to_string())]);
        assert!(
            handlers::get_diagnostics_with_overrides(
                &white_path,
                white_src,
                &white_doc,
                &overrides,
            )
            .is_empty()
        );
        let white_uri = Url::from_file_path(&white_path).unwrap();
        let helper_line = white_src
            .lines()
            .position(|line| line.contains("test_helper"))
            .unwrap() as u32;
        let helper_character = white_src
            .lines()
            .nth(helper_line as usize)
            .unwrap()
            .find("test_helper")
            .unwrap() as u32;
        let definition = handlers::goto_definition_with_overrides(
            &white_uri,
            &white_path,
            white_src,
            Position {
                line: helper_line,
                character: helper_character,
            },
            &white_doc,
            &overrides,
        );
        let Some(GotoDefinitionResponse::Scalar(definition)) = definition else {
            panic!("expected one helper definition");
        };
        assert_eq!(definition.uri, Url::from_file_path(&helper_path).unwrap());

        let completion_src = white_src.replace("test_helper()", "test_h");
        let completion_line = completion_src
            .lines()
            .position(|line| line.contains("test_h"))
            .unwrap() as u32;
        let completion_character = completion_src
            .lines()
            .nth(completion_line as usize)
            .unwrap()
            .find("test_h")
            .unwrap() as u32
            + "test_h".len() as u32;
        let completion = handlers::completion_with_overrides(
            &white_path,
            &completion_src,
            Position {
                line: completion_line,
                character: completion_character,
            },
            &overrides,
        );
        let completion = format_completion(completion);
        assert!(completion.contains("test_helper"), "{completion}");

        let invalid_helper = r#"package math;

fn test_helper() -> string {
    missing_value()
}
"#;
        let invalid_overrides = HashMap::from([(helper_path.clone(), invalid_helper.to_string())]);
        assert!(
            handlers::get_diagnostics_with_overrides(
                &white_path,
                white_src,
                &white_doc,
                &invalid_overrides,
            )
            .is_empty()
        );
        let helper_doc = Document::new(invalid_helper.to_string());
        let helper_diagnostics = handlers::get_diagnostics_with_overrides(
            &helper_path,
            invalid_helper,
            &helper_doc,
            &invalid_overrides,
        );
        assert!(
            helper_diagnostics
                .iter()
                .any(|diagnostic| diagnostic.message.contains("missing_value"))
        );

        let external_doc = Document::new(external_src.to_string());
        assert!(handlers::get_diagnostics(&external_path, external_src, &external_doc).is_empty());
        let private_external = external_src.replace("public_value", "private_value");
        let private_doc = Document::new(private_external.clone());
        let diagnostics =
            handlers::get_diagnostics(&external_path, &private_external, &private_doc);
        assert!(
            diagnostics
                .iter()
                .any(|diagnostic| diagnostic.message.contains("private_value"))
        );
    }

    #[test]
    fn test_files_expose_run_test_code_lenses() {
        let dir = tempdir().unwrap();
        let internal_path = dir.path().join("value_test.gom");
        let src = "package value;\n#[test]\nfn works() -> unit { () }\n";
        let doc = Document::new(src.to_string());
        let uri = Url::from_file_path(&internal_path).unwrap();
        let lenses = handlers::code_lenses(&uri, &internal_path, src, &doc);
        assert_eq!(lenses.len(), 1);
        let command = lenses[0].command.as_ref().unwrap();
        assert_eq!(command.command, "goml.runTest");
        assert_eq!(command.arguments.as_ref().unwrap()[1], "works");
        assert_eq!(command.arguments.as_ref().unwrap()[2], "internal");

        let external_path = dir.path().join("tests/api/api_test.gom");
        let external_uri = Url::from_file_path(&external_path).unwrap();
        let lenses = handlers::code_lenses(&external_uri, &external_path, src, &doc);
        assert_eq!(
            lenses[0]
                .command
                .as_ref()
                .unwrap()
                .arguments
                .as_ref()
                .unwrap()[2],
            "external"
        );
    }
}

mod robustness_tests {
    use super::*;

    #[test]
    fn lsp_handles_sampled_prefixes_of_hm_typechecker_without_panicking() {
        let input = include_str!("../../compiler/src/tests/pipeline/080_hm_typechecker/main.gom");
        assert_lsp_handles_sampled_prefixes_without_panicking_with_stack(
            "pipeline_080_hm_typechecker",
            input,
            16 * 1024 * 1024,
        );
    }

    #[test]
    fn lsp_handles_tricky_inputs_without_panicking() {
        let cases = [
            (
                "unterminated_string_and_block",
                "\nfn main() {\n  let s = \"hello\n  let x = 1;\n",
            ),
            (
                "unterminated_char_and_comment",
                "\nfn main() {\n  let c = '\\u12\n  // trailing",
            ),
            (
                "dense_operators_and_partial_tokens",
                "\nfn main() { let x = 1<<<<=>>>==!=&&||::..,,;; }\n",
            ),
            (
                "nested_brackets_missing_closers",
                "\nfn main() { let _ = ((([1, 2, 3]); }\n",
            ),
            (
                "attribute_generics_and_dyn_partial",
                "#[derive(ToString)]\nfn f[T: Eq + Hash +](x: T) -> dyn Show {\n",
            ),
            (
                "invalid_tokens_and_escape_like_sequence",
                "\nfn main() { let y = \\u2028; @@@ }\n",
            ),
            (
                "deeply_nested_expressions",
                "\nfn main() { let _ = (((((((((((((1 + 2))))))))))))); }\n",
            ),
        ];

        for (case_name, input) in cases {
            assert_lsp_handles_sampled_prefixes_without_panicking(case_name, input);
        }
    }
}

fn pipeline_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("compiler/src/tests/pipeline")
}

fn format_diagnostics(diags: &[Diagnostic]) -> String {
    if diags.is_empty() {
        return "no diagnostics".to_string();
    }
    diags
        .iter()
        .map(|d| {
            let severity = match d.severity {
                Some(DiagnosticSeverity::ERROR) => "error",
                Some(DiagnosticSeverity::WARNING) => "warning",
                Some(DiagnosticSeverity::INFORMATION) => "info",
                Some(DiagnosticSeverity::HINT) => "hint",
                _ => "unknown",
            };
            format!(
                "[{}:{}] {}: {}",
                d.range.start.line, d.range.start.character, severity, d.message
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn format_hover(hover: Option<Hover>) -> String {
    match hover {
        None => "no hover".to_string(),
        Some(h) => match h.contents {
            HoverContents::Markup(m) => m.value,
            HoverContents::Scalar(MarkedString::String(s)) => s,
            HoverContents::Scalar(MarkedString::LanguageString(ls)) => {
                format!("```{}\n{}\n```", ls.language, ls.value)
            }
            HoverContents::Array(arr) => arr
                .into_iter()
                .map(|ms| match ms {
                    MarkedString::String(s) => s,
                    MarkedString::LanguageString(ls) => {
                        format!("```{}\n{}\n```", ls.language, ls.value)
                    }
                })
                .collect::<Vec<_>>()
                .join("\n"),
        },
    }
}

fn format_completion(completion: Option<CompletionResponse>) -> String {
    match completion {
        None => "no completion".to_string(),
        Some(CompletionResponse::Array(items)) => {
            if items.is_empty() {
                return "empty completion".to_string();
            }
            let mut labels: Vec<_> = items.iter().map(|i| i.label.as_str()).collect();
            labels.sort();
            labels.join(", ")
        }
        Some(CompletionResponse::List(list)) => {
            if list.items.is_empty() {
                return "empty completion".to_string();
            }
            let mut labels: Vec<_> = list.items.iter().map(|i| i.label.as_str()).collect();
            labels.sort();
            labels.join(", ")
        }
    }
}

fn format_signature_help(signature_help: Option<SignatureHelp>) -> String {
    let Some(signature_help) = signature_help else {
        return "no signature".to_string();
    };
    let Some(signature) = signature_help.signatures.first() else {
        return "empty signature".to_string();
    };

    let active_parameter = signature_help.active_parameter.unwrap_or(0);
    let parameters = signature
        .parameters
        .as_ref()
        .map(|parameters| {
            parameters
                .iter()
                .map(|parameter| match &parameter.label {
                    ParameterLabel::Simple(label) => label.clone(),
                    ParameterLabel::LabelOffsets([start, end]) => signature
                        .label
                        .chars()
                        .skip(*start as usize)
                        .take((*end - *start) as usize)
                        .collect::<String>(),
                })
                .collect::<Vec<_>>()
                .join(", ")
        })
        .unwrap_or_default();

    format!(
        "label: {}\nactive_parameter: {}\nparameters: {}",
        signature.label, active_parameter, parameters
    )
}

fn format_inlay_hints(hints: Option<Vec<InlayHint>>) -> String {
    let Some(hints) = hints else {
        return "no hints".to_string();
    };
    if hints.is_empty() {
        return "empty hints".to_string();
    }

    hints
        .into_iter()
        .map(|hint| {
            let label = match hint.label {
                InlayHintLabel::String(text) => text,
                InlayHintLabel::LabelParts(parts) => parts
                    .into_iter()
                    .map(|part| part.value)
                    .collect::<Vec<_>>()
                    .join(""),
            };
            let kind = match hint.kind {
                Some(tower_lsp::lsp_types::InlayHintKind::TYPE) => "type",
                Some(tower_lsp::lsp_types::InlayHintKind::PARAMETER) => "parameter",
                _ => "unknown",
            };
            format!(
                "{}:{} {} {}",
                hint.position.line, hint.position.character, kind, label
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn check_diagnostics(src: &str, expect: Expect) {
    let path = PathBuf::from("test.gom");
    let doc = Document::new(src.to_string());
    let diags = handlers::get_diagnostics(&path, src, &doc);
    expect.assert_eq(&format_diagnostics(&diags));
}

fn check_hover(src: &str, line: u32, character: u32, expect: Expect) {
    let path = PathBuf::from("test.gom");
    let position = Position { line, character };
    let hover = handlers::hover(&path, src, position);
    expect.assert_eq(&format_hover(hover));
}

fn check_completion(src: &str, line: u32, character: u32, expect: Expect) {
    let path = PathBuf::from("test.gom");
    let position = Position { line, character };
    let completion = handlers::completion(&path, src, position);
    expect.assert_eq(&format_completion(completion));
}

fn check_signature_help(src: &str, line: u32, character: u32, expect: Expect) {
    let path = PathBuf::from("test.gom");
    let position = Position { line, character };
    let signature_help = handlers::signature_help(&path, src, position);
    expect.assert_eq(&format_signature_help(signature_help));
}

fn check_inlay_hints(src: &str, range: Range, expect: Expect) {
    let path = PathBuf::from("test.gom");
    let doc = Document::new(src.to_string());
    let hints = handlers::inlay_hints(&path, src, range, &doc);
    expect.assert_eq(&format_inlay_hints(hints));
}

fn check_module_diagnostics(project_name: &str, expect: Expect) {
    let project_dir = test_module_dir().join(project_name);
    let main_path = project_dir.join("main.gom");
    let src = std::fs::read_to_string(&main_path).unwrap();
    let doc = Document::new(src.clone());
    let diags = handlers::get_diagnostics(&main_path, &src, &doc);
    expect.assert_eq(&format_diagnostics(&diags));
}

fn check_module_file_diagnostics(project_name: &str, rel_file: &str, expect: Expect) {
    let project_dir = test_module_dir().join(project_name);
    let path = project_dir.join(rel_file);
    let src = std::fs::read_to_string(&path).unwrap();
    let doc = Document::new(src.clone());
    let diags = handlers::get_diagnostics(&path, &src, &doc);
    expect.assert_eq(&format_diagnostics(&diags));
}

fn check_pipeline_diagnostics(case_name: &str, expect: Expect) {
    let case_dir = pipeline_dir().join(case_name);
    let main_path = case_dir.join("main.gom");
    if !main_path.exists() {
        expect.assert_eq("case not found");
        return;
    }
    let src = std::fs::read_to_string(&main_path).unwrap();
    let doc = Document::new(src.clone());
    let diags = handlers::get_diagnostics(&main_path, &src, &doc);
    expect.assert_eq(&format_diagnostics(&diags));
}

fn utf8_prefixes(input: &str) -> impl Iterator<Item = usize> + '_ {
    std::iter::once(0)
        .chain(input.char_indices().map(|(idx, _)| idx).skip(1))
        .chain(std::iter::once(input.len()))
}

fn sampled_prefixes(input: &str, max_points: usize) -> Vec<usize> {
    let boundaries: Vec<usize> = utf8_prefixes(input).collect();
    if boundaries.len() <= max_points {
        return boundaries;
    }

    let len = input.len();
    let mut points = BTreeSet::new();
    points.insert(0);
    points.insert(len);

    for point in boundaries.iter().take(32) {
        points.insert(*point);
    }
    for point in boundaries.iter().rev().take(32) {
        points.insert(*point);
    }

    let dense_slots = 64usize;
    for i in 0..dense_slots {
        let idx = i * (boundaries.len() - 1) / (dense_slots - 1);
        points.insert(boundaries[idx]);
    }

    for (idx, ch) in input.char_indices() {
        if matches!(
            ch,
            '\n' | '{'
                | '}'
                | '('
                | ')'
                | '['
                | ']'
                | ';'
                | ','
                | ':'
                | '.'
                | '#'
                | '|'
                | '&'
                | '+'
                | '-'
                | '*'
                | '/'
                | '<'
                | '>'
                | '='
                | '!'
                | '"'
                | '\''
        ) {
            points.insert(idx);
            points.insert((idx + ch.len_utf8()).min(len));
            if idx > 0 {
                let prev = input[..idx]
                    .char_indices()
                    .last()
                    .map(|(pos, _)| pos)
                    .unwrap_or(0);
                points.insert(prev);
            }
        }
    }

    let mut collected: Vec<usize> = points
        .into_iter()
        .filter(|point| input.is_char_boundary(*point))
        .collect();
    collected.sort_unstable();
    collected.dedup();

    if collected.len() <= max_points {
        return collected;
    }

    let mut reduced = BTreeSet::new();
    reduced.insert(0);
    reduced.insert(len);
    for i in 0..max_points {
        let idx = i * (collected.len() - 1) / (max_points - 1);
        reduced.insert(collected[idx]);
    }
    reduced.into_iter().collect()
}

fn panic_payload_message(payload: &(dyn std::any::Any + Send)) -> String {
    if let Some(message) = payload.downcast_ref::<String>() {
        return message.clone();
    }
    if let Some(message) = payload.downcast_ref::<&str>() {
        return message.to_string();
    }
    "non-string panic payload".to_string()
}

fn end_position(doc: &Document, src: &str) -> Position {
    let Ok(offset) = u32::try_from(src.len()) else {
        return Position {
            line: 0,
            character: 0,
        };
    };
    doc.position(text_size::TextSize::from(offset))
        .unwrap_or(Position {
            line: 0,
            character: 0,
        })
}

fn assert_lsp_handles_sampled_prefixes_without_panicking(case_name: &str, input: &str) {
    let path = std::env::temp_dir().join(format!("{case_name}.gom"));
    let uri = Url::from_file_path(&path).ok().unwrap_or_else(|| {
        Url::parse("file:///tmp/goml_lsp_robustness.gom")
            .unwrap_or_else(|err| panic!("failed to create fallback file uri: {err}"))
    });
    let start = Position {
        line: 0,
        character: 0,
    };

    let prefixes = sampled_prefixes(input, 128);
    for (idx, end) in prefixes.iter().copied().enumerate() {
        let prefix = &input[..end];
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let doc = Document::new(prefix.to_string());
            let at_end = end_position(&doc, prefix);
            let _ = handlers::get_diagnostics(&path, prefix, &doc);
            if idx % 8 == 0 || end == input.len() {
                let _ = handlers::hover(&path, prefix, start);
                let _ = handlers::hover(&path, prefix, at_end);
                let _ = handlers::completion(&path, prefix, start);
                let _ = handlers::completion(&path, prefix, at_end);
                let _ = handlers::goto_definition(&uri, &path, prefix, start, &doc);
                let _ = handlers::goto_definition(&uri, &path, prefix, at_end, &doc);
            }
        }));
        if let Err(payload) = result {
            let panic_message = panic_payload_message(payload.as_ref());
            let tail = if prefix.len() > 160 {
                &prefix[prefix.len() - 160..]
            } else {
                prefix
            };
            panic!(
                "lsp panicked for case={case_name}, prefix_end={end}, panic={panic_message}, prefix_tail={tail:?}"
            );
        }
    }
}

fn assert_lsp_handles_sampled_prefixes_without_panicking_with_stack(
    case_name: &str,
    input: &str,
    stack_size: usize,
) {
    let case_name = case_name.to_string();
    let input = input.to_string();
    let handle = std::thread::Builder::new()
        .stack_size(stack_size)
        .spawn(move || {
            assert_lsp_handles_sampled_prefixes_without_panicking(&case_name, &input);
        })
        .unwrap();
    if let Err(payload) = handle.join() {
        std::panic::resume_unwind(payload);
    }
}

mod diagnostics_tests {
    use super::*;

    #[test]
    fn valid_code_no_diagnostics() {
        check_diagnostics(
            r#"


fn main() {
    let x = 42;
    println(x.to_string());
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn undefined_variable_error() {
        check_diagnostics(
            r#"


fn main() {
    println(undefined_var.to_string());
}
"#,
            expect![[r#"
                [4:12] error: Unresolved name undefined_var
                [4:4] error: Could not infer the type required to prove ToString<unknown>
                [4:12] error: Could not infer the receiver type for method to_string"#]],
        );
    }

    #[test]
    fn type_mismatch_error() {
        check_diagnostics(
            r#"


fn add(x: int32, y: int32) -> int32 {
    x + y
}

fn main() {
    let result = add("hello", 42);
}
"#,
            expect!["[8:21] error: Type mismatch: expected string, found int32"],
        );
    }

    #[test]
    fn pattern_constructor_wrong_arity_reports_pattern_location() {
        check_diagnostics(
            r#"


enum Maybe {
    Some(int32),
    None,
}

fn main() -> int32 {
    let x: Maybe = Some(1);
    match x {
        Some(a, b) => 1,
        None => 0,
    }
}
"#,
            expect![[r#"
                [11:8] error: Constructor Some expects 1 arguments, but got 2"#]],
        );
    }

    #[test]
    fn parse_error() {
        check_diagnostics(
            r#"


fn main( {
    let x = 42;
}
"#,
            expect![[r#"
                [3:9] error: expect ")", actual "{"
                [4:4] error: expected a function
                [4:10] error: expected a function
                [4:14] error: expected a function
                [5:0] error: expected a function"#]],
        );
    }

    #[test]
    fn missing_return_type() {
        check_diagnostics(
            r#"


fn add(x: int32, y: int32) {
    x + y
}

fn main() {
    let _ = add(1, 2);
}
"#,
            expect!["[4:4] error: Type mismatch: expected int32, found unit"],
        );
    }

    #[test]
    fn module_project001_no_errors() {
        check_module_diagnostics("project001", expect!["no diagnostics"]);
    }

    #[test]
    fn module_project002_no_errors() {
        check_module_diagnostics("project002", expect!["no diagnostics"]);
    }

    #[test]
    fn module_project003_no_errors() {
        check_module_diagnostics("project003", expect!["no diagnostics"]);
    }

    #[test]
    fn module_project011_math_package_no_missing_dir_errors() {
        check_module_file_diagnostics(
            "project011_complex_dependency_graph",
            "math/math.gom",
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn std_io_usage_no_diagnostics() {
        let dir = tempdir().unwrap();
        let src = r#"
package main;

use std::io;

fn main() -> unit {
    io::println("ok")
}
"#;
        let path = write_minimal_project(dir.path(), src);
        let doc = Document::new(src.to_string());
        let diags = handlers::get_diagnostics(&path, src, &doc);
        expect!["no diagnostics"].assert_eq(&format_diagnostics(&diags));
    }

    #[test]
    fn module_project011_pipeline_package_no_missing_dir_errors() {
        check_module_file_diagnostics(
            "project011_complex_dependency_graph",
            "pipeline/pipeline.gom",
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn missing_package_reports_high_level_diagnostic() {
        let dir = tempdir().unwrap();
        let root = dir.path();
        std::fs::write(
            root.join("goml.toml"),
            r#"[module]
path = "demo"
"#,
        )
        .unwrap();
        let src = r#"package main;

use demo::colors;
use colors::Paint;

fn main() -> unit {
    ()
}
"#;
        let path = root.join("main.gom");
        std::fs::write(&path, src).unwrap();
        let doc = Document::new(src.to_string());
        let diagnostics = handlers::get_diagnostics(&path, src, &doc);
        let formatted = format_diagnostics(&diagnostics);

        assert!(formatted.contains("package demo::colors not found at"));
        assert!(!formatted.contains("failed to read package directory"));
    }

    #[test]
    fn diagnostics_use_utf16_columns_after_non_ascii_text() {
        let dir = tempdir().unwrap();
        let src = r#"package main;

fn main() -> unit {
    let marker = "🙂"; let _ = 7.5f64 % 2.0f64;
}
"#;
        let path = write_minimal_project(dir.path(), src);
        let doc = Document::new(src.to_string());
        let diagnostics = handlers::get_diagnostics(&path, src, &doc);
        let diagnostic = diagnostics
            .iter()
            .find(|diagnostic| diagnostic.message.contains("Operator %"))
            .unwrap();
        let line = src.lines().nth(3).unwrap();
        let byte_column = line.find("7.5f64").unwrap();
        let utf16_column = line[..byte_column].encode_utf16().count();

        assert_eq!(diagnostic.range.start.line, 3);
        assert_eq!(diagnostic.range.start.character, utf16_column as u32);
        assert_ne!(diagnostic.range.start.character, byte_column as u32);
    }

    #[test]
    fn rich_diagnostics_include_related_locations_and_quick_fixes() {
        let dir = tempdir().unwrap();
        let main_path = dir.path().join("main.gom");
        let dependency_path = dir.path().join("dependency.gom");
        let main_src = "let 名 = value;\n";
        let dependency_src = "let old = 1;\n";
        let mut sources = diagnostics::SourceMap::new();
        let main_source = sources.add(&main_path, main_src);
        let dependency_source = sources.add(&dependency_path, dependency_src);
        let value_start = main_src.find("value").unwrap();
        let old_start = dependency_src.find("old").unwrap();
        let value_span = sources
            .span(main_source, value_start, value_start + "value".len())
            .unwrap();
        let old_span = sources
            .span(dependency_source, old_start, old_start + "old".len())
            .unwrap();
        let diagnostic = diagnostics::Diagnostic::new(
            diagnostics::Stage::Typer,
            diagnostics::Severity::Error,
            "duplicate definition",
        )
        .with_primary_label(value_span, "new definition")
        .with_secondary_label(old_span, "first definition")
        .with_note("names must be unique")
        .with_help("choose another name")
        .with_fix(
            diagnostics::FixIt::new(value_span, "renamed")
                .with_message("Rename value")
                .with_applicability(diagnostics::FixApplicability::MachineApplicable),
        );
        let doc = Document::new(main_src.to_string());
        let diagnostic =
            handlers::diagnostic_to_lsp(&main_path, &doc, &sources, &diagnostic).unwrap();

        assert_eq!(diagnostic.range.start.character, 8);
        assert_eq!(diagnostic.range.end.character, 13);
        assert_eq!(
            diagnostic.message,
            "duplicate definition\nnew definition\nnote: names must be unique\nhelp: choose another name"
        );
        let related = diagnostic.related_information.as_ref().unwrap();
        assert_eq!(related.len(), 1);
        assert_eq!(related[0].message, "first definition");
        assert_eq!(
            related[0].location.uri,
            Url::from_file_path(&dependency_path).unwrap()
        );
        assert_eq!(related[0].location.range.start.character, 4);

        let context = CodeActionContext {
            diagnostics: vec![diagnostic],
            ..Default::default()
        };
        let actions = handlers::code_actions(&context).unwrap();
        let CodeActionOrCommand::CodeAction(action) = &actions[0] else {
            panic!("expected a code action")
        };
        assert_eq!(action.title, "Rename value");
        assert_eq!(action.kind, Some(CodeActionKind::QUICKFIX));
        assert_eq!(action.is_preferred, Some(true));
        let changes = action.edit.as_ref().unwrap().changes.as_ref().unwrap();
        let edits = changes
            .get(&Url::from_file_path(&main_path).unwrap())
            .unwrap();
        assert_eq!(edits[0].new_text, "renamed");
        assert_eq!(edits[0].range.start.character, 8);
    }

    #[test]
    fn diagnostics_with_a_primary_label_in_another_file_are_not_reanchored() {
        let dir = tempdir().unwrap();
        let main_path = dir.path().join("main.gom");
        let dependency_path = dir.path().join("dependency.gom");
        let mut sources = diagnostics::SourceMap::new();
        sources.add(&main_path, "fn main() -> unit { () }\n");
        let dependency = sources.add(&dependency_path, "bad\n");
        let span = sources.span(dependency, 0, 3).unwrap();
        let diagnostic = diagnostics::Diagnostic::new(
            diagnostics::Stage::Typer,
            diagnostics::Severity::Error,
            "dependency error",
        )
        .with_primary_label(span, "bad dependency");
        let doc = Document::new("fn main() -> unit { () }\n".to_string());

        assert!(handlers::diagnostic_to_lsp(&main_path, &doc, &sources, &diagnostic).is_none());
    }
}

mod hover_tests {
    use super::*;

    #[test]
    fn hover_on_variable() {
        check_hover(
            r#"


fn main() {
    let x = 42;
    println(x.to_string());
}
"#,
            5,
            12,
            expect![[r#"
                ```goml
                int32
                ```"#]],
        );
    }

    #[test]
    fn hover_on_function_name() {
        check_hover(
            r#"


fn add(x: int32, y: int32) -> int32 {
    x + y
}

fn main() {
    let result = add(1, 2);
}
"#,
            3,
            5,
            expect![[r#"
                ```goml
                (int32, int32) -> int32
                ```"#]],
        );
    }

    #[test]
    fn hover_on_minimal_function_name() {
        check_hover(
            r#"


fn main() {
}
"#,
            3,
            3,
            expect![[r#"
                ```goml
                () -> unit
                ```"#]],
        );
    }

    #[test]
    fn hover_on_function_call() {
        check_hover(
            r#"


fn add(x: int32, y: int32) -> int32 {
    x + y
}

fn main() {
    let result = add(1, 2);
}
"#,
            8,
            18,
            expect![[r#"
                ```goml
                (int32, int32) -> int32
                ```"#]],
        );
    }

    #[test]
    fn hover_on_struct_field() {
        check_hover(
            r#"


struct Point {
    x: int32,
    y: int32,
}

fn main() {
    let p = Point { x: 10, y: 20 };
    println(p.x.to_string());
}
"#,
            10,
            14,
            expect![[r#"
                ```goml
                int32
                ```"#]],
        );
    }

    #[test]
    fn hover_on_enum_variant() {
        check_hover(
            r#"


enum Color {
    Red,
    Green,
    Blue,
}

fn main() {
    let c = Color::Red;
}
"#,
            10,
            18,
            expect![[r#"
                ```goml
                Color
                ```"#]],
        );
    }

    #[test]
    fn hover_on_parameter() {
        check_hover(
            r#"


fn double(n: int32) -> int32 {
    n * 2
}

fn main() {
    let _ = double(5);
}
"#,
            4,
            4,
            expect![[r#"
                ```goml
                int32
                ```"#]],
        );
    }

    #[test]
    fn hover_on_let_binding() {
        check_hover(
            r#"


fn main() {
    let result: int32 = 42;
    println(result.to_string());
}
"#,
            4,
            8,
            expect![[r#"
                ```goml
                int32
                ```"#]],
        );
    }

    #[test]
    fn hover_on_match_arm_binding() {
        check_hover(
            r#"


enum Option {
    Some(int32),
    None,
}

fn main() {
    let opt = Option::Some(42);
    match opt {
        Option::Some(value) => println(value.to_string()),
        Option::None => println("none"),
    };
}
"#,
            11,
            22,
            expect![[r#"
                ```goml
                int32
                ```"#]],
        );
    }
}

mod completion_tests {
    use super::*;

    #[test]
    fn dot_completion_on_struct() {
        check_completion(
            r#"


struct Point {
    x: int32,
    y: int32,
}

fn main() {
    let p = Point { x: 10, y: 20 };
    p.
}
"#,
            10,
            6,
            expect!["x, y"],
        );
    }

    #[test]
    fn dot_completion_on_int() {
        check_completion(
            r#"


fn main() {
    let x = 42;
    x.
}
"#,
            5,
            6,
            expect!["eq, hash, to_string"],
        );
    }

    #[test]
    fn colon_colon_completion_on_enum() {
        check_completion(
            r#"


enum Color {
    Red,
    Green,
    Blue,
}

fn main() {
    let c = Color::
}
"#,
            10,
            19,
            expect!["Blue, Green, Red"],
        );
    }

    #[test]
    fn dot_completion_on_builtin_vec() {
        check_completion(
            r#"


fn main() {
    let v: Vec[int32] = Vec::new();
    v.
}
"#,
            5,
            6,
            expect![
                "capacity, clear, extend, get, insert, into_iter, is_empty, iter, last, len, new, pop, push, pushed, remove, reserve, reverse, set, slice, swap, swap_remove, truncate, with_capacity"
            ],
        );
    }

    #[test]
    fn dot_completion_on_builtin_hashmap() {
        check_completion(
            r#"


fn main() {
    let m: HashMap[string, int32] = HashMap::new();
    m.
}
"#,
            5,
            6,
            expect!["contains, entries, get, len, new, remove, set"],
        );
    }

    #[test]
    fn colon_colon_completion_on_builtin_vec() {
        check_completion(
            r#"


fn main() {
    let _ = Vec::
}
"#,
            4,
            17,
            expect![
                "capacity, clear, extend, get, insert, is_empty, iter, last, len, new, pop, push, pushed, remove, reserve, reverse, set, slice, swap, swap_remove, truncate, with_capacity"
            ],
        );
    }

    #[test]
    fn colon_colon_completion_on_builtin_hashmap() {
        check_completion(
            r#"


fn main() {
    let _ = HashMap::
}
"#,
            4,
            21,
            expect!["contains, entries, get, len, new, remove, set"],
        );
    }

    #[test]
    fn value_completion_suggests_functions() {
        check_completion(
            r#"


fn helper() -> int32 {
    42
}

fn main() {
    let x = hel
}
"#,
            8,
            15,
            expect!["helper"],
        );
    }

    #[test]
    fn value_completion_suggests_locals() {
        let src = r#"


fn main() {
    let count = 1;
    cou
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 5,
            character: 7,
        };
        let completion = handlers::completion(&path, src, position);
        let Some(CompletionResponse::Array(items)) = completion else {
            panic!("expected completion items");
        };
        let Some(item) = items.into_iter().find(|item| item.label == "count") else {
            panic!("expected count completion item");
        };
        assert_eq!(item.kind, Some(CompletionItemKind::VARIABLE));
        assert_eq!(item.detail.as_deref(), Some("int32"));
    }

    #[test]
    fn call_argument_completion_is_empty_without_prefix() {
        let src = r#"


fn takes(count: int32, label: string) -> unit {
    ()
}

fn main() {
    let count = 1;
    let label = "ok";
    let _ = takes(count, );
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 10,
            character: 25,
        };
        let completion = handlers::completion(&path, src, position);
        let Some(CompletionResponse::Array(items)) = completion else {
            panic!("expected completion items");
        };
        assert!(items.is_empty());
    }

    #[test]
    fn call_argument_completion_prefers_matching_locals_with_prefix() {
        let src = r#"


fn takes(count: int32, label: string) -> unit {
    ()
}

fn main() {
    let count = 1;
    let label = "ok";
    let _ = takes(count, la);
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 10,
            character: 27,
        };
        let completion = handlers::completion(&path, src, position);
        let Some(CompletionResponse::Array(items)) = completion else {
            panic!("expected completion items");
        };
        let first = items
            .first()
            .expect("expected at least one completion item");
        assert_eq!(first.label, "label");
        assert_eq!(first.kind, Some(CompletionItemKind::VARIABLE));
        assert_eq!(first.detail.as_deref(), Some("string"));
    }

    #[test]
    fn value_completion_suggests_imported_package_names() {
        let dir = tempdir().unwrap();
        let root = dir.path();

        std::fs::write(
            root.join("goml.toml"),
            r#"[module]
path = "demo"
"#,
        )
        .unwrap();
        std::fs::create_dir_all(root.join("util")).unwrap();
        std::fs::write(
            root.join("util/util.gom"),
            r#"
package util;

pub fn ping() -> string {
    "pong"
}
"#,
        )
        .unwrap();

        let src = r#"package main;

use demo::util;

fn main() {
    ut
}
"#;
        let path = root.join("main.gom");
        std::fs::write(&path, src).unwrap();

        let completion = handlers::completion(
            &path,
            src,
            Position {
                line: 5,
                character: 6,
            },
        );
        expect!["util"].assert_eq(&format_completion(completion));
    }

    #[test]
    fn use_completion_ignores_configured_target_directory() {
        let dir = tempdir().unwrap();
        let root = dir.path();
        std::fs::write(
            root.join("goml.toml"),
            "[module]\npath = \"demo\"\n\n[build]\ntarget-dir = \"out\"\n",
        )
        .unwrap();
        std::fs::create_dir_all(root.join("util")).unwrap();
        std::fs::create_dir_all(root.join("out")).unwrap();
        std::fs::write(root.join("util/util.gom"), "package util;\n").unwrap();
        std::fs::write(root.join("out/generated.gom"), "package generated;\n").unwrap();
        let src = "package main;\n\nuse \n\nfn main() -> unit { () }\n";
        let path = root.join("main.gom");
        std::fs::write(&path, src).unwrap();

        let Some(CompletionResponse::Array(items)) = handlers::completion(
            &path,
            src,
            Position {
                line: 2,
                character: 4,
            },
        ) else {
            panic!("expected completion items");
        };
        assert!(items.iter().any(|item| item.label == "demo::util"));
        assert!(!items.iter().any(|item| item.label == "demo::out"));
    }

    #[test]
    fn use_completion_suggests_std() {
        let dir = tempdir().unwrap();
        let src = r#"package main;

use st

fn main() -> unit {
    ()
}
"#;
        let path = write_minimal_project(dir.path(), src);
        let completion = handlers::completion(
            &path,
            src,
            Position {
                line: 2,
                character: 6,
            },
        );
        expect!["std"].assert_eq(&format_completion(completion));
    }

    #[test]
    fn use_completion_suggests_std_children() {
        let dir = tempdir().unwrap();
        let src = r#"package main;

use std::

fn main() -> unit {
    ()
}
"#;
        let path = write_minimal_project(dir.path(), src);
        let completion = handlers::completion(
            &path,
            src,
            Position {
                line: 2,
                character: 9,
            },
        );
        expect!["env, fs, io, process"].assert_eq(&format_completion(completion));
    }

    #[test]
    fn colon_colon_completion_on_std_io() {
        let dir = tempdir().unwrap();
        let src = r#"package main;

use std::io;

fn main() -> unit {
    io::
}
"#;
        let path = write_minimal_project(dir.path(), src);
        let completion = handlers::completion(
            &path,
            src,
            Position {
                line: 5,
                character: 8,
            },
        );
        expect!["eprint, eprintln, print, println, read_stdin, write_stderr, write_stdout"]
            .assert_eq(&format_completion(completion));
    }

    #[test]
    fn colon_colon_completion_on_std_use_alias() {
        let dir = tempdir().unwrap();
        let src = r#"package main;

use std::env;

fn main() -> unit {
    env::
}
"#;
        let path = write_minimal_project(dir.path(), src);
        let completion = handlers::completion(
            &path,
            src,
            Position {
                line: 5,
                character: 9,
            },
        );
        expect!["args, current_dir, current_exe, var"].assert_eq(&format_completion(completion));
    }

    #[test]
    fn completion_accepts_utf16_positions_after_non_ascii_text() {
        let src = r#"fn main() -> unit {
    let values: Vec[int32] = vec_new(); let _ = "🙂"; values.
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let offset = src.find("values.\n").unwrap() + "values.".len();
        let position = doc
            .position(text_size::TextSize::from(offset as u32))
            .unwrap();
        let completion = handlers::completion(&path, src, position);
        let formatted = format_completion(completion);

        assert!(formatted.split(", ").any(|label| label == "len"));
        assert!(formatted.split(", ").any(|label| label == "push"));
    }

    #[test]
    fn colon_colon_completion_on_used_package() {
        let dir = tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("util")).unwrap();
        std::fs::write(
            dir.path().join("util/util.gom"),
            r#"
package util;

pub fn ping() -> string {
    "pong"
}
"#,
        )
        .unwrap();
        let src = r#"package main;

use demo::util;

fn main() -> unit {
    util::
}
"#;
        let path = write_minimal_project(dir.path(), src);
        let completion = handlers::completion(
            &path,
            src,
            Position {
                line: 5,
                character: 10,
            },
        );
        expect!["ping"].assert_eq(&format_completion(completion));
    }

    #[test]
    fn value_completion_suggests_keywords() {
        check_completion(
            r#"


fn main() {
    le
}
"#,
            4,
            6,
            expect!["let"],
        );
    }

    #[test]
    fn value_completion_keyword_kind_is_keyword() {
        let src = r#"


fn main() {
    le
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 4,
            character: 6,
        };
        let completion = handlers::completion(&path, src, position);
        let Some(CompletionResponse::Array(items)) = completion else {
            panic!("expected completion items");
        };
        let Some(item) = items.into_iter().find(|item| item.label == "let") else {
            panic!("expected let completion item");
        };
        assert_eq!(item.kind, Some(CompletionItemKind::KEYWORD));
    }

    #[test]
    fn completion_in_empty_function_body() {
        check_completion(
            r#"


fn greet(name: string) -> string {
    name
}

fn main() {
    
}
"#,
            8,
            4,
            expect!["empty completion"],
        );
    }
}

mod signature_help_tests {
    use super::*;

    #[test]
    fn signature_help_for_function_call() {
        check_signature_help(
            r#"


fn add(x: int32, y: string) -> bool {
    true
}

fn main() {
    let _ = add(1, 2);
}
"#,
            8,
            16,
            expect![[r#"
                label: (x: int32, y: string) -> bool
                active_parameter: 0
                parameters: x: int32, y: string"#]],
        );

        check_signature_help(
            r#"


fn add(x: int32, y: string) -> bool {
    true
}

fn main() {
    let _ = add(1, 2);
}
"#,
            8,
            18,
            expect![[r#"
                label: (x: int32, y: string) -> bool
                active_parameter: 1
                parameters: x: int32, y: string"#]],
        );
    }

    #[test]
    fn signature_help_for_method_call_hides_receiver() {
        check_signature_help(
            r#"


fn main() {
    let x = 1;
    let _ = x.to_string();
}
"#,
            5,
            24,
            expect![[r#"
                label: () -> string
                active_parameter: 0
                parameters: "#]],
        );
    }
}

mod inlay_hint_tests {
    use super::*;

    #[test]
    fn inlay_hints_for_let_bindings() {
        check_inlay_hints(
            r#"


fn main() {
    let x = 1;
    let y: int32 = 2;
    ()
}
"#,
            Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 99,
                    character: 0,
                },
            },
            expect!["4:9 type : int32"],
        );
    }

    #[test]
    fn inlay_hints_for_closure_params() {
        check_inlay_hints(
            r#"


fn main() {
    let f = |x| x + 1;
    ()
}
"#,
            Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 99,
                    character: 0,
                },
            },
            expect![[r#"
                4:9 type : (int32) -> int32
                4:14 type : int32"#]],
        );
    }

    #[test]
    fn inlay_hints_respect_range() {
        check_inlay_hints(
            r#"


fn main() {
    let a = 1;
    let b = 2;
    ()
}
"#,
            Range {
                start: Position {
                    line: 5,
                    character: 0,
                },
                end: Position {
                    line: 6,
                    character: 0,
                },
            },
            expect!["5:9 type : int32"],
        );
    }
}

mod goto_definition_tests {
    use super::*;

    fn test_file_path() -> PathBuf {
        let dir = std::env::temp_dir().join("goml_lsp_goto_test");
        std::fs::create_dir_all(&dir).unwrap();
        dir.join("goml_test.gom")
    }

    fn test_file_uri() -> Url {
        Url::from_file_path(test_file_path()).unwrap()
    }

    fn format_goto_result(result: Option<GotoDefinitionResponse>) -> String {
        fn short_uri(uri: &Url) -> String {
            let Ok(path) = uri.to_file_path() else {
                return uri.path().to_string();
            };
            if path.file_name().is_some_and(|n| n == "goml_test.gom") {
                return "goml_test.gom".to_string();
            }
            let parts = path
                .components()
                .filter_map(|c| c.as_os_str().to_str().map(|s| s.to_string()))
                .collect::<Vec<_>>();
            match parts.as_slice() {
                [] => "".to_string(),
                [one] => one.clone(),
                _ => format!("{}/{}", parts[parts.len() - 2], parts[parts.len() - 1]),
            }
        }

        match result {
            None => "no definition".to_string(),
            Some(GotoDefinitionResponse::Scalar(loc)) => {
                format!(
                    "{}:{}:{}",
                    short_uri(&loc.uri),
                    loc.range.start.line,
                    loc.range.start.character
                )
            }
            Some(GotoDefinitionResponse::Array(locs)) => {
                if locs.is_empty() {
                    return "no definition".to_string();
                }
                locs.iter()
                    .map(|loc| {
                        format!(
                            "{}:{}:{}",
                            short_uri(&loc.uri),
                            loc.range.start.line,
                            loc.range.start.character
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
            }
            Some(GotoDefinitionResponse::Link(links)) => {
                if links.is_empty() {
                    return "no definition".to_string();
                }
                links
                    .iter()
                    .map(|link| {
                        format!(
                            "{}:{}:{}",
                            short_uri(&link.target_uri),
                            link.target_range.start.line,
                            link.target_range.start.character
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
            }
        }
    }

    fn check_goto(src: &str, line: u32, character: u32, expect: Expect) {
        let path = test_file_path();
        let doc = Document::new(src.to_string());
        let uri = test_file_uri();
        let position = Position { line, character };
        let result = handlers::goto_definition(&uri, &path, src, position, &doc);
        expect.assert_eq(&format_goto_result(result));
    }

    fn position_in_src(src: &str, needle: &str, token: &str) -> Position {
        let needle_offset = src
            .find(needle)
            .unwrap_or_else(|| panic!("needle not found: {}", needle));
        let token_offset_in_needle = needle
            .find(token)
            .unwrap_or_else(|| panic!("token not found in needle: {}", token));
        let offset = needle_offset + token_offset_in_needle;
        let index = line_index::LineIndex::new(src);
        let line_col = index.line_col(text_size::TextSize::from(offset as u32));
        Position {
            line: line_col.line,
            character: line_col.col,
        }
    }

    fn check_goto_token(src: &str, needle: &str, token: &str, expect: Expect) {
        let path = test_file_path();
        let doc = Document::new(src.to_string());
        let uri = test_file_uri();
        let position = position_in_src(src, needle, token);
        let result = handlers::goto_definition(&uri, &path, src, position, &doc);
        expect.assert_eq(&format_goto_result(result));
    }

    #[test]
    fn goto_definition_local_variable() {
        check_goto(
            r#"


fn main() {
    let x = 42;
    println(x.to_string());
}
"#,
            5,
            12,
            expect!["goml_test.gom:4:8"],
        );
    }

    #[test]
    fn goto_definition_local_variable_via_token_search() {
        check_goto_token(
            r#"


fn main() {
    let x = 42;
    println(x.to_string());
}
"#,
            "println(x.to_string())",
            "x",
            expect!["goml_test.gom:4:8"],
        );
    }

    #[test]
    fn goto_definition_function() {
        check_goto(
            r#"


fn helper() -> int32 {
    42
}

fn main() {
    let x = helper();
}
"#,
            8,
            14,
            expect!["goml_test.gom:3:3"],
        );
    }

    #[test]
    fn goto_definition_struct_field() {
        check_goto(
            r#"


struct Point {
    x: int32,
    y: int32,
}

fn main() {
    let p = Point { x: 10, y: 20 };
    let _ = p.x;
}
"#,
            10,
            14,
            expect!["goml_test.gom:4:4"],
        );
    }

    #[test]
    fn goto_definition_parameter() {
        check_goto(
            r#"


fn double(n: int32) -> int32 {
    n * 2
}

fn main() {
    let _ = double(5);
}
"#,
            4,
            4,
            expect!["goml_test.gom:3:10"],
        );
    }

    fn check_module_goto_token(
        project_name: &str,
        rel_file: &str,
        needle: &str,
        token: &str,
        expect: Expect,
    ) {
        let project_dir = test_module_dir().join(project_name);
        let path = project_dir.join(rel_file);
        let src = std::fs::read_to_string(&path).unwrap();
        let doc = Document::new(src.clone());
        let uri = Url::from_file_path(&path).unwrap();
        let position = position_in_src(&src, needle, token);
        let result = handlers::goto_definition(&uri, &path, &src, position, &doc);
        expect.assert_eq(&format_goto_result(result));
    }

    fn temp_project_dir(test_name: &str) -> PathBuf {
        std::env::temp_dir()
            .join("goml_lsp_goto_definition_tests")
            .join(test_name)
    }

    fn write_file(path: &std::path::Path, content: &str) {
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, content).unwrap();
    }

    fn check_temp_module_goto_token(
        test_name: &str,
        rel_file: &str,
        needle: &str,
        token: &str,
        expect: Expect,
    ) {
        let root = temp_project_dir(test_name);
        let path = root.join(rel_file);
        let src = std::fs::read_to_string(&path).unwrap();
        let doc = Document::new(src.clone());
        let uri = Url::from_file_path(&path).unwrap();
        let position = position_in_src(&src, needle, token);
        let result = handlers::goto_definition(&uri, &path, &src, position, &doc);
        expect.assert_eq(&format_goto_result(result));
    }

    fn write_registry_project(test_name: &str, src: &str) -> PathBuf {
        let root = temp_project_dir(test_name);
        let _ = std::fs::remove_dir_all(&root);
        write_file(
            &root.join("goml.toml"),
            r#"
[module]
path = "demo"

[dependencies]
"alice::http" = "1.2.0"
"#,
        );
        write_file(&root.join("main.gom"), src);
        root
    }

    #[test]
    fn goto_definition_import_package() {
        check_module_goto_token(
            "project001",
            "main.gom",
            "use project001::lib;",
            "lib",
            expect!["lib/lib.gom:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_member_to_trait() {
        check_module_goto_token(
            "project007_trait_impl_orphan_ok",
            "main.gom",
            "use traitpkg::Show;",
            "Show",
            expect!["traitpkg/traitpkg.gom:2:10"],
        );
    }

    #[test]
    fn goto_definition_method_prefers_impl() {
        check_module_goto_token(
            "project007_trait_impl_orphan_ok",
            "main.gom",
            "item.show()",
            "show",
            expect!["datapkg/datapkg.gom:9:7"],
        );
    }

    #[test]
    fn goto_definition_enum_variant_across_package() {
        check_module_goto_token(
            "project001",
            "main.gom",
            "lib::Color::Green",
            "Green",
            expect!["lib/lib.gom:4:4"],
        );
    }

    #[test]
    fn goto_definition_struct_field_across_package() {
        check_module_goto_token(
            "project001",
            "main.gom",
            "x: 20",
            "x",
            expect!["lib/lib.gom:15:4"],
        );
    }

    #[test]
    fn goto_definition_use_package_project002() {
        check_module_goto_token(
            "project002",
            "main.gom",
            "use project002::util;",
            "util",
            expect!["util/util.gom:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_project003_math() {
        check_module_goto_token(
            "project003",
            "main.gom",
            "use project003::math;",
            "math",
            expect!["math/math.gom:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_project003_stats() {
        check_module_goto_token(
            "project003",
            "main.gom",
            "use project003::stats;",
            "stats",
            expect!["stats/stats.gom:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_project004() {
        check_module_goto_token(
            "project004",
            "main.gom",
            "use project004::util;",
            "util",
            expect!["util/util.gom:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_project005_shape() {
        check_module_goto_token(
            "project005",
            "main.gom",
            "use project005::shape;",
            "shape",
            expect!["shape/shape.gom:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_project005_geo() {
        check_module_goto_token(
            "project005",
            "main.gom",
            "use project005::geo;",
            "geo",
            expect!["geo/geo.gom:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_project006() {
        check_module_goto_token(
            "project006",
            "main.gom",
            "use project006::shape;",
            "shape",
            expect!["shape/shape.gom:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_project008_datapkg() {
        check_module_goto_token(
            "project008_trait_bounds_across_packages",
            "main.gom",
            "use project008::datapkg;",
            "datapkg",
            expect!["datapkg/datapkg.gom:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_project008_usepkg() {
        check_module_goto_token(
            "project008_trait_bounds_across_packages",
            "main.gom",
            "use project008::usepkg;",
            "usepkg",
            expect!["usepkg/usepkg.gom:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_segment_project007_traitpkg() {
        check_module_goto_token(
            "project007_trait_impl_orphan_ok",
            "main.gom",
            "use traitpkg::Show;",
            "traitpkg",
            expect!["traitpkg/traitpkg.gom:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_in_subpackage_project008_datapkg_traitpkg() {
        check_module_goto_token(
            "project008_trait_bounds_across_packages",
            "datapkg/datapkg.gom",
            "use project008::traitpkg;",
            "traitpkg",
            expect!["traitpkg/traitpkg.gom:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_in_subpackage_project008_usepkg_traitpkg() {
        check_module_goto_token(
            "project008_trait_bounds_across_packages",
            "usepkg/usepkg.gom",
            "use project008::traitpkg;",
            "traitpkg",
            expect!["traitpkg/traitpkg.gom:0:0"],
        );
    }

    #[test]
    fn goto_definition_value_project002_adjust() {
        check_module_goto_token(
            "project002",
            "main.gom",
            "util::adjust",
            "adjust",
            expect!["util/util.gom:12:7"],
        );
    }

    #[test]
    fn goto_definition_value_project002_dec() {
        check_module_goto_token(
            "project002",
            "main.gom",
            "util::dec",
            "dec",
            expect!["util/util.gom:8:7"],
        );
    }

    #[test]
    fn goto_definition_type_project003_pair() {
        check_module_goto_token(
            "project003",
            "main.gom",
            "math::Pair",
            "Pair",
            expect!["math/math.gom:2:11"],
        );
    }

    #[test]
    fn goto_definition_value_project003_sum() {
        check_module_goto_token(
            "project003",
            "main.gom",
            "stats::sum",
            "sum",
            expect!["stats/stats.gom:4:7"],
        );
    }

    #[test]
    fn goto_definition_variant_project003_add() {
        check_module_goto_token(
            "project003",
            "stats/stats.gom",
            "math::Op::Add",
            "Add",
            expect!["math/math.gom:8:4"],
        );
    }

    #[test]
    fn goto_definition_struct_field_project003_pair_a() {
        check_module_goto_token(
            "project003",
            "main.gom",
            "a: 9",
            "a",
            expect!["math/math.gom:3:4"],
        );
    }

    #[test]
    fn goto_definition_value_project005_move() {
        check_module_goto_token(
            "project005",
            "main.gom",
            "geo::move",
            "move",
            expect!["geo/geo.gom:9:7"],
        );
    }

    #[test]
    fn goto_definition_type_project005_shape_point_in_pattern() {
        check_module_goto_token(
            "project005",
            "geo/geo.gom",
            "shape::Point { x: x, y: y }",
            "Point",
            expect!["shape/shape.gom:2:11"],
        );
    }

    #[test]
    fn goto_definition_value_project008_bar_it() {
        check_module_goto_token(
            "project008_trait_bounds_across_packages",
            "main.gom",
            "usepkg::bar_it",
            "bar_it",
            expect!["usepkg/usepkg.gom:12:7"],
        );
    }

    #[test]
    fn goto_definition_type_in_generic_bound_project008_trait_c() {
        check_module_goto_token(
            "project008_trait_bounds_across_packages",
            "usepkg/usepkg.gom",
            "traitpkg::C",
            "C",
            expect!["traitpkg/traitpkg.gom:10:10"],
        );
    }

    #[test]
    fn goto_definition_package_segment_in_path_project001_lib() {
        check_module_goto_token(
            "project001",
            "main.gom",
            "lib::Color::Green",
            "lib",
            expect!["lib/lib.gom:0:0"],
        );
    }

    #[test]
    fn goto_definition_type_segment_in_path_project001_color() {
        check_module_goto_token(
            "project001",
            "main.gom",
            "lib::Color::Green",
            "Color",
            expect!["lib/lib.gom:2:9"],
        );
    }

    #[test]
    fn goto_definition_value_segment_in_path_project001_sum_point() {
        check_module_goto_token(
            "project001",
            "main.gom",
            "lib::sum_point",
            "sum_point",
            expect!["lib/lib.gom:19:7"],
        );
    }

    #[test]
    fn goto_definition_type_in_struct_literal_project001_point() {
        check_module_goto_token(
            "project001",
            "main.gom",
            "lib::Point",
            "Point",
            expect!["lib/lib.gom:14:11"],
        );
    }

    #[test]
    fn goto_definition_multi_file_in_package_project006_inc() {
        check_module_goto_token(
            "project006",
            "main.gom",
            "shape::inc",
            "inc",
            expect!["shape/shape.gom:11:7"],
        );
    }

    #[test]
    fn goto_definition_value_project006_sum() {
        check_module_goto_token(
            "project006",
            "main.gom",
            "shape::sum",
            "sum",
            expect!["shape/shape.gom:15:7"],
        );
    }

    #[test]
    fn goto_definition_builtin_option_variant_has_no_definition() {
        check_module_goto_token(
            "project009_builtin_option_result",
            "main.gom",
            "Option::Some",
            "Some",
            expect!["src/builtin_contract.gom:89:4"],
        );
    }

    #[test]
    fn goto_definition_builtin_vec_new_method() {
        check_goto_token(
            r#"


fn main() -> unit {
    let v: Vec[int32] = Vec::new();
    ()
}
"#,
            "Vec::new()",
            "new",
            expect!["src/builtin_prelude.gom:503:7"],
        );
    }

    #[test]
    fn goto_definition_builtin_ref_get() {
        check_goto_token(
            r#"


fn main() -> unit {
    let r = Ref::new(1);
    let x = r.get();
    println(x);
    ()
}
"#,
            "r.get()",
            "get",
            expect!["src/builtin_prelude.gom:746:7"],
        );
    }

    #[test]
    fn goto_definition_builtin_hashmap_methods() {
        let src = r#"


#[derive(Hash, Eq)]
enum Key {
    A,
}

fn main() -> unit {
    let m: HashMap[Key, int32] = HashMap::new();
    m.set(Key::A, 1);
    ()
}
"#;

        check_goto_token(
            src,
            "HashMap::new()",
            "new",
            expect!["src/builtin_prelude.gom:712:7"],
        );
        check_goto_token(
            src,
            "m.set(Key::A, 1)",
            "set",
            expect!["src/builtin_prelude.gom:720:7"],
        );
    }

    #[test]
    fn goto_definition_import_package_picks_package_file() {
        let root = temp_project_dir("import_package_file");
        let _ = std::fs::remove_dir_all(&root);
        write_file(
            &root.join("goml.toml"),
            r#"
[module]
path = "tmpmod"
"#,
        );
        write_file(
            &root.join("main.gom"),
            r#"package main;

use tmpmod::Pkg;

fn main() {
    let _ = 0;
}
"#,
        );
        write_file(
            &root.join("Pkg/Pkg.gom"),
            r#"package Pkg;

fn value() -> int32 { 0 }
"#,
        );

        check_temp_module_goto_token(
            "import_package_file",
            "main.gom",
            "use tmpmod::Pkg;",
            "Pkg",
            expect!["Pkg/Pkg.gom:0:0"],
        );
    }

    #[test]
    fn goto_definition_registry_packages() {
        let dir = tempdir().unwrap();
        let home = dir.path().join(".goml");
        write_cached_registry(&home);
        write_registry_project(
            "registry_packages",
            r#"package main;

use alice::http;
use alice::http::client;

fn main() -> unit {
    ()
}
"#,
        );

        with_goml_home(&home, || {
            check_temp_module_goto_token(
                "registry_packages",
                "main.gom",
                "use alice::http;",
                "http",
                expect!["1.2.0/goml.toml:0:0"],
            );
            check_temp_module_goto_token(
                "registry_packages",
                "main.gom",
                "use alice::http::client;",
                "client",
                expect!["client/client.gom:0:0"],
            );
        });
    }

    #[test]
    fn registry_packages_require_canonical_import_paths() {
        let dir = tempdir().unwrap();
        let home = dir.path().join(".goml");
        write_cached_registry(&home);
        let root = write_registry_project(
            "registry_packages_require_owner",
            r#"package main;

use http::client;

fn main() -> unit {
    ()
}
"#,
        );

        with_goml_home(&home, || {
            let path = root.join("main.gom");
            let src = std::fs::read_to_string(&path).unwrap();
            let doc = Document::new(src.clone());
            let diags = handlers::get_diagnostics(&path, &src, &doc);
            expect!["[0:0] error: package http::client is not provided by this module or its dependencies"]
            .assert_eq(&format_diagnostics(&diags));
        });
    }

    #[test]
    fn goto_definition_registry_members() {
        let dir = tempdir().unwrap();
        let home = dir.path().join(".goml");
        write_cached_registry(&home);
        write_registry_project(
            "registry_members",
            r#"package main;

use alice::http;
use alice::http::client;

fn main() -> unit {
    let _ = http::make_client();
    let _ = client::Client { name: "bob" };
}
"#,
        );

        with_goml_home(&home, || {
            check_temp_module_goto_token(
                "registry_members",
                "main.gom",
                "http::make_client",
                "make_client",
                expect!["1.2.0/lib.gom:5:7"],
            );
            check_temp_module_goto_token(
                "registry_members",
                "main.gom",
                "client::Client",
                "Client",
                expect!["client/client.gom:3:11"],
            );
        });
    }

    #[test]
    fn goto_definition_std_package_and_member() {
        let root = temp_project_dir("std_package_and_member");
        let _ = std::fs::remove_dir_all(&root);
        write_file(
            &root.join("goml.toml"),
            r#"
[module]
path = "demo"
"#,
        );
        write_file(
            &root.join("main.gom"),
            r#"package main;

use std::io;

fn main() -> unit {
    io::println("ok")
}
"#,
        );

        check_temp_module_goto_token(
            "std_package_and_member",
            "main.gom",
            "use std::io;",
            "io",
            expect!["io/io.gom:0:0"],
        );
        check_temp_module_goto_token(
            "std_package_and_member",
            "main.gom",
            "io::println",
            "println",
            expect!["io/io.gom:9:7"],
        );
    }

    #[test]
    fn goto_definition_unqualified_type_returns_multiple_candidates() {
        let root = temp_project_dir("unqualified_ambiguous_type");
        let _ = std::fs::remove_dir_all(&root);
        write_file(
            &root.join("goml.toml"),
            r#"
[module]
path = "tmpmod"
"#,
        );
        write_file(
            &root.join("main.gom"),
            r#"package main;

use tmpmod::A;
use tmpmod::B;

fn main() {
    let _ = Foo {};
}
"#,
        );
        write_file(
            &root.join("A/A.gom"),
            r#"package A;

pub struct Foo {}
"#,
        );
        write_file(
            &root.join("B/B.gom"),
            r#"package B;

pub struct Foo {}
"#,
        );

        check_temp_module_goto_token(
            "unqualified_ambiguous_type",
            "main.gom",
            "Foo {}",
            "Foo",
            expect![[r#"
                A/A.gom:2:11
                B/B.gom:2:11"#]],
        );
    }
}

mod document_tests {
    use super::*;
    use text_size::TextSize;

    fn check_position(src: &str, offset: u32, expect: Expect) {
        let doc = Document::new(src.to_string());
        match doc.position(TextSize::from(offset)) {
            Some(pos) => expect.assert_eq(&format!("{}:{}", pos.line, pos.character)),
            None => expect.assert_eq("invalid offset"),
        }
    }

    fn check_range(src: &str, start: u32, end: u32, expect: Expect) {
        let doc = Document::new(src.to_string());
        let range = doc.range(text_size::TextRange::new(
            TextSize::from(start),
            TextSize::from(end),
        ));
        match range {
            Some(r) => expect.assert_eq(&format!(
                "{}:{}-{}:{}",
                r.start.line, r.start.character, r.end.line, r.end.character
            )),
            None => expect.assert_eq("invalid range"),
        }
    }

    #[test]
    fn document_position_first_line() {
        check_position("hello\nworld", 0, expect!["0:0"]);
    }

    #[test]
    fn document_position_second_line() {
        check_position("hello\nworld", 6, expect!["1:0"]);
    }

    #[test]
    fn document_position_middle_of_line() {
        check_position("hello\nworld", 3, expect!["0:3"]);
    }

    #[test]
    fn document_range() {
        check_range("hello\nworld", 0, 5, expect!["0:0-0:5"]);
    }

    #[test]
    fn document_range_multiline() {
        check_range("hello\nworld", 0, 11, expect!["0:0-1:5"]);
    }

    #[test]
    fn document_positions_are_utf16_code_units() {
        check_position("a中🙂z", 1, expect!["0:1"]);
        check_position("a中🙂z", 4, expect!["0:2"]);
        check_position("a中🙂z", 8, expect!["0:4"]);
        check_position("a中🙂z", 9, expect!["0:5"]);
        check_range("a中🙂z", 1, 8, expect!["0:1-0:4"]);
    }

    #[test]
    fn document_converts_valid_utf16_positions_to_utf8_columns() {
        let doc = Document::new("a中🙂z".to_string());

        assert_eq!(
            doc.utf8_position(Position {
                line: 0,
                character: 4,
            }),
            Some((0, 8))
        );
        assert!(
            doc.offset(Position {
                line: 0,
                character: 3,
            })
            .is_none()
        );
        assert!(
            doc.offset(Position {
                line: 0,
                character: 6,
            })
            .is_none()
        );
    }
}

mod pipeline_integration_tests {
    use super::*;

    #[test]
    fn pipeline_000() {
        check_pipeline_diagnostics("000", expect!["no diagnostics"]);
    }

    #[test]
    fn pipeline_001() {
        check_pipeline_diagnostics("001", expect!["no diagnostics"]);
    }

    #[test]
    fn pipeline_002() {
        check_pipeline_diagnostics("002", expect!["no diagnostics"]);
    }

    #[test]
    fn pipeline_003() {
        check_pipeline_diagnostics("003", expect!["no diagnostics"]);
    }

    #[test]
    fn pipeline_004() {
        check_pipeline_diagnostics("004", expect!["no diagnostics"]);
    }

    #[test]
    fn pipeline_005() {
        check_pipeline_diagnostics("005", expect!["no diagnostics"]);
    }

    #[test]
    fn pipeline_006() {
        check_pipeline_diagnostics("006", expect!["no diagnostics"]);
    }

    #[test]
    fn pipeline_007_expr_pattern_matching() {
        check_pipeline_diagnostics("007_expr_pattern_matching", expect!["no diagnostics"]);
    }

    #[test]
    fn pipeline_008_expr_pattern_matching_unit() {
        check_pipeline_diagnostics("008_expr_pattern_matching_unit", expect!["case not found"]);
    }

    #[test]
    fn pipeline_009() {
        check_pipeline_diagnostics("009", expect!["no diagnostics"]);
    }

    #[test]
    fn pipeline_010() {
        check_pipeline_diagnostics("010", expect!["no diagnostics"]);
    }
}

mod complex_code_tests {
    use super::*;

    #[test]
    fn generics_hover() {
        check_hover(
            r#"


fn identity[T](x: T) -> T {
    x
}

fn main() {
    let n = identity(42);
    let s = identity("hello");
}
"#,
            8,
            12,
            expect![[r#"
                ```goml
                (int32) -> int32
                ```"#]],
        );
    }

    #[test]
    fn trait_method_hover() {
        check_hover(
            r#"


trait Greet {
    fn greet(Self) -> string;
}

struct Person {
    name: string,
}

impl Greet for Person {
    fn greet(self: Person) -> string {
        self.name
    }
}

fn main() {
    let p = Person { name: "Alice" };
    let greeting = Greet::greet(p);
}
"#,
            19,
            25,
            expect![[r#"
                ```goml
                (Person) -> string
                ```"#]],
        );
    }

    #[test]
    fn closure_hover() {
        check_hover(
            r#"


fn main() {
    let add = |x: int32, y: int32| -> int32 { x + y };
    let result: int32 = add(1, 2);
    println(result.to_string());
}
"#,
            4,
            14,
            expect!["no hover"],
        );
    }

    #[test]
    fn match_expression_hover() {
        check_hover(
            r#"


enum Result {
    Ok(int32),
    Err(string),
}

fn main() {
    let r = Result::Ok(42);
    let value = match r {
        Result::Ok(n) => n,
        Result::Err(_) => 0,
    };
}
"#,
            11,
            22,
            expect![[r#"
                ```goml
                int32
                ```"#]],
        );
    }

    #[test]
    fn ref_type_hover() {
        check_hover(
            r#"


fn main() {
    let counter = Ref::new(0);
    counter.set(counter.get() + 1);
    println(counter.get().to_string());
}
"#,
            4,
            8,
            expect![[r#"
                ```goml
                Ref[int32]
                ```"#]],
        );
    }

    #[test]
    fn array_hover() {
        check_hover(
            r#"


fn main() {
    let arr: [int32; 3] = [1, 2, 3];
    let first = arr[0];
}
"#,
            4,
            8,
            expect![[r#"
                ```goml
                [int32; 3]
                ```"#]],
        );
    }

    #[test]
    fn tuple_hover() {
        check_hover(
            r#"


fn main() {
    let pair = (42, "hello");
    let (n, s) = pair;
}
"#,
            4,
            8,
            expect![[r#"
                ```goml
                (int32, string)
                ```"#]],
        );
    }

    #[test]
    fn while_loop_diagnostics() {
        check_diagnostics(
            r#"


fn main() {
    let i: Ref[int32] = Ref::new(0);
    while i.get() < 10 {
        i.set(i.get() + 1);
    };
    println(i.get().to_string());
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn extern_function_diagnostics() {
        check_diagnostics(
            r#"


fn main() {
    let s = "value: 42";
    println(s);
}
"#,
            expect!["no diagnostics"],
        );
    }
}

mod edge_case_tests {
    use super::*;

    #[test]
    fn empty_file() {
        check_diagnostics("", expect!["no diagnostics"]);
    }

    #[test]
    fn package_declaration_is_supported() {
        check_diagnostics("package main;", expect!["no diagnostics"]);
    }

    #[test]
    fn unicode_in_strings() {
        check_diagnostics(
            r#"


fn main() {
    let s = "你好世界 🌍";
    println(s);
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn deeply_nested_expressions() {
        check_diagnostics(
            r#"


fn main() {
    let x: int32 = ((((1 + 2) * 3) - 4) / 2);
    println(x.to_string());
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn multiline_string() {
        check_diagnostics(
            r#"


fn main() {
    let s = "line1 line2 line3";
    println(s);
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn hover_at_file_start() {
        check_hover("\n\nfn main() {}", 0, 0, expect!["no hover"]);
    }

    #[test]
    fn hover_at_file_end() {
        check_hover("\n\nfn main() {}", 2, 10, expect!["no hover"]);
    }

    #[test]
    fn completion_at_file_start() {
        check_completion("\n\nfn main() {}", 0, 0, expect!["empty completion"]);
    }

    #[test]
    fn very_long_line() {
        let long_string = "a".repeat(1000);
        let src = format!(
            r#"


fn main() {{
    let s = "{}";
    println(s);
}}
"#,
            long_string
        );
        check_diagnostics(&src, expect!["no diagnostics"]);
    }

    #[test]
    fn many_functions() {
        let mut src = "\n\n".to_string();
        for i in 0..100 {
            src.push_str(&format!("fn func{}() -> int32 {{ {} }}\n", i, i));
        }
        src.push_str("fn main() { let _ = func0(); }");
        check_diagnostics(&src, expect!["no diagnostics"]);
    }
}

mod builtin_tests {
    use super::*;

    #[test]
    fn builtin_println() {
        check_diagnostics(
            r#"


fn main() {
    println("hello");
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn builtin_print() {
        check_diagnostics(
            r#"


fn main() {
    print("hello");
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn builtin_ref_operations() {
        check_diagnostics(
            r#"


fn main() {
    let r = Ref::new(42);
    let v = r.get();
    r.set(v + 1);
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn builtin_vec_operations() {
        check_diagnostics(
            r#"


fn main() {
    let v = Vec::new();
    v.push(1);
    v.push(2);
    let len = v.len();
    let first = v[0];
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn builtin_array_operations() {
        check_diagnostics(
            r#"


fn main() {
    let mut arr: [int32; 3] = [1, 2, 3];
    let v = arr[0];
    arr[0] = v + 1;
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn builtin_string_operations() {
        check_diagnostics(
            r#"


fn main() {
    let s = "hello";
    let len = s.len();
    let c = s.get(0);
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn builtin_hashmap_operations() {
        check_diagnostics(
            r#"


fn main() {
    let m = HashMap::new();
    m.set("key", 42);
    let v = m.get("key");
    let has = m.contains("key");
    let len = m.len();
    m.remove("key");
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn builtin_to_string_trait() {
        check_diagnostics(
            r#"


fn main() {
    let n: int32 = 42;
    let s = n.to_string();
    println(s);
}
"#,
            expect!["no diagnostics"],
        );
    }
}
mod exhaustiveness_tests {
    use super::*;

    #[test]
    fn exhaustive_bool_match() {
        check_diagnostics(
            r#"


fn main() -> int32 {
    match true {
        true => 1,
        false => 0,
    }
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn non_exhaustive_bool_missing_false() {
        check_diagnostics(
            r#"


fn main() -> int32 {
    match true {
        true => 1,
    }
}
"#,
            expect!["[4:4] error: non-exhaustive match: missing pattern false"],
        );
    }

    #[test]
    fn non_exhaustive_bool_missing_true() {
        check_diagnostics(
            r#"


fn main() -> int32 {
    match true {
        false => 0,
    }
}
"#,
            expect!["[4:4] error: non-exhaustive match: missing pattern true"],
        );
    }

    #[test]
    fn exhaustive_enum_match() {
        check_diagnostics(
            r#"


enum Color {
    Red,
    Green,
    Blue
}

fn main() -> int32 {
    let c = Color::Red;
    match c {
        Color::Red => 1,
        Color::Green => 2,
        Color::Blue => 3,
    }
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn non_exhaustive_enum_missing_variants() {
        check_diagnostics(
            r#"


enum Color {
    Red,
    Green,
    Blue
}

fn main() -> int32 {
    let c = Color::Red;
    match c {
        Color::Red => 1,
    }
}
"#,
            expect!["[11:4] error: non-exhaustive match: missing patterns Green, Blue"],
        );
    }

    #[test]
    fn exhaustive_enum_with_wildcard() {
        check_diagnostics(
            r#"


enum Color {
    Red,
    Green,
    Blue
}

fn main() -> int32 {
    let c = Color::Red;
    match c {
        Color::Red => 1,
        _ => 0,
    }
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn exhaustive_generic_enum() {
        check_diagnostics(
            r#"


enum Option[T] {
    Some(T),
    None
}

fn main() -> int32 {
    let x: Option[int32] = Option::Some(42);
    match x {
        Option::Some(n) => n,
        Option::None => 0,
    }
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn non_exhaustive_generic_enum() {
        check_diagnostics(
            r#"


enum Option[T] {
    Some(T),
    None
}

fn main() -> int32 {
    let x: Option[int32] = Option::Some(42);
    match x {
        Option::Some(n) => n,
    }
}
"#,
            expect!["[10:4] error: non-exhaustive match: missing pattern None"],
        );
    }

    #[test]
    fn exhaustive_int_with_wildcard() {
        check_diagnostics(
            r#"


fn main() -> int32 {
    match 42 {
        0 => 0,
        1 => 1,
        _ => 2,
    }
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn non_exhaustive_int_no_wildcard() {
        check_diagnostics(
            r#"


fn main() -> int32 {
    match 42 {
        0 => 0,
        1 => 1,
    }
}
"#,
            expect!["[4:4] error: non-exhaustive match on int32 literal; add a wildcard arm `_`"],
        );
    }

    #[test]
    fn exhaustive_string_with_wildcard() {
        check_diagnostics(
            r#"


fn main() -> int32 {
    match "hello" {
        "hello" => 1,
        _ => 0,
    }
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn non_exhaustive_string_no_wildcard() {
        check_diagnostics(
            r#"


fn main() -> int32 {
    match "hello" {
        "hello" => 1,
        "world" => 2,
    }
}
"#,
            expect!["[4:4] error: non-exhaustive match on string literal; add a wildcard arm `_`"],
        );
    }

    #[test]
    fn non_exhaustive_char_no_wildcard() {
        check_diagnostics(
            r#"


fn main() -> int32 {
    match 'a' {
        'a' => 1,
        'b' => 2,
    }
}
"#,
            expect!["[4:4] error: non-exhaustive match on char literal; add a wildcard arm `_`"],
        );
    }

    #[test]
    fn non_exhaustive_nested_tuple() {
        check_diagnostics(
            r#"


fn main() -> int32 {
    let pair = (true, false);
    match pair {
        (true, true) => 1,
        (true, false) => 2,
        (false, true) => 3,
    }
}
"#,
            expect!["[5:4] error: non-exhaustive match: missing pattern false"],
        );
    }
}
