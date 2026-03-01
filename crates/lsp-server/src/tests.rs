use std::collections::BTreeSet;
use std::path::PathBuf;

use expect_test::{Expect, expect};
use tower_lsp::lsp_types::*;

use crate::{Document, handlers};

fn test_module_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("compiler/src/tests/module")
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
                "package main;\nfn main() {\n  let s = \"hello\n  let x = 1;\n",
            ),
            (
                "unterminated_char_and_comment",
                "package main;\nfn main() {\n  let c = '\\u12\n  // trailing",
            ),
            (
                "dense_operators_and_partial_tokens",
                "package main;\nfn main() { let x = 1<<<<=>>>==!=&&||::..,,;; }\n",
            ),
            (
                "nested_brackets_missing_closers",
                "package main;\nfn main() { let _ = ((([1, 2, 3]); }\n",
            ),
            (
                "attribute_generics_and_dyn_partial",
                "#[derive(ToString)]\nfn f[T: Eq + Hash +](x: T) -> dyn Show {\n",
            ),
            (
                "invalid_tokens_and_escape_like_sequence",
                "package main;\nfn main() { let y = \\u2028; @@@ }\n",
            ),
            (
                "deeply_nested_expressions",
                "package main;\nfn main() { let _ = (((((((((((((1 + 2))))))))))))); }\n",
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
package main;

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
package main;

fn main() {
    println(undefined_var.to_string());
}
"#,
            expect![[r#"
                [4:12] error: Unresolved name undefined_var
                [4:12] error: Could not solve all type constraints
                [4:12] error: Type inference failed due to unresolved constraints
                [4:12] error: Could not infer type"#]],
        );
    }

    #[test]
    fn type_mismatch_error() {
        check_diagnostics(
            r#"
package main;

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
package main;

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
package main;

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
package main;

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
            "math/lib.gom",
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn module_project011_pipeline_package_no_missing_dir_errors() {
        check_module_file_diagnostics(
            "project011_complex_dependency_graph",
            "pipeline/lib.gom",
            expect!["no diagnostics"],
        );
    }
}

mod hover_tests {
    use super::*;

    #[test]
    fn hover_on_variable() {
        check_hover(
            r#"
package main;

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
package main;

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
    fn hover_on_function_call() {
        check_hover(
            r#"
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

fn main() {
    let x = 42;
    x.
}
"#,
            5,
            6,
            expect!["to_string"],
        );
    }

    #[test]
    fn colon_colon_completion_on_enum() {
        check_completion(
            r#"
package main;

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
package main;

fn main() {
    let v: Vec[int32] = Vec::new();
    v.
}
"#,
            5,
            6,
            expect!["get, len, new, push"],
        );
    }

    #[test]
    fn dot_completion_on_builtin_hashmap() {
        check_completion(
            r#"
package main;

fn main() {
    let m: HashMap[string, int32] = HashMap::new();
    m.
}
"#,
            5,
            6,
            expect!["contains, get, len, new, remove, set"],
        );
    }

    #[test]
    fn colon_colon_completion_on_builtin_vec() {
        check_completion(
            r#"
package main;

fn main() {
    let _ = Vec::
}
"#,
            4,
            17,
            expect!["get, len, new, push"],
        );
    }

    #[test]
    fn colon_colon_completion_on_builtin_hashmap() {
        check_completion(
            r#"
package main;

fn main() {
    let _ = HashMap::
}
"#,
            4,
            21,
            expect!["contains, get, len, new, remove, set"],
        );
    }

    #[test]
    fn value_completion_suggests_functions() {
        check_completion(
            r#"
package main;

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
    fn value_completion_suggests_keywords() {
        check_completion(
            r#"
package main;

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
package main;

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
package main;

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
package main;

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
                label: (int32, string) -> bool
                active_parameter: 0
                parameters: int32, string"#]],
        );

        check_signature_help(
            r#"
package main;

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
                label: (int32, string) -> bool
                active_parameter: 1
                parameters: int32, string"#]],
        );
    }

    #[test]
    fn signature_help_for_method_call_hides_receiver() {
        check_signature_help(
            r#"
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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

    fn check_module_goto(
        project_name: &str,
        rel_file: &str,
        line: u32,
        character: u32,
        expect: Expect,
    ) {
        let project_dir = test_module_dir().join(project_name);
        let path = project_dir.join(rel_file);
        let src = std::fs::read_to_string(&path).unwrap();
        let doc = Document::new(src.clone());
        let uri = Url::from_file_path(&path).unwrap();
        let position = Position { line, character };
        let result = handlers::goto_definition(&uri, &path, &src, position, &doc);
        expect.assert_eq(&format_goto_result(result));
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

    #[test]
    fn goto_definition_use_package_to_goml_toml() {
        check_module_goto("project001", "main.gom", 1, 6, expect!["lib/goml.toml:0:0"]);
    }

    #[test]
    fn goto_definition_use_member_to_trait() {
        check_module_goto(
            "project007_trait_impl_orphan_ok",
            "main.gom",
            1,
            17,
            expect!["traitpkg/lib.gom:2:6"],
        );
    }

    #[test]
    fn goto_definition_method_prefers_impl() {
        check_module_goto(
            "project007_trait_impl_orphan_ok",
            "main.gom",
            6,
            18,
            expect!["datapkg/lib.gom:8:7"],
        );
    }

    #[test]
    fn goto_definition_enum_variant_across_package() {
        check_module_goto("project001", "main.gom", 4, 42, expect!["lib/lib.gom:4:4"]);
    }

    #[test]
    fn goto_definition_struct_field_across_package() {
        check_module_goto("project001", "main.gom", 5, 25, expect!["lib/lib.gom:15:4"]);
    }

    #[test]
    fn goto_definition_use_package_project002() {
        check_module_goto_token(
            "project002",
            "main.gom",
            "use util;",
            "util",
            expect!["util/goml.toml:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_project003_math() {
        check_module_goto_token(
            "project003",
            "main.gom",
            "use math;",
            "math",
            expect!["math/goml.toml:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_project003_stats() {
        check_module_goto_token(
            "project003",
            "main.gom",
            "use stats;",
            "stats",
            expect!["stats/goml.toml:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_project004() {
        check_module_goto_token(
            "project004",
            "main.gom",
            "use util;",
            "util",
            expect!["util/goml.toml:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_project005_shape() {
        check_module_goto_token(
            "project005",
            "main.gom",
            "use shape;",
            "shape",
            expect!["shape/goml.toml:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_project005_geo() {
        check_module_goto_token(
            "project005",
            "main.gom",
            "use geo;",
            "geo",
            expect!["geo/goml.toml:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_project006() {
        check_module_goto_token(
            "project006",
            "main.gom",
            "use shape;",
            "shape",
            expect!["shape/goml.toml:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_project008_datapkg() {
        check_module_goto_token(
            "project008_trait_bounds_across_packages",
            "main.gom",
            "use datapkg;",
            "datapkg",
            expect!["datapkg/goml.toml:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_project008_usepkg() {
        check_module_goto_token(
            "project008_trait_bounds_across_packages",
            "main.gom",
            "use usepkg;",
            "usepkg",
            expect!["usepkg/goml.toml:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_segment_project007_traitpkg() {
        check_module_goto_token(
            "project007_trait_impl_orphan_ok",
            "main.gom",
            "use traitpkg::Show;",
            "traitpkg",
            expect!["traitpkg/goml.toml:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_in_subpackage_project008_datapkg_traitpkg() {
        check_module_goto_token(
            "project008_trait_bounds_across_packages",
            "datapkg/lib.gom",
            "use traitpkg;",
            "traitpkg",
            expect!["traitpkg/goml.toml:0:0"],
        );
    }

    #[test]
    fn goto_definition_use_package_in_subpackage_project008_usepkg_traitpkg() {
        check_module_goto_token(
            "project008_trait_bounds_across_packages",
            "usepkg/lib.gom",
            "use traitpkg;",
            "traitpkg",
            expect!["traitpkg/goml.toml:0:0"],
        );
    }

    #[test]
    fn goto_definition_value_project002_adjust() {
        check_module_goto_token(
            "project002",
            "main.gom",
            "util::adjust",
            "adjust",
            expect!["util/lib.gom:11:3"],
        );
    }

    #[test]
    fn goto_definition_value_project002_dec() {
        check_module_goto_token(
            "project002",
            "main.gom",
            "util::dec",
            "dec",
            expect!["util/lib.gom:7:3"],
        );
    }

    #[test]
    fn goto_definition_type_project003_pair() {
        check_module_goto_token(
            "project003",
            "main.gom",
            "math::Pair",
            "Pair",
            expect!["math/lib.gom:2:7"],
        );
    }

    #[test]
    fn goto_definition_value_project003_sum() {
        check_module_goto_token(
            "project003",
            "main.gom",
            "stats::sum",
            "sum",
            expect!["stats/lib.gom:3:3"],
        );
    }

    #[test]
    fn goto_definition_variant_project003_add() {
        check_module_goto_token(
            "project003",
            "stats/lib.gom",
            "math::Op::Add",
            "Add",
            expect!["math/lib.gom:8:4"],
        );
    }

    #[test]
    fn goto_definition_struct_field_project003_pair_a() {
        check_module_goto_token(
            "project003",
            "main.gom",
            "a: 9",
            "a",
            expect!["math/lib.gom:3:4"],
        );
    }

    #[test]
    fn goto_definition_value_project005_move() {
        check_module_goto_token(
            "project005",
            "main.gom",
            "geo::move",
            "move",
            expect!["geo/lib.gom:8:3"],
        );
    }

    #[test]
    fn goto_definition_type_project005_shape_point_in_pattern() {
        check_module_goto_token(
            "project005",
            "geo/lib.gom",
            "shape::Point { x: x, y: y }",
            "Point",
            expect!["shape/lib.gom:2:7"],
        );
    }

    #[test]
    fn goto_definition_value_project008_bar_it() {
        check_module_goto_token(
            "project008_trait_bounds_across_packages",
            "main.gom",
            "usepkg::bar_it",
            "bar_it",
            expect!["usepkg/lib.gom:11:3"],
        );
    }

    #[test]
    fn goto_definition_type_in_generic_bound_project008_trait_c() {
        check_module_goto_token(
            "project008_trait_bounds_across_packages",
            "usepkg/lib.gom",
            "traitpkg::C",
            "C",
            expect!["traitpkg/lib.gom:10:6"],
        );
    }

    #[test]
    fn goto_definition_package_segment_in_path_project001_lib() {
        check_module_goto_token(
            "project001",
            "main.gom",
            "lib::Color::Green",
            "lib",
            expect!["lib/goml.toml:0:0"],
        );
    }

    #[test]
    fn goto_definition_type_segment_in_path_project001_color() {
        check_module_goto_token(
            "project001",
            "main.gom",
            "lib::Color::Green",
            "Color",
            expect!["lib/lib.gom:2:5"],
        );
    }

    #[test]
    fn goto_definition_value_segment_in_path_project001_sum_point() {
        check_module_goto_token(
            "project001",
            "main.gom",
            "lib::sum_point",
            "sum_point",
            expect!["lib/lib.gom:19:3"],
        );
    }

    #[test]
    fn goto_definition_type_in_struct_literal_project001_point() {
        check_module_goto_token(
            "project001",
            "main.gom",
            "lib::Point",
            "Point",
            expect!["lib/lib.gom:14:7"],
        );
    }

    #[test]
    fn goto_definition_multi_file_in_package_project006_inc() {
        check_module_goto_token(
            "project006",
            "main.gom",
            "shape::inc",
            "inc",
            expect!["shape/ops.gom:2:3"],
        );
    }

    #[test]
    fn goto_definition_value_project006_sum() {
        check_module_goto_token(
            "project006",
            "main.gom",
            "shape::sum",
            "sum",
            expect!["shape/ops.gom:6:3"],
        );
    }

    #[test]
    fn goto_definition_builtin_option_variant_has_no_definition() {
        check_module_goto_token(
            "project009_builtin_option_result",
            "main.gom",
            "Option::Some",
            "Some",
            expect!["src/builtin.gom:102:4"],
        );
    }

    #[test]
    fn goto_definition_builtin_vec_new() {
        check_goto_token(
            r#"
package main;

fn main() -> unit {
    let v: Vec[int32] = vec_new();
    ()
}
"#,
            "vec_new()",
            "vec_new",
            expect!["src/builtin.gom:111:10"],
        );
    }

    #[test]
    fn goto_definition_builtin_ref_get() {
        check_goto_token(
            r#"
package main;

fn main() -> unit {
    let r = ref(1);
    let x = ref_get(r);
    println(x);
    ()
}
"#,
            "ref_get(r)",
            "ref_get",
            expect!["src/builtin.gom:487:10"],
        );
    }

    #[test]
    fn goto_definition_builtin_hashmap_methods() {
        let src = r#"
package main;

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
            expect!["src/builtin.gom:459:7"],
        );
        check_goto_token(
            src,
            "m.set(Key::A, 1)",
            "set",
            expect!["src/builtin.gom:467:7"],
        );
    }

    #[test]
    fn goto_definition_use_package_fallback_picks_first_gom_file() {
        let root = temp_project_dir("use_fallback_first_gom");
        let _ = std::fs::remove_dir_all(&root);
        write_file(
            &root.join("goml.toml"),
            r#"
[module]
name = "tmpmod"

[package]
name = "main"
entry = "main.gom"
"#,
        );
        write_file(
            &root.join("main.gom"),
            r#"
package main;
use Pkg;

fn main() {
    let _ = 0;
}
"#,
        );
        write_file(
            &root.join("Pkg/b.gom"),
            r#"
package Pkg;

fn b() -> int32 { 0 }
"#,
        );
        write_file(
            &root.join("Pkg/a.gom"),
            r#"
package Pkg;

fn a() -> int32 { 0 }
"#,
        );

        check_temp_module_goto_token(
            "use_fallback_first_gom",
            "main.gom",
            "use Pkg;",
            "Pkg",
            expect!["Pkg/a.gom:0:0"],
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
name = "tmpmod"

[package]
name = "main"
entry = "main.gom"
"#,
        );
        write_file(
            &root.join("main.gom"),
            r#"
package main;
use A;
use B;

fn main() {
    let _ = Foo {};
}
"#,
        );
        write_file(
            &root.join("A/goml.toml"),
            r#"
[package]
name = "A"
"#,
        );
        write_file(
            &root.join("A/lib.gom"),
            r#"
package A;

struct Foo {}
"#,
        );
        write_file(
            &root.join("B/goml.toml"),
            r#"
[package]
name = "B"
"#,
        );
        write_file(
            &root.join("B/lib.gom"),
            r#"
package B;

struct Foo {}
"#,
        );

        check_temp_module_goto_token(
            "unqualified_ambiguous_type",
            "main.gom",
            "Foo {}",
            "Foo",
            expect![[r#"
                A/lib.gom:3:7
                B/lib.gom:3:7"#]],
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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

fn main() {
    let counter = ref(0);
    ref_set(counter, ref_get(counter) + 1);
    println(ref_get(counter).to_string());
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
package main;

fn main() {
    let arr: [int32; 3] = [1, 2, 3];
    let first = array_get(arr, 0);
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
package main;

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
package main;

fn main() {
    let i: Ref[int32] = ref(0);
    while ref_get(i) < 10 {
        ref_set(i, ref_get(i) + 1);
    };
    println(ref_get(i).to_string());
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn extern_function_diagnostics() {
        check_diagnostics(
            r#"
package main;

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
    fn only_package_declaration() {
        check_diagnostics("package main;", expect!["no diagnostics"]);
    }

    #[test]
    fn unicode_in_strings() {
        check_diagnostics(
            r#"
package main;

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
package main;

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
package main;

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
        check_hover("package main;\n\nfn main() {}", 0, 0, expect!["no hover"]);
    }

    #[test]
    fn hover_at_file_end() {
        check_hover("package main;\n\nfn main() {}", 2, 10, expect!["no hover"]);
    }

    #[test]
    fn completion_at_file_start() {
        check_completion(
            "package main;\n\nfn main() {}",
            0,
            0,
            expect!["empty completion"],
        );
    }

    #[test]
    fn very_long_line() {
        let long_string = "a".repeat(1000);
        let src = format!(
            r#"
package main;

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
        let mut src = "package main;\n\n".to_string();
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
package main;

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
package main;

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
package main;

fn main() {
    let r = ref(42);
    let v = ref_get(r);
    ref_set(r, v + 1);
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn builtin_vec_operations() {
        check_diagnostics(
            r#"
package main;

fn main() {
    let v = vec_new();
    vec_push(v, 1);
    vec_push(v, 2);
    let len = vec_len(v);
    let first = vec_get(v, 0);
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn builtin_array_operations() {
        check_diagnostics(
            r#"
package main;

fn main() {
    let arr: [int32; 3] = [1, 2, 3];
    let v = array_get(arr, 0);
    array_set(arr, 0, v + 1);
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn builtin_string_operations() {
        check_diagnostics(
            r#"
package main;

fn main() {
    let s = "hello";
    let len = string_len(s);
    let c = string_get(s, 0);
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn builtin_hashmap_operations() {
        check_diagnostics(
            r#"
package main;

fn main() {
    let m = hashmap_new();
    hashmap_set(m, "key", 42);
    let v = hashmap_get(m, "key");
    let has = hashmap_contains(m, "key");
    let len = hashmap_len(m);
    hashmap_remove(m, "key");
}
"#,
            expect!["no diagnostics"],
        );
    }

    #[test]
    fn builtin_to_string_trait() {
        check_diagnostics(
            r#"
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
package main;

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
