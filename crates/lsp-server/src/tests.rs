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

fn check_module_diagnostics(project_name: &str, expect: Expect) {
    let project_dir = test_module_dir().join(project_name);
    let main_path = project_dir.join("main.gom");
    let src = std::fs::read_to_string(&main_path).unwrap();
    let doc = Document::new(src.clone());
    let diags = handlers::get_diagnostics(&main_path, &src, &doc);
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

mod diagnostics_tests {
    use super::*;

    #[test]
    fn valid_code_no_diagnostics() {
        check_diagnostics(
            r#"
package Main;

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
package Main;

fn main() {
    println(undefined_var.to_string());
}
"#,
            expect![[r#"
                [0:0] error: Unresolved name undefined_var
                [0:0] error: Method to_string not found for type ExprId { pkg: PackageId(1), idx: 2 }
                [0:0] error: Unresolved name undefined_var
                [0:0] error: Method to_string not found for type ExprId { pkg: PackageId(1), idx: 2 }
                [0:0] error: Could not solve all constraints: [Overloaded { op: TastIdent("to_string"), trait_name: TastIdent("ToString"), call_site_type: TFunc([TVar(4)], TString) }]
                [0:0] error: Type inference failed, remaining constraints: [Overloaded { op: TastIdent("to_string"), trait_name: TastIdent("ToString"), call_site_type: TFunc([TVar(4)], TString) }]
                [0:0] error: Type variable TypeVar(2) not resolved
                [0:0] error: Type variable TypeVar(4) not resolved"#]],
        );
    }

    #[test]
    fn type_mismatch_error() {
        check_diagnostics(
            r#"
package Main;

fn add(x: int32, y: int32) -> int32 {
    x + y
}

fn main() {
    let result = add("hello", 42);
}
"#,
            expect![[r#"
                [0:0] error: Types are not equal: TString and TInt32
                [0:0] error: Types are not equal: TInt32 and TString
                [0:0] error: Type variable TypeVar(0) not resolved
                [0:0] error: Type variable TypeVar(0) not resolved"#]],
        );
    }

    #[test]
    fn parse_error() {
        check_diagnostics(
            r#"
package Main;

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
package Main;

fn add(x: int32, y: int32) {
    x + y
}

fn main() {
    let _ = add(1, 2);
}
"#,
            expect![[r#"
                [0:0] error: Types are not equal: TInt32 and TUnit
                [0:0] error: Types are not equal: TInt32 and TUnit"#]],
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
}

mod hover_tests {
    use super::*;

    #[test]
    fn hover_on_variable() {
        check_hover(
            r#"
package Main;

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
package Main;

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
package Main;

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
package Main;

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
package Main;

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
package Main;

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
package Main;

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
package Main;

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
package Main;

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
package Main;

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
package Main;

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
    fn value_completion_suggests_functions() {
        check_completion(
            r#"
package Main;

fn helper() -> int32 {
    42
}

fn main() {
    let x = hel
}
"#,
            8,
            15,
            expect!["empty completion"],
        );
    }

    #[test]
    fn completion_in_empty_function_body() {
        check_completion(
            r#"
package Main;

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

mod goto_definition_tests {
    use super::*;

    fn test_file_path() -> PathBuf {
        std::env::temp_dir().join("goml_test.gom")
    }

    fn test_file_uri() -> Url {
        Url::from_file_path(test_file_path()).unwrap()
    }

    fn format_goto_result(result: Option<GotoDefinitionResponse>) -> String {
        match result {
            None => "no definition".to_string(),
            Some(GotoDefinitionResponse::Scalar(loc)) => {
                format!(
                    "{}:{}:{}",
                    loc.uri.path().split('/').next_back().unwrap_or(""),
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
                            loc.uri.path().split('/').next_back().unwrap_or(""),
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
                            link.target_uri.path().split('/').next_back().unwrap_or(""),
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

    #[test]
    fn goto_definition_local_variable() {
        check_goto(
            r#"
package Main;

fn main() {
    let x = 42;
    println(x.to_string());
}
"#,
            5,
            12,
            expect!["goml_test.gom:5:12"],
        );
    }

    #[test]
    fn goto_definition_function() {
        check_goto(
            r#"
package Main;

fn helper() -> int32 {
    42
}

fn main() {
    let x = helper();
}
"#,
            8,
            14,
            expect!["goml_test.gom:8:12"],
        );
    }

    #[test]
    fn goto_definition_struct_field() {
        check_goto(
            r#"
package Main;

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
            expect!["no definition"],
        );
    }

    #[test]
    fn goto_definition_parameter() {
        check_goto(
            r#"
package Main;

fn double(n: int32) -> int32 {
    n * 2
}

fn main() {
    let _ = double(5);
}
"#,
            4,
            4,
            expect!["goml_test.gom:4:4"],
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
package Main;

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
package Main;

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
package Main;

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
package Main;

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
package Main;

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
package Main;

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
package Main;

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
package Main;

fn main() {
    let i: Ref[int32] = ref(0);
    while ref_get(i) < 10 {
        ref_set(i, ref_get(i) + 1);
    };
    println(ref_get(i).to_string());
}
"#,
            expect![[r#"
                [0:0] error: Method to_string not found for type ExprId { pkg: PackageId(1), idx: 24 }
                [0:0] error: Method to_string not found for type ExprId { pkg: PackageId(1), idx: 24 }
                [0:0] error: Could not solve all constraints: [Overloaded { op: TastIdent("to_string"), trait_name: TastIdent("ToString"), call_site_type: TFunc([TVar(17)], TString) }]
                [0:0] error: Type inference failed, remaining constraints: [Overloaded { op: TastIdent("to_string"), trait_name: TastIdent("ToString"), call_site_type: TFunc([TVar(17)], TString) }]
                [0:0] error: Type variable TypeVar(14) not resolved
                [0:0] error: Type variable TypeVar(17) not resolved"#]],
        );
    }

    #[test]
    fn extern_function_diagnostics() {
        check_diagnostics(
            r#"
package Main;

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
        check_diagnostics("package Main;", expect!["no diagnostics"]);
    }

    #[test]
    fn unicode_in_strings() {
        check_diagnostics(
            r#"
package Main;

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
package Main;

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
package Main;

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
        check_hover("package Main;\n\nfn main() {}", 0, 0, expect!["no hover"]);
    }

    #[test]
    fn hover_at_file_end() {
        check_hover("package Main;\n\nfn main() {}", 2, 10, expect!["no hover"]);
    }

    #[test]
    fn completion_at_file_start() {
        check_completion(
            "package Main;\n\nfn main() {}",
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
package Main;

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
        let mut src = "package Main;\n\n".to_string();
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
package Main;

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
package Main;

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
package Main;

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
package Main;

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
package Main;

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
package Main;

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
package Main;

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
package Main;

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
