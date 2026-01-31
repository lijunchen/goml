use std::path::PathBuf;

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

mod diagnostics_tests {
    use super::*;

    #[test]
    fn valid_code_no_diagnostics() {
        let src = r#"
package Main;

fn main() {
    let x = 42;
    println(x.to_string());
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let diags = handlers::get_diagnostics(&path, src, &doc);
        assert!(diags.is_empty(), "Expected no diagnostics for valid code");
    }

    #[test]
    fn undefined_variable_error() {
        let src = r#"
package Main;

fn main() {
    println(undefined_var.to_string());
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let diags = handlers::get_diagnostics(&path, src, &doc);
        assert!(
            !diags.is_empty(),
            "Expected diagnostics for undefined variable"
        );
        assert!(
            diags
                .iter()
                .any(|d| d.severity == Some(DiagnosticSeverity::ERROR))
        );
    }

    #[test]
    fn type_mismatch_error() {
        let src = r#"
package Main;

fn add(x: int32, y: int32) -> int32 {
    x + y
}

fn main() {
    let result = add("hello", 42);
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let diags = handlers::get_diagnostics(&path, src, &doc);
        assert!(!diags.is_empty(), "Expected diagnostics for type mismatch");
    }

    #[test]
    fn parse_error() {
        let src = r#"
package Main;

fn main( {
    let x = 42;
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let diags = handlers::get_diagnostics(&path, src, &doc);
        assert!(!diags.is_empty(), "Expected diagnostics for parse error");
    }

    #[test]
    fn missing_return_type() {
        let src = r#"
package Main;

fn add(x: int32, y: int32) {
    x + y
}

fn main() {
    let _ = add(1, 2);
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let _diags = handlers::get_diagnostics(&path, src, &doc);
    }

    #[test]
    fn module_project001_no_errors() {
        let project_dir = test_module_dir().join("project001");
        let main_path = project_dir.join("main.gom");
        let src = std::fs::read_to_string(&main_path).unwrap();
        let doc = Document::new(src.clone());
        let diags = handlers::get_diagnostics(&main_path, &src, &doc);
        assert!(
            diags.is_empty(),
            "project001/main.gom should have no errors: {:?}",
            diags
        );
    }

    #[test]
    fn module_project002_no_errors() {
        let project_dir = test_module_dir().join("project002");
        let main_path = project_dir.join("main.gom");
        let src = std::fs::read_to_string(&main_path).unwrap();
        let doc = Document::new(src.clone());
        let diags = handlers::get_diagnostics(&main_path, &src, &doc);
        assert!(
            diags.is_empty(),
            "project002/main.gom should have no errors: {:?}",
            diags
        );
    }

    #[test]
    fn module_project003_no_errors() {
        let project_dir = test_module_dir().join("project003");
        let main_path = project_dir.join("main.gom");
        let src = std::fs::read_to_string(&main_path).unwrap();
        let doc = Document::new(src.clone());
        let diags = handlers::get_diagnostics(&main_path, &src, &doc);
        assert!(
            diags.is_empty(),
            "project003/main.gom should have no errors: {:?}",
            diags
        );
    }
}

mod hover_tests {
    use super::*;

    #[test]
    fn hover_on_variable() {
        let src = r#"
package Main;

fn main() {
    let x = 42;
    println(x.to_string());
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 5,
            character: 12,
        };
        let _hover = handlers::hover(&path, src, position);
    }

    #[test]
    fn hover_on_function_name() {
        let src = r#"
package Main;

fn add(x: int32, y: int32) -> int32 {
    x + y
}

fn main() {
    let result = add(1, 2);
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 3,
            character: 5,
        };
        let hover = handlers::hover(&path, src, position);
        assert!(hover.is_some(), "Expected hover info on function");
    }

    #[test]
    fn hover_on_function_call() {
        let src = r#"
package Main;

fn add(x: int32, y: int32) -> int32 {
    x + y
}

fn main() {
    let result = add(1, 2);
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 8,
            character: 18,
        };
        let hover = handlers::hover(&path, src, position);
        assert!(hover.is_some(), "Expected hover info on function call");
    }

    #[test]
    fn hover_on_struct_field() {
        let src = r#"
package Main;

struct Point {
    x: int32,
    y: int32,
}

fn main() {
    let p = Point { x: 10, y: 20 };
    println(p.x.to_string());
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 10,
            character: 14,
        };
        let hover = handlers::hover(&path, src, position);
        assert!(
            hover.is_some(),
            "Expected hover info on struct field access"
        );
    }

    #[test]
    fn hover_on_enum_variant() {
        let src = r#"
package Main;

enum Color {
    Red,
    Green,
    Blue,
}

fn main() {
    let c = Color::Red;
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 10,
            character: 18,
        };
        let hover = handlers::hover(&path, src, position);
        assert!(hover.is_some(), "Expected hover info on enum variant");
    }

    #[test]
    fn hover_on_parameter() {
        let src = r#"
package Main;

fn double(n: int32) -> int32 {
    n * 2
}

fn main() {
    let _ = double(5);
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 4,
            character: 4,
        };
        let hover = handlers::hover(&path, src, position);
        assert!(hover.is_some(), "Expected hover info on parameter");
    }

    #[test]
    fn hover_on_let_binding() {
        let src = r#"
package Main;

fn main() {
    let result: int32 = 42;
    println(result.to_string());
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 4,
            character: 8,
        };
        let hover = handlers::hover(&path, src, position);
        assert!(hover.is_some(), "Expected hover info on let binding");
    }

    #[test]
    fn hover_on_match_arm_binding() {
        let src = r#"
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
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 11,
            character: 22,
        };
        let hover = handlers::hover(&path, src, position);
        assert!(hover.is_some(), "Expected hover info on match arm binding");
    }
}

mod completion_tests {
    use super::*;

    #[test]
    fn dot_completion_on_struct() {
        let src = r#"
package Main;

struct Point {
    x: int32,
    y: int32,
}

fn main() {
    let p = Point { x: 10, y: 20 };
    p.
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 10,
            character: 6,
        };
        let completion = handlers::completion(&path, src, position);
        assert!(
            completion.is_some(),
            "Expected completions for struct fields"
        );
        if let Some(CompletionResponse::Array(items)) = completion {
            let labels: Vec<_> = items.iter().map(|i| i.label.as_str()).collect();
            assert!(labels.contains(&"x"), "Should suggest field 'x'");
            assert!(labels.contains(&"y"), "Should suggest field 'y'");
        }
    }

    #[test]
    fn dot_completion_on_int() {
        let src = r#"
package Main;

fn main() {
    let x = 42;
    x.
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 5,
            character: 6,
        };
        let completion = handlers::completion(&path, src, position);
        assert!(completion.is_some(), "Expected completions for int methods");
        if let Some(CompletionResponse::Array(items)) = completion {
            let labels: Vec<_> = items.iter().map(|i| i.label.as_str()).collect();
            assert!(
                labels.contains(&"to_string"),
                "Should suggest 'to_string' method"
            );
        }
    }

    #[test]
    fn colon_colon_completion_on_enum() {
        let src = r#"
package Main;

enum Color {
    Red,
    Green,
    Blue,
}

fn main() {
    let c = Color::
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 10,
            character: 19,
        };
        let completion = handlers::completion(&path, src, position);
        assert!(
            completion.is_some(),
            "Expected completions for enum variants"
        );
        if let Some(CompletionResponse::Array(items)) = completion {
            let labels: Vec<_> = items.iter().map(|i| i.label.as_str()).collect();
            assert!(labels.contains(&"Red"), "Should suggest variant 'Red'");
            assert!(labels.contains(&"Green"), "Should suggest variant 'Green'");
            assert!(labels.contains(&"Blue"), "Should suggest variant 'Blue'");
        }
    }

    #[test]
    fn value_completion_suggests_functions() {
        let src = r#"
package Main;

fn helper() -> int32 {
    42
}

fn main() {
    let x = hel
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 8,
            character: 15,
        };
        let _completion = handlers::completion(&path, src, position);
    }

    #[test]
    fn completion_in_empty_function_body() {
        let src = r#"
package Main;

fn greet(name: string) -> string {
    name
}

fn main() {
    
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 8,
            character: 4,
        };
        let _completion = handlers::completion(&path, src, position);
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

    #[test]
    fn goto_definition_local_variable() {
        let src = r#"
package Main;

fn main() {
    let x = 42;
    println(x.to_string());
}
"#;
        let path = test_file_path();
        let doc = Document::new(src.to_string());
        let uri = test_file_uri();
        let position = Position {
            line: 5,
            character: 12,
        };
        let _result = handlers::goto_definition(&uri, &path, src, position, &doc);
    }

    #[test]
    fn goto_definition_function() {
        let src = r#"
package Main;

fn helper() -> int32 {
    42
}

fn main() {
    let x = helper();
}
"#;
        let path = test_file_path();
        let doc = Document::new(src.to_string());
        let uri = test_file_uri();
        let position = Position {
            line: 8,
            character: 14,
        };
        let _result = handlers::goto_definition(&uri, &path, src, position, &doc);
    }

    #[test]
    fn goto_definition_struct_field() {
        let src = r#"
package Main;

struct Point {
    x: int32,
    y: int32,
}

fn main() {
    let p = Point { x: 10, y: 20 };
    let _ = p.x;
}
"#;
        let path = test_file_path();
        let doc = Document::new(src.to_string());
        let uri = test_file_uri();
        let position = Position {
            line: 10,
            character: 14,
        };
        let _result = handlers::goto_definition(&uri, &path, src, position, &doc);
    }

    #[test]
    fn goto_definition_parameter() {
        let src = r#"
package Main;

fn double(n: int32) -> int32 {
    n * 2
}

fn main() {
    let _ = double(5);
}
"#;
        let path = test_file_path();
        let doc = Document::new(src.to_string());
        let uri = test_file_uri();
        let position = Position {
            line: 4,
            character: 4,
        };
        let _result = handlers::goto_definition(&uri, &path, src, position, &doc);
    }
}

mod document_tests {
    use super::*;
    use text_size::TextSize;

    #[test]
    fn document_position_first_line() {
        let doc = Document::new("hello\nworld".to_string());
        let pos = doc.position(TextSize::from(0)).unwrap();
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 0);
    }

    #[test]
    fn document_position_second_line() {
        let doc = Document::new("hello\nworld".to_string());
        let pos = doc.position(TextSize::from(6)).unwrap();
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 0);
    }

    #[test]
    fn document_position_middle_of_line() {
        let doc = Document::new("hello\nworld".to_string());
        let pos = doc.position(TextSize::from(3)).unwrap();
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 3);
    }

    #[test]
    fn document_range() {
        let doc = Document::new("hello\nworld".to_string());
        let range = doc
            .range(text_size::TextRange::new(
                TextSize::from(0),
                TextSize::from(5),
            ))
            .unwrap();
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 0);
        assert_eq!(range.end.line, 0);
        assert_eq!(range.end.character, 5);
    }

    #[test]
    fn document_range_multiline() {
        let doc = Document::new("hello\nworld".to_string());
        let range = doc
            .range(text_size::TextRange::new(
                TextSize::from(0),
                TextSize::from(11),
            ))
            .unwrap();
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 0);
        assert_eq!(range.end.line, 1);
        assert_eq!(range.end.character, 5);
    }
}

mod pipeline_integration_tests {
    use super::*;

    fn run_diagnostics_on_pipeline_case(case_name: &str) {
        let case_dir = pipeline_dir().join(case_name);
        let main_path = case_dir.join("main.gom");
        if !main_path.exists() {
            return;
        }
        let src = std::fs::read_to_string(&main_path).unwrap();
        let doc = Document::new(src.clone());
        let diags = handlers::get_diagnostics(&main_path, &src, &doc);
        assert!(
            diags.is_empty(),
            "Pipeline case {} should have no LSP diagnostics: {:?}",
            case_name,
            diags
        );
    }

    #[test]
    fn pipeline_000() {
        run_diagnostics_on_pipeline_case("000");
    }
    #[test]
    fn pipeline_001() {
        run_diagnostics_on_pipeline_case("001");
    }
    #[test]
    fn pipeline_002() {
        run_diagnostics_on_pipeline_case("002");
    }
    #[test]
    fn pipeline_003() {
        run_diagnostics_on_pipeline_case("003");
    }
    #[test]
    fn pipeline_004() {
        run_diagnostics_on_pipeline_case("004");
    }
    #[test]
    fn pipeline_005() {
        run_diagnostics_on_pipeline_case("005");
    }
    #[test]
    fn pipeline_006() {
        run_diagnostics_on_pipeline_case("006");
    }
    #[test]
    fn pipeline_007_expr_pattern_matching() {
        run_diagnostics_on_pipeline_case("007_expr_pattern_matching");
    }
    #[test]
    fn pipeline_008_expr_pattern_matching_unit() {
        run_diagnostics_on_pipeline_case("008_expr_pattern_matching_unit");
    }
    #[test]
    fn pipeline_009() {
        run_diagnostics_on_pipeline_case("009");
    }
    #[test]
    fn pipeline_010() {
        run_diagnostics_on_pipeline_case("010");
    }
}

mod complex_code_tests {
    use super::*;

    #[test]
    fn generics_hover() {
        let src = r#"
package Main;

fn identity[T](x: T) -> T {
    x
}

fn main() {
    let n = identity(42);
    let s = identity("hello");
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 8,
            character: 12,
        };
        let hover = handlers::hover(&path, src, position);
        assert!(hover.is_some(), "Expected hover on generic function call");
    }

    #[test]
    fn trait_method_hover() {
        let src = r#"
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
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 19,
            character: 25,
        };
        let hover = handlers::hover(&path, src, position);
        assert!(hover.is_some(), "Expected hover on trait method call");
    }

    #[test]
    fn closure_hover() {
        let src = r#"
package Main;

fn main() {
    let add = |x: int32, y: int32| -> int32 { x + y };
    let result: int32 = add(1, 2);
    println(result.to_string());
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 4,
            character: 14,
        };
        let _hover = handlers::hover(&path, src, position);
    }

    #[test]
    fn match_expression_hover() {
        let src = r#"
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
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 11,
            character: 22,
        };
        let hover = handlers::hover(&path, src, position);
        assert!(hover.is_some(), "Expected hover in match arm");
    }

    #[test]
    fn ref_type_hover() {
        let src = r#"
package Main;

fn main() {
    let counter = ref(0);
    ref_set(counter, ref_get(counter) + 1);
    println(ref_get(counter).to_string());
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 4,
            character: 8,
        };
        let hover = handlers::hover(&path, src, position);
        assert!(hover.is_some(), "Expected hover on ref variable");
    }

    #[test]
    fn array_hover() {
        let src = r#"
package Main;

fn main() {
    let arr: [int32; 3] = [1, 2, 3];
    let first = array_get(arr, 0);
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 4,
            character: 8,
        };
        let hover = handlers::hover(&path, src, position);
        assert!(hover.is_some(), "Expected hover on array variable");
    }

    #[test]
    fn tuple_hover() {
        let src = r#"
package Main;

fn main() {
    let pair = (42, "hello");
    let (n, s) = pair;
}
"#;
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 4,
            character: 8,
        };
        let hover = handlers::hover(&path, src, position);
        assert!(hover.is_some(), "Expected hover on tuple variable");
    }

    #[test]
    fn while_loop_diagnostics() {
        let src = r#"
package Main;

fn main() {
    let i: Ref[int32] = ref(0);
    while ref_get(i) < 10 {
        ref_set(i, ref_get(i) + 1);
    };
    println(ref_get(i).to_string());
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let _diags = handlers::get_diagnostics(&path, src, &doc);
    }

    #[test]
    fn extern_function_diagnostics() {
        let src = r#"
package Main;

fn main() {
    let s = "value: 42";
    println(s);
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let diags = handlers::get_diagnostics(&path, src, &doc);
        assert!(diags.is_empty(), "Simple code should have no errors");
    }
}

mod edge_case_tests {
    use super::*;

    #[test]
    fn empty_file() {
        let src = "";
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let _diags = handlers::get_diagnostics(&path, src, &doc);
    }

    #[test]
    fn only_package_declaration() {
        let src = "package Main;";
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let _diags = handlers::get_diagnostics(&path, src, &doc);
    }

    #[test]
    fn unicode_in_strings() {
        let src = r#"
package Main;

fn main() {
    let s = "你好世界 🌍";
    println(s);
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let diags = handlers::get_diagnostics(&path, src, &doc);
        assert!(diags.is_empty(), "Unicode strings should work");
    }

    #[test]
    fn deeply_nested_expressions() {
        let src = r#"
package Main;

fn main() {
    let x: int32 = ((((1 + 2) * 3) - 4) / 2);
    println(x.to_string());
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let diags = handlers::get_diagnostics(&path, src, &doc);
        assert!(
            diags.is_empty(),
            "Nested expressions should work: {:?}",
            diags
        );
    }

    #[test]
    fn multiline_string() {
        let src = r#"
package Main;

fn main() {
    let s = "line1 line2 line3";
    println(s);
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let diags = handlers::get_diagnostics(&path, src, &doc);
        assert!(diags.is_empty(), "Simple strings should work");
    }

    #[test]
    fn hover_at_file_start() {
        let src = "package Main;\n\nfn main() {}";
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 0,
            character: 0,
        };
        let _hover = handlers::hover(&path, src, position);
    }

    #[test]
    fn hover_at_file_end() {
        let src = "package Main;\n\nfn main() {}";
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 2,
            character: 10,
        };
        let _hover = handlers::hover(&path, src, position);
    }

    #[test]
    fn completion_at_file_start() {
        let src = "package Main;\n\nfn main() {}";
        let path = PathBuf::from("test.gom");
        let position = Position {
            line: 0,
            character: 0,
        };
        let _completion = handlers::completion(&path, src, position);
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
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.clone());
        let diags = handlers::get_diagnostics(&path, &src, &doc);
        assert!(diags.is_empty(), "Long lines should work");
    }

    #[test]
    fn many_functions() {
        let mut src = "package Main;\n\n".to_string();
        for i in 0..100 {
            src.push_str(&format!("fn func{}() -> int32 {{ {} }}\n", i, i));
        }
        src.push_str("fn main() { let _ = func0(); }");

        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.clone());
        let diags = handlers::get_diagnostics(&path, &src, &doc);
        assert!(diags.is_empty(), "Many functions should work");
    }
}

mod builtin_tests {
    use super::*;

    #[test]
    fn builtin_println() {
        let src = r#"
package Main;

fn main() {
    println("hello");
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let diags = handlers::get_diagnostics(&path, src, &doc);
        assert!(diags.is_empty(), "println should be available");
    }

    #[test]
    fn builtin_print() {
        let src = r#"
package Main;

fn main() {
    print("hello");
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let diags = handlers::get_diagnostics(&path, src, &doc);
        assert!(diags.is_empty(), "print should be available");
    }

    #[test]
    fn builtin_ref_operations() {
        let src = r#"
package Main;

fn main() {
    let r = ref(42);
    let v = ref_get(r);
    ref_set(r, v + 1);
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let diags = handlers::get_diagnostics(&path, src, &doc);
        assert!(diags.is_empty(), "ref operations should be available");
    }

    #[test]
    fn builtin_vec_operations() {
        let src = r#"
package Main;

fn main() {
    let v = vec_new();
    vec_push(v, 1);
    vec_push(v, 2);
    let len = vec_len(v);
    let first = vec_get(v, 0);
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let diags = handlers::get_diagnostics(&path, src, &doc);
        assert!(diags.is_empty(), "vec operations should be available");
    }

    #[test]
    fn builtin_array_operations() {
        let src = r#"
package Main;

fn main() {
    let arr: [int32; 3] = [1, 2, 3];
    let v = array_get(arr, 0);
    array_set(arr, 0, v + 1);
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let diags = handlers::get_diagnostics(&path, src, &doc);
        assert!(diags.is_empty(), "array operations should be available");
    }

    #[test]
    fn builtin_string_operations() {
        let src = r#"
package Main;

fn main() {
    let s = "hello";
    let len = string_len(s);
    let c = string_get(s, 0);
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let diags = handlers::get_diagnostics(&path, src, &doc);
        assert!(diags.is_empty(), "string operations should be available");
    }

    #[test]
    fn builtin_hashmap_operations() {
        let src = r#"
package Main;

fn main() {
    let m = hashmap_new();
    hashmap_set(m, "key", 42);
    let v = hashmap_get(m, "key");
    let has = hashmap_contains(m, "key");
    let len = hashmap_len(m);
    hashmap_remove(m, "key");
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let diags = handlers::get_diagnostics(&path, src, &doc);
        assert!(diags.is_empty(), "hashmap operations should be available");
    }

    #[test]
    fn builtin_to_string_trait() {
        let src = r#"
package Main;

fn main() {
    let n: int32 = 42;
    let s = n.to_string();
    println(s);
}
"#;
        let path = PathBuf::from("test.gom");
        let doc = Document::new(src.to_string());
        let diags = handlers::get_diagnostics(&path, src, &doc);
        assert!(diags.is_empty(), "to_string should be available on int32");
    }
}
