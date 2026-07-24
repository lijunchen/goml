use std::path::Path;

use parser::{parse, syntax::MySyntaxNode};

fn assert_lossless(source: &str) {
    let result = parse(Path::new("robustness.gom"), source);
    let root = MySyntaxNode::new_root(result.green_node);
    assert_eq!(root.text().to_string(), source);
}

fn next_noise(state: &mut u64) -> u64 {
    *state ^= *state << 13;
    *state ^= *state >> 7;
    *state ^= *state << 17;
    *state
}

#[test]
fn every_utf8_prefix_parses_losslessly() {
    let corpus = [
        r#"package main;
use alice::compiler::syntax as syntax;
#[derive(Eq)]
struct Token[T] { kind: T, text: string }
enum Result[T, E] { Ok(T), Err(E) }
trait Parse[T] { type Error; fn parse(Self, Slice[T]) -> Result[T, Self::Error]; }
impl Parse[char] for Token[char] {
    type Error = string;
    fn parse(self: Token[char], input: Slice[char]) -> Result[char, string] {
        let first = if input.len() > 0i32 { input.get(0i32) } else { '中' };
        match first { '\n' => Result::Err("line"), value => Result::Ok(value) }
    }
}
fn main() -> unit {
    let values = [1i32, 2i32, 3i32];
    let total = values[0i32] + values[1i32] * values[2i32];
    let mapper = |value: int32| value << 1i32;
    while mapper(total) > 0i32 { break; };
}
"#,
        r#"fn broken(x: Vec[Option[(int32, string)] -> unit {
    let value = match x { Some((left, right) => left +, _ => "unterminated
"#,
        "#[derive(Eq\nextern pub fn (x:: => { ] ) ;\n",
        "fn unicode() -> unit { let text = \"héllo 🦀\"; let code = '中'; }\n",
        "\\\\first\n    \\\\second\n    \\\\third\n",
    ];

    for source in corpus {
        for end in source
            .char_indices()
            .map(|(index, _)| index)
            .chain(std::iter::once(source.len()))
        {
            assert_lossless(&source[..end]);
        }
    }
}

#[test]
fn fixed_seed_token_noise_parses_losslessly() {
    let fragments = [
        "fn",
        "extern",
        "struct",
        "enum",
        "trait",
        "impl",
        "match",
        "if",
        "else",
        "let",
        "return",
        "::",
        "->",
        "=>",
        "&&",
        "||",
        "(",
        ")",
        "{",
        "}",
        "[",
        "]",
        ",",
        ";",
        ":",
        "#",
        "_",
        "name",
        "0i32",
        "\"text\"",
        "'x'",
        " ",
        "\n",
        "// trailing\n",
        "中",
        "🦀",
        "\0",
    ];
    let seeds = [
        0x8a5c_1937_2df4_6b01u64,
        0x4d59_5df4_d0f3_3173u64,
        0xa076_1d64_78bd_642fu64,
        0xe703_7ed1_a0b4_28dbu64,
    ];

    for mut state in seeds {
        for _ in 0..2048 {
            let count = (next_noise(&mut state) % 64) as usize;
            let mut source = String::new();
            for _ in 0..count {
                let index = (next_noise(&mut state) as usize) % fragments.len();
                source.push_str(fragments[index]);
            }
            assert_lossless(&source);
        }
    }
}

#[test]
fn missing_path_segment_after_generic_qualifier_is_recoverable() {
    let source = "name::[]::&&";
    let result = parse(Path::new("missing_path.gom"), source);
    assert!(result.has_errors());
    assert!(result.diagnostics().iter().any(|diagnostic| {
        diagnostic.message() == "expected a path segment after '::'"
            || diagnostic.message() == "expected a path segment"
    }));
    let root = MySyntaxNode::new_root(result.green_node);
    assert_eq!(root.text().to_string(), source);
}

#[test]
fn legacy_expression_generics_report_turbofish_migration() {
    let source = "Convert[int32]::convert(value)";
    let result = parse(Path::new("legacy_generics.gom"), source);
    assert!(result.has_errors());
    assert!(result.diagnostics().iter().any(|diagnostic| {
        diagnostic.message() == "generic expression arguments must use `::[...]`"
    }));
    let root = MySyntaxNode::new_root(result.green_node);
    assert_eq!(root.text().to_string(), source);
}

fn assert_depth_error(source: &str, expected: &str) {
    let result = parse(Path::new("deep.gom"), source);
    let matches = result
        .diagnostics()
        .iter()
        .filter(|diagnostic| diagnostic.message() == expected)
        .count();
    assert_eq!(matches, 1, "{:?}", result.diagnostics());
}

#[test]
fn deeply_nested_expression_reports_an_error() {
    let source = format!(
        "fn main() -> unit {{ let value = {}1i32{}; }}",
        "(".repeat(4096),
        ")".repeat(4096)
    );
    assert_depth_error(&source, "expression is too deeply nested");
}

#[test]
fn deeply_nested_pattern_reports_an_error() {
    let source = format!(
        "fn main() -> unit {{ let {}_{} = 0i32; }}",
        "(".repeat(4096),
        ")".repeat(4096)
    );
    assert_depth_error(&source, "pattern is too deeply nested");
}

#[test]
fn deeply_nested_alias_pattern_reports_an_error() {
    let source = format!(
        "fn main() -> unit {{ let {}_ = 0i32; }}",
        "value @ ".repeat(4096),
    );
    assert_depth_error(&source, "pattern is too deeply nested");
}

#[test]
fn deeply_nested_type_reports_an_error() {
    let source = format!(
        "fn consume(value: {}int32{}) -> unit {{ () }}",
        "Vec[".repeat(4096),
        "]".repeat(4096)
    );
    assert_depth_error(&source, "type is too deeply nested");
}

#[test]
fn very_wide_parameter_list_parses_losslessly() {
    let params = (0..12_000)
        .map(|index| format!("value{index}: int32"))
        .collect::<Vec<_>>()
        .join(", ");
    let source = format!("fn consume({params}) -> unit {{ () }}\n");
    let result = parse(Path::new("wide.gom"), &source);
    assert!(
        !result.has_errors(),
        "{}",
        result.format_errors(&source).join("\n")
    );
    let root = MySyntaxNode::new_root(result.green_node);
    assert_eq!(root.text().to_string(), source);
}
