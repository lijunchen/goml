use std::{fs, path::Path, process::Command};

use super::oracle;

fn next_noise(state: &mut u64) -> u64 {
    *state ^= *state << 13;
    *state ^= *state >> 7;
    *state ^= *state << 17;
    *state
}

fn lexer_source(state: &mut u64, fragments: &[&str]) -> String {
    let count = (next_noise(state) % 96) as usize;
    let mut source = String::new();
    for _ in 0..count {
        let index = next_noise(state) as usize % fragments.len();
        source.push_str(fragments[index]);
    }
    source
}

pub fn compare_lexer(parser: &Path, input_path: &Path, iterations: usize) {
    let fragments = [
        "fn", "let", "extern", "package", "::", "->", "=>", "&&", "||", "<<", ">>", "..", "..=",
        "(", ")", "{", "}", "[", "]", "'", "\"", "\\", "//", "\n", "\r\n", "\t", "0", "9i32",
        "1.5f64", "_name", "_", "é", "中", "🦀", "\u{301}", "\0",
    ];
    let seeds = [
        0x4d59_5df4_d0f3_3173u64,
        0x243f_6a88_85a3_08d3u64,
        0x1319_8a2e_0370_7344u64,
        0xa409_3822_299f_31d0u64,
    ];
    let chunk = iterations.div_ceil(seeds.len());
    let mut state = seeds[0];
    for iteration in 0..iterations {
        if iteration % chunk == 0 {
            state = seeds[iteration / chunk];
        }
        let source = lexer_source(&mut state, &fragments);
        fs::write(input_path, &source).unwrap();
        let expected = oracle::encode(&source);
        let output = Command::new(parser)
            .arg("lex")
            .arg(input_path)
            .output()
            .unwrap();
        if !output.status.success() {
            panic!(
                "GoML parser failed at iteration {iteration} for {source:?}: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        let actual = String::from_utf8(output.stdout).unwrap();
        assert_eq!(
            actual, expected,
            "lexer mismatch at iteration {iteration} for {source:?}"
        );
    }
}

fn random_source(state: &mut u64) -> String {
    let fragments = [
        "fn", "let", "extern", "package", "use", "trait", "impl", "enum", "struct", "type",
        "where", "match", "if", "else", "while", "for", "return", "break", "continue", "go", "dyn",
        "pub", "mut", "in", "as", "::", "->", "=>", "&&", "||", "<<", ">>", "..", "..=", "(", ")",
        "{", "}", "[", "]", "'", "\"", "\\", "// text", "\n", "\r\n", "\t", " ", "0", "9i32",
        "1.5f64", "name", "_", "é", "中", "🦀", "\u{301}", "\0",
    ];
    let count = (next_noise(state) % 96) as usize;
    let mut source = String::new();
    for _ in 0..count {
        source.push_str(fragments[next_noise(state) as usize % fragments.len()]);
    }
    source
}

fn decimal(state: &mut u64) -> String {
    let whole_length = if next_noise(state) % 12 == 0 {
        310 + (next_noise(state) % 120) as usize
    } else {
        1 + (next_noise(state) % 24) as usize
    };
    let fraction_length = 1 + (next_noise(state) % 32) as usize;
    let mut value = String::with_capacity(whole_length + fraction_length + 1);
    for index in 0..whole_length {
        let mut digit = (next_noise(state) % 10) as u8;
        if index == 0 && digit == 0 {
            digit = 1;
        }
        value.push((b'0' + digit) as char);
    }
    value.push('.');
    for _ in 0..fraction_length {
        value.push((b'0' + (next_noise(state) % 10) as u8) as char);
    }
    value
}

fn valid_source(state: &mut u64) -> String {
    let number = decimal(state);
    let types = [
        "unit",
        "bool",
        "int",
        "int8",
        "int16",
        "int32",
        "int64",
        "uint",
        "uint8",
        "uint16",
        "uint32",
        "uint64",
        "float32",
        "float64",
        "string",
        "char",
        "(int32, string)",
        "[int32; 3]",
        "Choice[int32]",
        "dyn Show",
        "(int32, string) -> bool",
    ];
    let expressions = vec![
        "()".to_string(),
        "true".to_string(),
        "42".to_string(),
        "-7".to_string(),
        number,
        "2.5".to_string(),
        "3.75".to_string(),
        "'λ'".to_string(),
        "\"λ\\u0041🙂\"".to_string(),
        "value".to_string(),
        "Choice::None".to_string(),
        "Choice::Some(value)".to_string(),
        "Point { x: value, y: 2 }".to_string(),
        "(value, \"text\")".to_string(),
        "[1, 2, 3]".to_string(),
        "|item: int32| item + 1".to_string(),
        "match value { 0..=2 => 1, item if item > 4 => item, _ => 0 }".to_string(),
        "if value > 0 { value } else { 0 }".to_string(),
        "while value > 0 { () }".to_string(),
        "for item in [1, 2] { let _ = item; }".to_string(),
        "return value".to_string(),
        "go || ()".to_string(),
        "id[int32](value)".to_string(),
        "value.to_string()".to_string(),
        "(value, 2).0".to_string(),
        "[value, 2][0]".to_string(),
        "!true".to_string(),
        "value as int64".to_string(),
        "result?".to_string(),
        "value + 2 * 3 << 1 == 14 || false".to_string(),
        "{ let nested = value; nested }".to_string(),
    ];
    let patterns = [
        "_",
        "name",
        "()",
        "true",
        "-12",
        "1.25",
        "\"text\"",
        "'λ'",
        "Choice::None",
        "Choice::Some(name)",
        "Point { x, y: other, .. }",
        "(left, right)",
        "[first, rest @ .., last]",
        "name @ Choice::Some(_)",
        "Choice::None | Choice::Some(_)",
        "1..=4",
    ];
    let ty = types[next_noise(state) as usize % types.len()];
    let binding_pattern = patterns[next_noise(state) as usize % patterns.len()];
    let arm_pattern = patterns[next_noise(state) as usize % patterns.len()];
    let first = &expressions[next_noise(state) as usize % expressions.len()];
    let second = &expressions[next_noise(state) as usize % expressions.len()];
    let third = &expressions[next_noise(state) as usize % expressions.len()];
    let fourth = &expressions[next_noise(state) as usize % expressions.len()];
    format!(
        "package generated;\nuse example::library as lib;\n#[derive(ToString)] struct Point {{ x: int32, y: int32 }}\nenum Choice[T] {{ None, Some(T) }}\nfn id[T: Eq](item: T) -> T where T: Hash {{ item }}\nfn generated(value: int32, result: Result[int32, string]) -> unit {{\n    let {binding_pattern}: {ty} = {first};\n    let _ = match {second} {{ {arm_pattern} if true => {third}, _ => {fourth} }};\n}}\n"
    )
}

fn typed_source(state: &mut u64) -> String {
    let value = next_noise(state) % 1000;
    match next_noise(state) % 18 {
        0 => format!("fn generated() -> int32 {{ {value} }}\n"),
        1 => "fn id[T](value: T) -> T { value }\nfn generated() -> string { id(\"text\") }\n"
            .to_string(),
        2 => "struct Point { x: int32, y: int32 }\nfn generated(value: int32) -> Point { Point { x: value, y: 2 } }\n"
            .to_string(),
        3 => "enum Choice[T] { None, Some(T) }\nfn generated(value: Choice[int32]) -> int32 { match value { Choice::Some(item) => item, Choice::None => 0 } }\n"
            .to_string(),
        4 => "fn generated(flag: bool, left: int32, right: int32) -> int32 { if flag { left } else { right } }\n"
            .to_string(),
        5 => "fn generated(value: int32) -> (int32, string) { (value, \"text\") }\n"
            .to_string(),
        6 => "fn generated() -> [int32; 3] { [1, 2, 3] }\n".to_string(),
        7 => "fn generated(value: int32) -> int32 { let add = |item: int32| item + 1; add(value) }\n"
            .to_string(),
        8 => "fn generated(value: int32) -> int32 { match value { 0..=2 => 1, _ => value } }\n"
            .to_string(),
        9 => "fn generated(value: int32) -> int32 { let nested = value; nested }\n"
            .to_string(),
        10 => "fn generated(left: bool, right: bool) -> bool { left && !right }\n".to_string(),
        11 => "fn generated() -> char { 'λ' }\n".to_string(),
        12 => "struct Point[T] { value: T }\nimpl[T] Point[T] { fn new(value: T) -> Point[T] { Point { value } } }\nfn generated() -> Point[int32] { Point::new(1) }\n"
            .to_string(),
        13 => "fn generated(values: [int32; 3]) -> int32 { values[0] }\n".to_string(),
        14 => "struct Values {} impl Iterator for Values { type Item = int32; fn next(self: Values) -> Option[int32] { Option::None } } fn generated(values: Values) -> unit { for value in values { let _ = value.to_string(); } }\n"
            .to_string(),
        15 => "trait Source { type Item; fn get(self: Self) -> Self::Item; } trait Pick[T] { fn pick(self: Self) -> T; } impl[S: Source] Pick[S::Item] for S { fn pick(self: S) -> S::Item { Source::get(self) } } struct Value { value: int32 } impl Source for Value { type Item = int32; fn get(self: Value) -> int32 { self.value } } fn generated() -> int32 { (Value { value: 7 }).pick() }\n"
            .to_string(),
        16 => "struct Values { values: Vec[int32] } impl IntoIterator for Values { type Item = int32; type IntoIter = FnIterator[int32]; fn into_iter(self: Values) -> FnIterator[int32] { self.values.iter() } } fn generated(values: Values) -> unit { for value in values { let _: int32 = value; } }\n"
            .to_string(),
        _ => "fn generated[S: IntoIterator](source: S) -> int32 where S::Item = int32 { let total = Ref::new(0); for value in source { total.set(total.get() + value); }; total.get() }\n"
            .to_string(),
    }
}

pub fn compare_parser(parser: &Path, input_path: &Path, mode: &str, iterations: usize) {
    assert!(
        mode == "cst" || mode == "ast" || mode == "hir" || mode == "tast",
        "invalid mode"
    );
    let seeds = [
        0x6a09_e667_f3bc_c909u64,
        0x3c6e_f372_fe94_f82bu64,
        0xa54f_f53a_5f1d_36f1u64,
        0x510e_527f_ade6_82d1u64,
    ];
    let chunk = iterations.div_ceil(seeds.len());
    let mut state = seeds[0];
    for iteration in 0..iterations {
        if iteration % chunk == 0 {
            state = seeds[iteration / chunk];
        }
        let source = match mode {
            "cst" => random_source(&mut state),
            "hir" | "tast" => typed_source(&mut state),
            "ast" => valid_source(&mut state),
            _ => unreachable!(),
        };
        fs::write(input_path, &source).unwrap();
        let expected = match mode {
            "cst" => oracle::encode_parse(input_path, &source),
            "ast" => oracle::encode_ast(input_path, &source),
            "hir" => oracle::encode_hir(input_path, &source),
            "tast" => oracle::encode_tast(input_path, &source),
            _ => unreachable!(),
        };
        if mode == "tast" && expected.is_empty() {
            panic!("Rust TAST oracle rejected iteration {iteration} for {source:?}");
        }
        if mode == "hir" && expected.is_empty() {
            panic!("Rust HIR oracle rejected iteration {iteration} for {source:?}");
        }
        let output = Command::new(parser)
            .arg("__canonical-stage")
            .arg(mode)
            .arg(input_path)
            .output()
            .unwrap();
        if !output.status.success() {
            panic!(
                "GoML parser failed in {mode} iteration {iteration} for {source:?}: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        let actual = String::from_utf8(output.stdout).unwrap();
        assert_eq!(
            actual, expected,
            "{mode} mismatch at iteration {iteration} for {source:?}"
        );
    }
}

pub fn compare_codegen(parser: &Path, input_path: &Path, iterations: usize) {
    let seeds = [
        0xbb67_ae85_84ca_a73bu64,
        0x9b05_688c_2b3e_6c1fu64,
        0x1f83_d9ab_fb41_bd6bu64,
        0x5be0_cd19_137e_2179u64,
    ];
    let chunk = iterations.div_ceil(seeds.len());
    let mut state = seeds[0];
    for iteration in 0..iterations {
        if iteration % chunk == 0 {
            state = seeds[iteration / chunk];
        }
        let source = typed_source(&mut state);
        fs::write(input_path, &source).unwrap();
        let expected = oracle::encode_go(input_path, &source);
        if expected.is_empty() {
            panic!("Rust Go oracle rejected iteration {iteration} for {source:?}");
        }
        let output = Command::new(parser)
            .arg("__canonical-stage")
            .arg("go")
            .arg(input_path)
            .output()
            .unwrap();
        if !output.status.success() {
            panic!(
                "GoML compiler failed in Go iteration {iteration} for {source:?}: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        let actual = String::from_utf8(output.stdout).unwrap();
        assert_eq!(
            actual, expected,
            "Go mismatch at iteration {iteration} for {source:?}"
        );
    }
}
