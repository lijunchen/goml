use std::{env, fs, path::PathBuf, process::Command};

fn next_noise(state: &mut u64) -> u64 {
    *state ^= *state << 13;
    *state ^= *state >> 7;
    *state ^= *state << 17;
    *state
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
        "int8",
        "int16",
        "int32",
        "int64",
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
        "-7i32".to_string(),
        number,
        "2.5f32".to_string(),
        "3.75f64".to_string(),
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
        "-12i32",
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

fn main() {
    let parser = env::args_os()
        .nth(1)
        .map(PathBuf::from)
        .expect("usage: diff_parser <gomlang-parser> <cst|ast> [iterations]");
    let mode = env::args()
        .nth(2)
        .expect("usage: diff_parser <gomlang-parser> <cst|ast> [iterations]");
    let iterations = env::args()
        .nth(3)
        .map(|value| value.parse::<usize>().expect("invalid iteration count"))
        .unwrap_or(2048);
    assert!(mode == "cst" || mode == "ast", "invalid mode");
    let directory =
        env::temp_dir().join(format!("gomlang-parser-{mode}-diff-{}", std::process::id()));
    fs::create_dir_all(&directory).unwrap();
    let input_path = directory.join("input.gom");
    let mut state = 0x6a09_e667_f3bc_c909u64;
    for iteration in 0..iterations {
        let source = if mode == "cst" {
            random_source(&mut state)
        } else {
            valid_source(&mut state)
        };
        fs::write(&input_path, &source).unwrap();
        let expected = if mode == "cst" {
            gomlang_parser_rust_oracle::encode_parse(&input_path, &source)
        } else {
            gomlang_parser_rust_oracle::encode_ast(&input_path, &source)
        };
        let output = Command::new(&parser)
            .arg(&mode)
            .arg(&input_path)
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
    fs::remove_dir_all(&directory).unwrap();
    println!("matched {iterations} generated {mode} inputs");
}
