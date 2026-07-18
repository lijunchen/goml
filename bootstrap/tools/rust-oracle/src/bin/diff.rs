use std::{env, fs, path::PathBuf, process::Command};

fn next_noise(state: &mut u64) -> u64 {
    *state ^= *state << 13;
    *state ^= *state >> 7;
    *state ^= *state << 17;
    *state
}

fn generated_source(state: &mut u64, fragments: &[&str]) -> String {
    let count = (next_noise(state) % 96) as usize;
    let mut source = String::new();
    for _ in 0..count {
        let index = next_noise(state) as usize % fragments.len();
        source.push_str(fragments[index]);
    }
    source
}

fn main() {
    let parser = env::args_os()
        .nth(1)
        .map(PathBuf::from)
        .expect("usage: diff <gomlang-parser> [iterations]");
    let iterations = env::args()
        .nth(2)
        .map(|value| value.parse::<usize>().expect("invalid iteration count"))
        .unwrap_or(4096);
    let directory =
        env::temp_dir().join(format!("gomlang-parser-lexer-diff-{}", std::process::id()));
    fs::create_dir_all(&directory).unwrap();
    let input_path = directory.join("input.gom");
    let fragments = [
        "fn", "let", "extern", "package", "::", "->", "=>", "&&", "||", "<<", ">>", "..", "..=",
        "(", ")", "{", "}", "[", "]", "'", "\"", "\\", "//", "\n", "\r\n", "\t", "0", "9i32",
        "1.5f64", "_name", "_", "é", "中", "🦀", "\u{301}", "\0",
    ];
    let mut state = 0x4d59_5df4_d0f3_3173u64;
    for iteration in 0..iterations {
        let source = generated_source(&mut state, &fragments);
        fs::write(&input_path, &source).unwrap();
        let expected = gomlang_parser_rust_oracle::encode(&source);
        let output = Command::new(&parser)
            .arg("lex")
            .arg(&input_path)
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
    fs::remove_dir_all(&directory).unwrap();
    println!("matched {iterations} generated lexer inputs");
}
