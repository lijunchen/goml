use std::path::PathBuf;

use super::*;
use expect_test::expect;

#[test]
fn multiline_string_prints_lines() -> anyhow::Result<()> {
    if !runtime_executor_available() {
        return Ok(());
    }

    let src = r#"
fn main() -> unit {
    let s = \\first line
        \\second line
    ;
    string_println(s)
}
"#;

    let path = PathBuf::from("multiline_string.gom");
    let compilation = pipeline::pipeline::compile(&path, src)
        .map_err(|err| anyhow::anyhow!("compilation failed: {:?}", err))?;

    let go_source = compilation.go.to_pretty(&compilation.goenv, 120);
    let go_output = execute_go_source(&go_source, "multiline_string_prints_lines")?;

    expect![[r#"
        first line
        second line
    "#]]
    .assert_eq(&go_output);

    Ok(())
}

#[test]
fn string_nul_escape_executes() -> anyhow::Result<()> {
    if !runtime_executor_available() {
        return Ok(());
    }

    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/string_nul_escape/main.gom");
    let src = std::fs::read_to_string(&path)?;
    let compilation = pipeline::pipeline::compile_single_file(&path, &src)
        .map_err(|err| anyhow::anyhow!("compilation failed: {:?}", err))?;

    let go_source = compilation.go.to_pretty(&compilation.goenv, 120);
    let go_output = execute_go_source(&go_source, "string_nul_escape_executes")?;

    expect![[r#"
        1
    "#]]
    .assert_eq(&go_output);

    Ok(())
}
