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
