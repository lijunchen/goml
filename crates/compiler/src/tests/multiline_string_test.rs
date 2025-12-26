use std::path::PathBuf;

use super::*;
use expect_test::expect;

#[test]
fn multiline_string_prints_lines() -> anyhow::Result<()> {
    let src = r#"
fn main() -> unit {
    let s = \\first line
        \\second line
    ;
    string_println(s)
}
"#;

    let path = PathBuf::from("multiline_string.gom");
    let compilation = pipeline::compile(&path, src)
        .map_err(|err| anyhow::anyhow!("compilation failed: {:?}", err))?;

    let go_source = compilation.go.to_pretty(&compilation.goenv, 120);
    let go_output = execute_go_source(&go_source)?;

    expect![[r#"
        first line
        second line
    "#]]
    .assert_eq(&go_output);

    Ok(())
}
