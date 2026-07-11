use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::OnceLock;

use expect_test::expect;
use tempfile::TempDir;

const HELLO_PROGRAM: &str = r#"fn main() -> unit {
    println("hello")
}
"#;

const FUNCTION_VECTOR_PROGRAM: &str = r#"fn inc(x: int32) -> int32 {
    x + 1
}

fn dec(x: int32) -> int32 {
    x - 1
}

fn main() -> unit {
    let fs: Vec[(int32) -> int32] = Vec::new();
    fs.push(inc);
    fs.push(dec);
    let f = fs[0];
    println(f(10));
}
"#;

fn gomlc_bin() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_gomlc"))
}

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("gomlc crate must live under crates/gomlc")
        .to_path_buf()
}

fn write_program(contents: &str) -> anyhow::Result<(TempDir, PathBuf)> {
    let dir = tempfile::tempdir()?;
    let path = dir.path().join("main.gom");
    let contents = if contents.trim_start().starts_with("package ") {
        contents.to_string()
    } else {
        format!("package main;\n\n{contents}")
    };
    fs::write(&path, contents)?;
    Ok((dir, path))
}

fn deep_left_nested_tuple_type(depth: usize) -> String {
    let mut ty = "int32".to_string();
    for _ in 0..depth {
        ty = format!("({}, int32)", ty);
    }
    ty
}

fn wide_struct_pattern_program(field_count: usize) -> String {
    let fields = (0..field_count)
        .map(|idx| format!("f{}: int32", idx))
        .collect::<Vec<_>>()
        .join(", ");
    let values = (0..field_count)
        .map(|idx| format!("f{}: {}i32", idx, idx))
        .collect::<Vec<_>>()
        .join(", ");
    let pattern_fields = (0..field_count)
        .map(|idx| format!("f{}: x{}", idx, idx))
        .collect::<Vec<_>>()
        .join(", ");
    let sum = (0..200)
        .map(|idx| format!("x{}", idx))
        .collect::<Vec<_>>()
        .join(" + ");
    format!(
        "package main;\n\nstruct S {{ {fields} }}\nfn main() -> unit {{ let s = S {{ {values} }}; let total = match s {{ S {{ {pattern_fields} }} => {sum} }}; println(total.to_string()) }}\n"
    )
}

fn go_available() -> bool {
    static AVAILABLE: OnceLock<bool> = OnceLock::new();
    *AVAILABLE.get_or_init(|| {
        Command::new("go")
            .arg("version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .is_ok_and(|status| status.success())
    })
}

fn yaegi_available() -> bool {
    static AVAILABLE: OnceLock<bool> = OnceLock::new();
    *AVAILABLE.get_or_init(|| {
        Command::new("yaegi")
            .arg("help")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .is_ok_and(|status| status.success())
    })
}

fn runtime_executor_available() -> bool {
    yaegi_available() || go_available()
}

fn copy_dir_recursive(src: &Path, dst: &Path) -> anyhow::Result<()> {
    fs::create_dir_all(dst)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let target = dst.join(entry.file_name());
        if entry.file_type()?.is_dir() {
            copy_dir_recursive(&entry.path(), &target)?;
        } else {
            fs::copy(entry.path(), target)?;
        }
    }
    Ok(())
}

fn run_goml_with_goml_home(
    args: &[&str],
    cwd: &Path,
    home: &Path,
) -> anyhow::Result<std::process::Output> {
    let args = args.strip_prefix(&["compiler"]).unwrap_or(args);
    Ok(Command::new(gomlc_bin())
        .args(args)
        .current_dir(cwd)
        .env("GOML_HOME", home)
        .output()?)
}

#[test]
fn gomlc_run_single_executes_program() -> anyhow::Result<()> {
    if !runtime_executor_available() {
        return Ok(());
    }

    let (_dir, path) = write_program(HELLO_PROGRAM)?;

    let output = Command::new(gomlc_bin())
        .arg("run-single")
        .arg(&path)
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(output.status.success(), "stderr: {stderr}");
    expect!["hello\n"].assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    Ok(())
}

#[test]
fn gomlc_run_single_falls_back_when_yaegi_cannot_run_function_vectors() -> anyhow::Result<()> {
    if !runtime_executor_available() {
        return Ok(());
    }

    let (_dir, path) = write_program(FUNCTION_VECTOR_PROGRAM)?;

    let output = Command::new(gomlc_bin())
        .arg("run-single")
        .arg(&path)
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(output.status.success(), "stderr: {stderr}");
    expect!["11\n"].assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    Ok(())
}

#[test]
fn gomlc_loads_std_from_goml_home() -> anyhow::Result<()> {
    if !runtime_executor_available() {
        return Ok(());
    }

    let dir = tempfile::tempdir()?;
    let home = dir.path().join("goml-home");
    copy_dir_recursive(&workspace_root().join("stdlib/std"), &home.join("lib/std"))?;
    let path = dir.path().join("main.gom");
    fs::write(
        &path,
        r#"
use std::io;

fn main() -> unit {
    io::println("std-home")
}
"#,
    )?;

    let output = run_goml_with_goml_home(
        &["compiler", "run-single", path.to_string_lossy().as_ref()],
        dir.path(),
        &home,
    )?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(output.status.success(), "stderr: {stderr}");
    expect!["std-home\n"].assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    Ok(())
}

#[test]
fn gomlc_build_handles_deep_tuple_projection() -> anyhow::Result<()> {
    let input = workspace_root()
        .join("crates/compiler/src/tests/crashers/deep_tuple_projection_stack/main.gom");
    let dir = tempfile::tempdir()?;
    let output_path = dir.path().join("main");

    let output = Command::new(gomlc_bin())
        .arg("build")
        .arg("--package")
        .arg("main")
        .arg("--input")
        .arg(&input)
        .arg("--output")
        .arg(&output_path)
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        output.status.code(),
        Some(1),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stderr.contains("expression is too deeply nested"),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    expect![""].assert_eq(&stdout);

    Ok(())
}

#[test]
fn gomlc_build_handles_deep_tuple_type() -> anyhow::Result<()> {
    let ty = deep_left_nested_tuple_type(4000);
    let program = format!("fn take(x: {ty}) -> unit {{ () }}\nfn main() -> unit {{ () }}\n");
    let (_input_dir, input) = write_program(&program)?;
    let output_dir = tempfile::tempdir()?;
    let output_path = output_dir.path().join("main");

    let output = Command::new(gomlc_bin())
        .arg("build")
        .arg("--package")
        .arg("main")
        .arg("--input")
        .arg(&input)
        .arg("--output")
        .arg(&output_path)
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        output.status.code(),
        Some(1),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stderr.contains("type is too deeply nested"),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    expect![""].assert_eq(&stdout);

    Ok(())
}

#[test]
fn gomlc_build_rejects_reserved_array_wildcard_length() -> anyhow::Result<()> {
    let program = r#"
fn take(x: [int32; 18446744073709551615]) -> int32 {
    x[0]
}

fn main() -> unit {
    let a: [int32; 18446744073709551615] = [7i32, 8i32];
    println(take(a).to_string())
}
"#;
    let (_input_dir, input) = write_program(program)?;
    let output_dir = tempfile::tempdir()?;
    let output_path = output_dir.path().join("main");

    let output = Command::new(gomlc_bin())
        .arg("build")
        .arg("--package")
        .arg("main")
        .arg("--input")
        .arg(&input)
        .arg("--output")
        .arg(&output_path)
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        output.status.code(),
        Some(1),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stderr.contains("Invalid array length"),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    expect![""].assert_eq(&stdout);

    Ok(())
}

#[test]
fn gomlc_build_rejects_array_length_above_go_int() -> anyhow::Result<()> {
    let program = r#"
fn take(x: [int32; 18446744073709551614]) -> int32 {
    x[0]
}

fn main() -> unit {
    let dummy = [0i32];
    let a: [int32; 18446744073709551614] = [7i32, 8i32];
    println(take(a).to_string())
}
"#;
    let (_input_dir, input) = write_program(program)?;
    let output_dir = tempfile::tempdir()?;
    let output_path = output_dir.path().join("main");

    let output = Command::new(gomlc_bin())
        .arg("build")
        .arg("--package")
        .arg("main")
        .arg("--input")
        .arg(&input)
        .arg("--output")
        .arg(&output_path)
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        output.status.code(),
        Some(1),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stderr.contains("Invalid array length"),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    expect![""].assert_eq(&stdout);

    Ok(())
}

#[test]
fn gomlc_build_handles_wide_struct_pattern() -> anyhow::Result<()> {
    let program = wide_struct_pattern_program(1000);
    let (_input_dir, input) = write_program(&program)?;
    let output_dir = tempfile::tempdir()?;
    let output_path = output_dir.path().join("main");

    let output = Command::new(gomlc_bin())
        .arg("build")
        .arg("--package")
        .arg("main")
        .arg("--input")
        .arg(&input)
        .arg("--output")
        .arg(&output_path)
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "stdout: {stdout}\nstderr: {stderr}"
    );

    Ok(())
}

#[test]
fn gomlc_build_handles_very_wide_struct_pattern() -> anyhow::Result<()> {
    let program = wide_struct_pattern_program(2600);
    let (_input_dir, input) = write_program(&program)?;
    let output_dir = tempfile::tempdir()?;
    let output_path = output_dir.path().join("main");

    let output = Command::new(gomlc_bin())
        .arg("build")
        .arg("--package")
        .arg("main")
        .arg("--input")
        .arg(&input)
        .arg("--output")
        .arg(&output_path)
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "stdout: {stdout}\nstderr: {stderr}"
    );

    Ok(())
}

#[test]
fn gomlc_run_single_dumps_requested_stages() -> anyhow::Result<()> {
    if !runtime_executor_available() {
        return Ok(());
    }

    let (_dir, path) = write_program(HELLO_PROGRAM)?;

    let output = Command::new(gomlc_bin())
        .arg("run-single")
        .args([
            "--dump-ast",
            "--dump-hir",
            "--dump-tast",
            "--dump-core",
            "--dump-mono",
            "--dump-lift",
            "--dump-anf",
            "--dump-go",
        ])
        .arg(&path)
        .output()?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(output.status.success(), "stderr: {stderr}");
    expect![[r#"
        == AST ==
        package main;
        fn main() -> unit {
            println("hello")
        }



        == HIR ==
        package main
        file main.gom
          fn main() -> unit {
                println("hello")
            }

        == Typed AST ==
        fn main() -> unit {
            (println : (string) -> unit)("hello")
        }

        == Core ==
        fn print(value/0: T) -> unit {
          @runtime(core.string_print)(trait_call[ToString::to_string](value/0))
        }

        fn println(value/1: T) -> unit {
          @runtime(core.string_println)(trait_call[ToString::to_string](value/1))
        }

        fn inherent#int32#int32#to_string(self/2: int32) -> string {
          @runtime(core.int32_to_string)(self/2)
        }

        fn inherent#char#char#to_string(self/3: char) -> string {
          @runtime(core.char_to_string)(self/3)
        }

        fn inherent#string#string#len(self/4: string) -> int32 {
          @runtime(core.string_len)(self/4)
        }

        fn inherent#string#string#get(self/5: string, index/6: int32) -> char {
          @runtime(core.string_get)(self/5, index/6)
        }

        fn trait_impl#ToString#unit#to_string(self/7: unit) -> string {
          @runtime(core.unit_to_string)(self/7)
        }

        fn trait_impl#ToString#bool#to_string(self/8: bool) -> string {
          @runtime(core.bool_to_string)(self/8)
        }

        fn trait_impl#ToString#string#to_string(self/9: string) -> string {
          self/9
        }

        fn trait_impl#ToString#char#to_string(self/10: char) -> string {
          @runtime(core.char_to_string)(self/10)
        }

        fn trait_impl#ToString#int8#to_string(self/11: int8) -> string {
          @runtime(core.int8_to_string)(self/11)
        }

        fn trait_impl#ToString#int16#to_string(self/12: int16) -> string {
          @runtime(core.int16_to_string)(self/12)
        }

        fn trait_impl#ToString#int32#to_string(self/13: int32) -> string {
          @runtime(core.int32_to_string)(self/13)
        }

        fn trait_impl#ToString#int64#to_string(self/14: int64) -> string {
          @runtime(core.int64_to_string)(self/14)
        }

        fn trait_impl#ToString#uint8#to_string(self/15: uint8) -> string {
          @runtime(core.uint8_to_string)(self/15)
        }

        fn trait_impl#ToString#uint16#to_string(self/16: uint16) -> string {
          @runtime(core.uint16_to_string)(self/16)
        }

        fn trait_impl#ToString#uint32#to_string(self/17: uint32) -> string {
          @runtime(core.uint32_to_string)(self/17)
        }

        fn trait_impl#ToString#uint64#to_string(self/18: uint64) -> string {
          @runtime(core.uint64_to_string)(self/18)
        }

        fn trait_impl#ToString#float32#to_string(self/19: float32) -> string {
          @runtime(core.float32_to_string)(self/19)
        }

        fn trait_impl#ToString#float64#to_string(self/20: float64) -> string {
          @runtime(core.float64_to_string)(self/20)
        }

        fn trait_impl#Eq#unit#eq(self/21: unit, other/22: unit) -> bool {
          true
        }

        fn trait_impl#Eq#bool#eq(self/23: bool, other/24: bool) -> bool {
          (self/23 == other/24)
        }

        fn trait_impl#Eq#string#eq(self/25: string, other/26: string) -> bool {
          (self/25 == other/26)
        }

        fn trait_impl#Eq#char#eq(self/27: char, other/28: char) -> bool {
          (self/27 == other/28)
        }

        fn trait_impl#Eq#int8#eq(self/29: int8, other/30: int8) -> bool {
          (self/29 == other/30)
        }

        fn trait_impl#Eq#int16#eq(self/31: int16, other/32: int16) -> bool {
          (self/31 == other/32)
        }

        fn trait_impl#Eq#int32#eq(self/33: int32, other/34: int32) -> bool {
          (self/33 == other/34)
        }

        fn trait_impl#Eq#int64#eq(self/35: int64, other/36: int64) -> bool {
          (self/35 == other/36)
        }

        fn trait_impl#Eq#uint8#eq(self/37: uint8, other/38: uint8) -> bool {
          (self/37 == other/38)
        }

        fn trait_impl#Eq#uint16#eq(self/39: uint16, other/40: uint16) -> bool {
          (self/39 == other/40)
        }

        fn trait_impl#Eq#uint32#eq(self/41: uint32, other/42: uint32) -> bool {
          (self/41 == other/42)
        }

        fn trait_impl#Eq#uint64#eq(self/43: uint64, other/44: uint64) -> bool {
          (self/43 == other/44)
        }

        fn trait_impl#Eq#float32#eq(self/45: float32, other/46: float32) -> bool {
          (self/45 == other/46)
        }

        fn trait_impl#Eq#float64#eq(self/47: float64, other/48: float64) -> bool {
          (self/47 == other/48)
        }

        fn trait_impl#Hash#unit#hash(self/49: unit) -> uint64 {
          0
        }

        fn trait_impl#Hash#bool#hash(self/50: bool) -> uint64 {
          if self/50 {
              {
                1
              }
          } else {
              {
                0
              }
          }
        }

        fn trait_impl#Hash#string#hash(self/51: string) -> uint64 {
          @runtime(core.string_hash)(self/51)
        }

        fn trait_impl#Hash#char#hash(self/52: char) -> uint64 {
          @runtime(core.char_hash)(self/52)
        }

        fn trait_impl#Hash#int8#hash(self/53: int8) -> uint64 {
          @runtime(core.int8_hash)(self/53)
        }

        fn trait_impl#Hash#int16#hash(self/54: int16) -> uint64 {
          @runtime(core.int16_hash)(self/54)
        }

        fn trait_impl#Hash#int32#hash(self/55: int32) -> uint64 {
          @runtime(core.int32_hash)(self/55)
        }

        fn trait_impl#Hash#int64#hash(self/56: int64) -> uint64 {
          @runtime(core.int64_hash)(self/56)
        }

        fn trait_impl#Hash#uint8#hash(self/57: uint8) -> uint64 {
          @runtime(core.uint8_hash)(self/57)
        }

        fn trait_impl#Hash#uint16#hash(self/58: uint16) -> uint64 {
          @runtime(core.uint16_hash)(self/58)
        }

        fn trait_impl#Hash#uint32#hash(self/59: uint32) -> uint64 {
          @runtime(core.uint32_hash)(self/59)
        }

        fn trait_impl#Hash#uint64#hash(self/60: uint64) -> uint64 {
          self/60
        }

        fn trait_impl#Hash#float32#hash(self/61: float32) -> uint64 {
          @runtime(core.float32_hash)(self/61)
        }

        fn trait_impl#Hash#float64#hash(self/62: float64) -> uint64 {
          @runtime(core.float64_hash)(self/62)
        }

        fn trait_impl#Eq#Ref[T]#eq(self/63: Ref[T], other/64: Ref[T]) -> bool {
          let a/65 = inherent#Ref#Ref[T]#get(self/63) in
          let b/66 = inherent#Ref#Ref[T]#get(other/64) in
          trait_call[Eq::eq](a/65, b/66)
        }

        fn trait_impl#Hash#Ref[T]#hash(self/67: Ref[T]) -> uint64 {
          let v/68 = inherent#Ref#Ref[T]#get(self/67) in
          trait_call[Hash::hash](v/68)
        }

        fn trait_impl#ToString#Ref[T]#to_string(self/69: Ref[T]) -> string {
          let v/70 = inherent#Ref#Ref[T]#get(self/69) in
          (("ref(" + trait_call[ToString::to_string](v/70)) + ")")
        }

        fn inherent#Iterator#Iterator[T]#from_fn(next_fn/71: () -> Option[T]) -> Iterator[T] {
          Iterator { next_fn: next_fn/71 }
        }

        fn inherent#Iterator#Iterator[T]#next(self/72: Iterator[T]) -> Option[T] {
          Iterator.next_fn(self/72)()
        }

        fn inherent#Vec#Vec[T]#new() -> Vec[T] {
          @intrinsic(vec.new)()
        }

        fn inherent#Vec#Vec[T]#push(self/73: Vec[T], elem/74: T) -> unit {
          @intrinsic(vec.push)(self/73, elem/74)
        }

        fn inherent#Vec#Vec[T]#pushed(self/75: Vec[T], elem/76: T) -> Vec[T] {
          let result/77 = inherent#Vec#Vec[T]#new() in
          let index/78 = inherent#Ref#Ref[int32]#new(0) in
          let _wild2 = while (inherent#Ref#Ref[int32]#get(index/78) < inherent#Vec#Vec[T]#len(self/75)) {
              {
                let _wild0 = inherent#Vec#Vec[T]#push(result/77, inherent#Vec#Vec[T]#get(self/75, inherent#Ref#Ref[int32]#get(index/78))) in
                let _wild1 = inherent#Ref#Ref[int32]#set(index/78, (inherent#Ref#Ref[int32]#get(index/78) + 1)) in
              }
          } in
          let _wild3 = inherent#Vec#Vec[T]#push(result/77, elem/76) in
          result/77
        }

        fn inherent#Vec#Vec[T]#get(self/79: Vec[T], index/80: int32) -> T {
          @intrinsic(vec.get)(self/79, index/80)
        }

        fn inherent#Vec#Vec[T]#set(self/81: Vec[T], index/82: int32, elem/83: T) -> unit {
          @intrinsic(vec.set)(self/81, index/82, elem/83)
        }

        fn inherent#Vec#Vec[T]#len(self/84: Vec[T]) -> int32 {
          @intrinsic(vec.len)(self/84)
        }

        fn inherent#Vec#Vec[T]#slice(self/85: Vec[T], start/86: int32, end/87: int32) -> Slice[T] {
          @intrinsic(slice.new)(self/85, start/86, end/87)
        }

        fn inherent#Vec#Vec[T]#iter(self/88: Vec[T]) -> Iterator[T] {
          let index/89 = inherent#Ref#Ref[int32]#new(0) in
          let len/90 = inherent#Vec#Vec[T]#len(self/88) in
          inherent#Iterator#Iterator#from_fn(|| => {
            let current/91 = inherent#Ref#Ref[int32]#get(index/89) in
            if (current/91 < len/90) {
                {
                  let value/92 = inherent#Vec#Vec[T]#get(self/88, current/91) in
                  let _wild4 = inherent#Ref#Ref[int32]#set(index/89, (current/91 + 1)) in
                  Option::Some(value/92)
                }
            } else {
                {
                  Option::None
                }
            }
          })
        }

        fn inherent#Slice#Slice[T]#get(self/93: Slice[T], index/94: int32) -> T {
          @intrinsic(slice.get)(self/93, index/94)
        }

        fn inherent#Slice#Slice[T]#len(self/95: Slice[T]) -> int32 {
          @intrinsic(slice.len)(self/95)
        }

        fn inherent#Slice#Slice[T]#sub(self/96: Slice[T], start/97: int32, end/98: int32) -> Slice[T] {
          @intrinsic(slice.sub)(self/96, start/97, end/98)
        }

        fn inherent#Slice#Slice[T]#iter(self/99: Slice[T]) -> Iterator[T] {
          let index/100 = inherent#Ref#Ref[int32]#new(0) in
          let len/101 = inherent#Slice#Slice[T]#len(self/99) in
          inherent#Iterator#Iterator#from_fn(|| => {
            let current/102 = inherent#Ref#Ref[int32]#get(index/100) in
            if (current/102 < len/101) {
                {
                  let value/103 = inherent#Slice#Slice[T]#get(self/99, current/102) in
                  let _wild5 = inherent#Ref#Ref[int32]#set(index/100, (current/102 + 1)) in
                  Option::Some(value/103)
                }
            } else {
                {
                  Option::None
                }
            }
          })
        }

        fn inherent#HashMap#HashMap[K,V]#new() -> HashMap[K, V] {
          @intrinsic(hashmap.new)()
        }

        fn inherent#HashMap#HashMap[K,V]#get(self/104: HashMap[K, V], key/105: K) -> Option[V] {
          @intrinsic(hashmap.get)(self/104, key/105)
        }

        fn inherent#HashMap#HashMap[K,V]#set(self/106: HashMap[K, V], key/107: K, value/108: V) -> unit {
          @intrinsic(hashmap.set)(self/106, key/107, value/108)
        }

        fn inherent#HashMap#HashMap[K,V]#remove(self/109: HashMap[K, V], key/110: K) -> unit {
          @intrinsic(hashmap.remove)(self/109, key/110)
        }

        fn inherent#HashMap#HashMap[K,V]#len(self/111: HashMap[K, V]) -> int32 {
          @intrinsic(hashmap.len)(self/111)
        }

        fn inherent#HashMap#HashMap[K,V]#contains(self/112: HashMap[K, V], key/113: K) -> bool {
          @intrinsic(hashmap.contains)(self/112, key/113)
        }

        fn inherent#Ref#Ref[T]#new(value/114: T) -> Ref[T] {
          @intrinsic(ref.new)(value/114)
        }

        fn inherent#Ref#Ref[T]#get(self/115: Ref[T]) -> T {
          @intrinsic(ref.get)(self/115)
        }

        fn inherent#Ref#Ref[T]#set(self/116: Ref[T], value/117: T) -> unit {
          @intrinsic(ref.set)(self/116, value/117)
        }

        fn range(start/118: int32, end/119: int32) -> Iterator[int32] {
          let current/120 = inherent#Ref#Ref[int32]#new(start/118) in
          inherent#Iterator#Iterator#from_fn(|| => {
            let value/121 = inherent#Ref#Ref[int32]#get(current/120) in
            if (value/121 < end/119) {
                {
                  let _wild6 = inherent#Ref#Ref[int32]#set(current/120, (value/121 + 1)) in
                  Option::Some(value/121)
                }
            } else {
                {
                  Option::None
                }
            }
          })
        }

        fn main() -> unit {
          println("hello")
        }

        == Mono ==
        fn main() -> unit {
          println__T_string("hello")
        }

        fn println__T_string(value/1: string) -> unit {
          @runtime(core.string_println)(trait_impl#ToString#string#to_string(value/1))
        }

        fn trait_impl#ToString#string#to_string(self/9: string) -> string {
          self/9
        }

        == Lifted ==
        fn main() -> unit {
          println__T_string("hello")
        }

        fn println__T_string(value/1: string) -> unit {
          @runtime(core.string_println)(trait_impl#ToString#string#to_string(value/1))
        }

        fn trait_impl#ToString#string#to_string(self/9: string) -> string {
          self/9
        }

        == ANF ==
        fn main() -> unit {
          join ret7() -> unit {
            ()
          } in
          let t8 = println__T_string("hello") in
          jump ret7()
        }

        fn println__T_string(value/1: string) -> unit {
          join ret9() -> unit {
            ()
          } in
          let t10 = trait_impl#ToString#string#to_string(value/1) in
          let t11 = @runtime(core.string_println)(t10) in
          jump ret9()
        }

        fn trait_impl#ToString#string#to_string(self/9: string) -> string {
          join ret12(retv13: string) -> string {
            retv13
          } in
          jump ret12(self/9)
        }

        == Go ==
        package main

        import (
            _goml_fmt "fmt"
        )

        func _goml_runtime_core_string_println(s string) struct{} {
            _goml_fmt.Println(s)
            return struct{}{}
        }

        func main0() struct{} {
            println__T_string("hello")
            return struct{}{}
        }

        func println__T_string(value__1 string) struct{} {
            var t10 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
            _goml_runtime_core_string_println(t10)
            return struct{}{}
        }

        func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
            var retv13 string
            retv13 = self__9
            return retv13
        }

        func main() {
            main0()
        }

        hello
    "#]]
    .assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    Ok(())
}

#[test]
fn version_reports_text_and_protocol() -> anyhow::Result<()> {
    let text = Command::new(gomlc_bin()).arg("version").output()?;
    assert!(text.status.success());
    assert!(String::from_utf8_lossy(&text.stdout).starts_with("gomlc 0.0.0"));

    let json = Command::new(gomlc_bin())
        .args(["version", "--format", "json"])
        .output()?;
    assert!(json.status.success());
    let value: serde_json::Value = serde_json::from_slice(&json.stdout)?;
    assert_eq!(value["tool"], "gomlc");
    assert_eq!(value["driver_protocol"], 1);
    assert_eq!(value["artifact_format"], compiler::artifact::FORMAT_VERSION);
    assert_eq!(value["compiler_abi"], compiler::artifact::COMPILER_ABI);
    Ok(())
}
