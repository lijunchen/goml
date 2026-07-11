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

        fn inherent#FnIterator#FnIterator[T]#from_fn(next_fn/71: () -> Option[T]) -> FnIterator[T] {
          FnIterator { next_fn: next_fn/71 }
        }

        fn trait_impl#Iterator#[T]@FnIterator[T]#next(self/72: FnIterator[T]) -> Option[T] {
          FnIterator.next_fn(self/72)()
        }

        fn trait_impl#Iterator#[B]@MapIterator[A,B,I]#next(self/73: MapIterator[A, B, I]) -> Option[B] {
          {
            let mtmp0 = trait_call[Iterator[A]::next](MapIterator.iterator(self/73)) in
            match mtmp0 {
              Option::None => {
                Option::None
              },
              Option::Some(x1) => {
                {
                  let x1 = Option::Some._0(mtmp0) in
                  {
                    let value/74 = x1 in
                    Option::Some(MapIterator.map_fn(self/73)(value/74))
                  }
                }
              },
            }
          }
        }

        fn trait_impl#Iterator#[T]@FilterIterator[T,I]#next(self/75: FilterIterator[T, I]) -> Option[T] {
          let _wild5 = while true {
              {
                let _wild4 = {
                  let mtmp2 = trait_call[Iterator[T]::next](FilterIterator.iterator(self/75)) in
                  match mtmp2 {
                    Option::None => {
                      return Option::None
                    },
                    Option::Some(x3) => {
                      {
                        let x3 = Option::Some._0(mtmp2) in
                        {
                          let value/76 = x3 in
                          if FilterIterator.predicate(self/75)(value/76) {
                              {
                                return Option::Some(value/76)
                              }
                          } else {
                              {
                                ()
                              }
                          }
                        }
                      }
                    },
                  }
                } in
              }
          } in
          Option::None
        }

        fn trait_impl#Iterator#[T]@TakeIterator[I]#next(self/77: TakeIterator[I]) -> Option[T] {
          let remaining/78 = inherent#Ref#Ref[int32]#get(TakeIterator.remaining(self/77)) in
          if (remaining/78 > 0) {
              {
                let _wild6 = inherent#Ref#Ref[int32]#set(TakeIterator.remaining(self/77), (remaining/78 - 1)) in
                trait_call[Iterator[T]::next](TakeIterator.iterator(self/77))
              }
          } else {
              {
                Option::None
              }
          }
        }

        fn iterator_map(iterator/79: I, map_fn/80: (A) -> B) -> MapIterator[A, B, I] {
          MapIterator { iterator: iterator/79, map_fn: map_fn/80 }
        }

        fn iterator_filter(iterator/81: I, predicate/82: (T) -> bool) -> FilterIterator[T, I] {
          FilterIterator { iterator: iterator/81, predicate: predicate/82 }
        }

        fn iterator_take(iterator/83: I, count/84: int32) -> TakeIterator[I] {
          let remaining/85 = if (count/84 > 0) {
              {
                count/84
              }
          } else {
              {
                0
              }
          } in
          TakeIterator { iterator: iterator/83, remaining: inherent#Ref#Ref[int32]#new(remaining/85) }
        }

        fn iterator_fold(iterator/86: I, initial/87: A, combine/88: (A, T) -> A) -> A {
          let accumulator/89 = inherent#Ref#Ref[A]#new(initial/87) in
          let running/90 = inherent#Ref#Ref[bool]#new(true) in
          let _wild10 = while inherent#Ref#Ref[bool]#get(running/90) {
              {
                let _wild9 = {
                  let mtmp7 = trait_call[Iterator[T]::next](iterator/86) in
                  match mtmp7 {
                    Option::None => {
                      inherent#Ref#Ref[bool]#set(running/90, false)
                    },
                    Option::Some(x8) => {
                      {
                        let x8 = Option::Some._0(mtmp7) in
                        {
                          let value/91 = x8 in
                          inherent#Ref#Ref[A]#set(accumulator/89, combine/88(inherent#Ref#Ref[A]#get(accumulator/89), value/91))
                        }
                      }
                    },
                  }
                } in
              }
          } in
          inherent#Ref#Ref[A]#get(accumulator/89)
        }

        fn iterator_collect(iterator/92: I) -> Vec[T] {
          let values/93 = inherent#Vec#Vec[T]#new() in
          let running/94 = inherent#Ref#Ref[bool]#new(true) in
          let _wild14 = while inherent#Ref#Ref[bool]#get(running/94) {
              {
                let _wild13 = {
                  let mtmp11 = trait_call[Iterator[T]::next](iterator/92) in
                  match mtmp11 {
                    Option::None => {
                      inherent#Ref#Ref[bool]#set(running/94, false)
                    },
                    Option::Some(x12) => {
                      {
                        let x12 = Option::Some._0(mtmp11) in
                        {
                          let value/95 = x12 in
                          inherent#Vec#Vec[T]#push(values/93, value/95)
                        }
                      }
                    },
                  }
                } in
              }
          } in
          values/93
        }

        fn inherent#Vec#Vec[T]#new() -> Vec[T] {
          @intrinsic(vec.new)()
        }

        fn inherent#Vec#Vec[T]#push(self/96: Vec[T], elem/97: T) -> unit {
          @intrinsic(vec.push)(self/96, elem/97)
        }

        fn inherent#Vec#Vec[T]#pushed(self/98: Vec[T], elem/99: T) -> Vec[T] {
          let result/100 = inherent#Vec#Vec[T]#new() in
          let index/101 = inherent#Ref#Ref[int32]#new(0) in
          let _wild17 = while (inherent#Ref#Ref[int32]#get(index/101) < inherent#Vec#Vec[T]#len(self/98)) {
              {
                let _wild15 = inherent#Vec#Vec[T]#push(result/100, inherent#Vec#Vec[T]#get(self/98, inherent#Ref#Ref[int32]#get(index/101))) in
                let _wild16 = inherent#Ref#Ref[int32]#set(index/101, (inherent#Ref#Ref[int32]#get(index/101) + 1)) in
              }
          } in
          let _wild18 = inherent#Vec#Vec[T]#push(result/100, elem/99) in
          result/100
        }

        fn inherent#Vec#Vec[T]#get(self/102: Vec[T], index/103: int32) -> T {
          @intrinsic(vec.get)(self/102, index/103)
        }

        fn inherent#Vec#Vec[T]#set(self/104: Vec[T], index/105: int32, elem/106: T) -> unit {
          @intrinsic(vec.set)(self/104, index/105, elem/106)
        }

        fn inherent#Vec#Vec[T]#len(self/107: Vec[T]) -> int32 {
          @intrinsic(vec.len)(self/107)
        }

        fn inherent#Vec#Vec[T]#slice(self/108: Vec[T], start/109: int32, end/110: int32) -> Slice[T] {
          @intrinsic(slice.new)(self/108, start/109, end/110)
        }

        fn inherent#Vec#Vec[T]#iter(self/111: Vec[T]) -> FnIterator[T] {
          let index/112 = inherent#Ref#Ref[int32]#new(0) in
          let len/113 = inherent#Vec#Vec[T]#len(self/111) in
          inherent#FnIterator#FnIterator#from_fn(|| => {
            let current/114 = inherent#Ref#Ref[int32]#get(index/112) in
            if (current/114 < len/113) {
                {
                  let value/115 = inherent#Vec#Vec[T]#get(self/111, current/114) in
                  let _wild19 = inherent#Ref#Ref[int32]#set(index/112, (current/114 + 1)) in
                  Option::Some(value/115)
                }
            } else {
                {
                  Option::None
                }
            }
          })
        }

        fn inherent#Slice#Slice[T]#get(self/116: Slice[T], index/117: int32) -> T {
          @intrinsic(slice.get)(self/116, index/117)
        }

        fn inherent#Slice#Slice[T]#len(self/118: Slice[T]) -> int32 {
          @intrinsic(slice.len)(self/118)
        }

        fn inherent#Slice#Slice[T]#sub(self/119: Slice[T], start/120: int32, end/121: int32) -> Slice[T] {
          @intrinsic(slice.sub)(self/119, start/120, end/121)
        }

        fn inherent#Slice#Slice[T]#iter(self/122: Slice[T]) -> FnIterator[T] {
          let index/123 = inherent#Ref#Ref[int32]#new(0) in
          let len/124 = inherent#Slice#Slice[T]#len(self/122) in
          inherent#FnIterator#FnIterator#from_fn(|| => {
            let current/125 = inherent#Ref#Ref[int32]#get(index/123) in
            if (current/125 < len/124) {
                {
                  let value/126 = inherent#Slice#Slice[T]#get(self/122, current/125) in
                  let _wild20 = inherent#Ref#Ref[int32]#set(index/123, (current/125 + 1)) in
                  Option::Some(value/126)
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

        fn inherent#HashMap#HashMap[K,V]#get(self/127: HashMap[K, V], key/128: K) -> Option[V] {
          @intrinsic(hashmap.get)(self/127, key/128)
        }

        fn inherent#HashMap#HashMap[K,V]#set(self/129: HashMap[K, V], key/130: K, value/131: V) -> unit {
          @intrinsic(hashmap.set)(self/129, key/130, value/131)
        }

        fn inherent#HashMap#HashMap[K,V]#remove(self/132: HashMap[K, V], key/133: K) -> unit {
          @intrinsic(hashmap.remove)(self/132, key/133)
        }

        fn inherent#HashMap#HashMap[K,V]#len(self/134: HashMap[K, V]) -> int32 {
          @intrinsic(hashmap.len)(self/134)
        }

        fn inherent#HashMap#HashMap[K,V]#contains(self/135: HashMap[K, V], key/136: K) -> bool {
          @intrinsic(hashmap.contains)(self/135, key/136)
        }

        fn inherent#Ref#Ref[T]#new(value/137: T) -> Ref[T] {
          @intrinsic(ref.new)(value/137)
        }

        fn inherent#Ref#Ref[T]#get(self/138: Ref[T]) -> T {
          @intrinsic(ref.get)(self/138)
        }

        fn inherent#Ref#Ref[T]#set(self/139: Ref[T], value/140: T) -> unit {
          @intrinsic(ref.set)(self/139, value/140)
        }

        fn range(start/141: int32, end/142: int32) -> FnIterator[int32] {
          let current/143 = inherent#Ref#Ref[int32]#new(start/141) in
          inherent#FnIterator#FnIterator#from_fn(|| => {
            let value/144 = inherent#Ref#Ref[int32]#get(current/143) in
            if (value/144 < end/142) {
                {
                  let _wild21 = inherent#Ref#Ref[int32]#set(current/143, (value/144 + 1)) in
                  Option::Some(value/144)
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
          join ret22() -> unit {
            ()
          } in
          let t23 = println__T_string("hello") in
          jump ret22()
        }

        fn println__T_string(value/1: string) -> unit {
          join ret24() -> unit {
            ()
          } in
          let t25 = trait_impl#ToString#string#to_string(value/1) in
          let t26 = @runtime(core.string_println)(t25) in
          jump ret24()
        }

        fn trait_impl#ToString#string#to_string(self/9: string) -> string {
          join ret27(retv28: string) -> string {
            retv28
          } in
          jump ret27(self/9)
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
            var t25 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
            _goml_runtime_core_string_println(t25)
            return struct{}{}
        }

        func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
            var retv28 string
            retv28 = self__9
            return retv28
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
