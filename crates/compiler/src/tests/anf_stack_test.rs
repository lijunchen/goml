use std::path::PathBuf;

use crate::pipeline::pipeline::compile_single_file;

fn wide_call_source(arg_count: usize) -> String {
    let params = (0..arg_count)
        .map(|idx| format!("x{}: int32", idx))
        .collect::<Vec<_>>()
        .join(", ");
    let args = std::iter::repeat_n("1i32", arg_count)
        .collect::<Vec<_>>()
        .join(", ");

    format!(
        "fn first({params}) -> int32 {{ x0 }}\nfn main() -> unit {{ println(first({args}).to_string()) }}\n"
    )
}

fn wide_struct_source(field_count: usize) -> String {
    let fields = (0..field_count)
        .map(|idx| format!("f{}: int32", idx))
        .collect::<Vec<_>>()
        .join(", ");
    let values = (0..field_count)
        .map(|idx| format!("f{}: 1i32", idx))
        .collect::<Vec<_>>()
        .join(", ");

    format!(
        "struct S {{ {fields} }}\nfn main() -> unit {{ let s = S {{ {values} }}; println(s.f0.to_string()) }}\n"
    )
}

fn wide_complex_call_source(arg_count: usize) -> String {
    let params = (0..arg_count)
        .map(|idx| format!("x{}: int32", idx))
        .collect::<Vec<_>>()
        .join(", ");
    let args = std::iter::repeat_n("id(1i32)", arg_count)
        .collect::<Vec<_>>()
        .join(", ");

    format!(
        "fn id(x: int32) -> int32 {{ x }}\nfn first({params}) -> int32 {{ x0 }}\nfn main() -> unit {{ println(first({args}).to_string()) }}\n"
    )
}

fn wide_match_call_source(arg_count: usize) -> String {
    let params = (0..arg_count)
        .map(|idx| format!("x{}: int32", idx))
        .collect::<Vec<_>>()
        .join(", ");
    let args = std::iter::repeat_n("match 0i32 { 0i32 => 1i32, _ => 2i32 }", arg_count)
        .collect::<Vec<_>>()
        .join(", ");

    format!(
        "fn first({params}) -> int32 {{ x0 }}\nfn main() -> unit {{ println(first({args}).to_string()) }}\n"
    )
}

fn wide_or_pattern_source(alternative_count: usize) -> String {
    let alternatives = (0..alternative_count)
        .map(|index| {
            if index % 2 == 0 {
                "(true, true)"
            } else {
                "(false, false)"
            }
        })
        .collect::<Vec<_>>()
        .join(" | ");

    format!(
        "fn classify(value: (bool, bool)) -> int32 {{ match value {{ {alternatives} => 1, _ => 0 }} }}\nfn main() -> unit {{ println(classify((true, true)).to_string()) }}\n"
    )
}

#[test]
fn wide_call_argument_list_compiles_without_crashing_anf() {
    let src = wide_call_source(1500);
    let path = PathBuf::from("wide_call_argument_list.gom");

    compile_single_file(&path, &src).unwrap();
}

#[test]
fn wide_struct_literal_compiles_without_crashing_anf() {
    let src = wide_struct_source(1500);
    let path = PathBuf::from("wide_struct_literal.gom");

    compile_single_file(&path, &src).unwrap();
}

#[test]
fn wide_complex_call_argument_list_compiles_without_crashing_anf() {
    let src = wide_complex_call_source(500);
    let path = PathBuf::from("wide_complex_call_argument_list.gom");

    compile_single_file(&path, &src).unwrap();
}

#[test]
fn wide_match_call_argument_list_compiles_without_crashing_anf() {
    let src = wide_match_call_source(600);
    let path = PathBuf::from("wide_match_call_argument_list.gom");

    compile_single_file(&path, &src).unwrap();
}

#[test]
fn very_wide_match_call_argument_list_compiles_without_crashing_anf() {
    let src = wide_match_call_source(2000);
    let path = PathBuf::from("very_wide_match_call_argument_list.gom");

    compile_single_file(&path, &src).unwrap();
}

#[test]
fn wide_or_pattern_compiles_without_exponential_growth() {
    let src = wide_or_pattern_source(24);
    let path = PathBuf::from("wide_or_pattern.gom");

    compile_single_file(&path, &src).unwrap();
}
