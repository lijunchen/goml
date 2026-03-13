const n = `#[go_option_last]
extern "go" "strings" "CutPrefix" cut_prefix(s: string, prefix: string) -> Option[string]

fn trim_go(input: string) -> Option[string] {
    Option::Some(cut_prefix(input, "go")? + "!")
}

fn show(opt: Option[string]) -> string {
    match opt {
        Option::Some(value) => "some=" + value,
        Option::None => "none",
    }
}

fn main() -> unit {
    println(show(trim_go("goml")));
    println(show(trim_go("ml")));
}
`;
export {
  n as default
};
