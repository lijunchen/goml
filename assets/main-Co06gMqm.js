const n = `#[go_option_last]
extern "go" "strings" "CutPrefix" cut_prefix(s: string, prefix: string) -> Option[string]

fn describe(input: string, prefix: string) -> string {
    match cut_prefix(input, prefix) {
        Option::Some(rest) => "some=" + rest,
        Option::None => "none",
    }
}

fn main() -> unit {
    println(describe("goml", "go"));
    println(describe("goml", "ml"));
}
`;
export {
  n as default
};
