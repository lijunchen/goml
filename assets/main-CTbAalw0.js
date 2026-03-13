const n = `#[go_option_last]
extern "go" "strings" "Cut" cut_pair(text: string, sep: string) -> Option[(string, string)]

fn pair(text: string) -> Option[(string, string)] {
    cut_pair(text, ":")
}

fn describe(text: string) -> string {
    match pair(text) {
        Option::Some((before, after)) => before + "|" + after,
        Option::None => "missing",
    }
}

fn main() -> unit {
    println(describe("alpha:beta"));
    println(describe("plain"));
}
`;
export {
  n as default
};
