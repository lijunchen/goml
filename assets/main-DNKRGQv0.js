const n = `#[go_option_last]
extern "go" "strings" "Cut" cut_pair(text: string, sep: string) -> Option[(string, string)]

fn touch(text: string) -> Option[string] {
    let _ = cut_pair(text, ":")?;
    Option::Some("ok")
}

fn show(value: Option[string]) -> string {
    match value {
        Option::Some(text) => text,
        Option::None => "none",
    }
}

fn main() {
    println(show(touch("left:right")));
    println(show(touch("plain")));
}
`;
export {
  n as default
};
