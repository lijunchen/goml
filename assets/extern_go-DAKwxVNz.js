const n = `extern "go" "strings" contains(haystack: string, needle: string) -> bool

fn main() -> string {
    if contains("goml", "go") {
        "yes"
    } else {
        "no"
    }
}
`;
export {
  n as default
};
