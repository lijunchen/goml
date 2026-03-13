const r = `extern "go" "io" "Reader" type Reader

#[go_error_last]
extern "go" "os" "Open" open_reader(path: string) -> Result[Reader, GoError]

extern "go" "fmt" "Sprintf" format_reader(format: string, value: Reader) -> string

fn describe(path: string) -> string {
    match open_reader(path) {
        Result::Ok(reader) => format_reader("type=%T", reader),
        Result::Err(err) => "err=" + err.to_string(),
    }
}

fn main() -> unit {
    println(describe("/etc/hosts"));
}
`;
export {
  r as default
};
