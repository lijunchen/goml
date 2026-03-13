const n = `extern "go" "time" "Duration" type Duration

#[go_error_last]
extern "go" "time" "ParseDuration" parse_duration(s: string) -> Result[Duration, GoError]

#[go_error]
extern "go" "os" "Setenv" setenv(key: string, value: string) -> Result[unit, GoError]

extern "go" "fmt" "Sprintf" format_duration(format: string, value: Duration) -> string

fn describe_duration(input: string) -> string {
    match parse_duration(input) {
        Result::Ok(value) => format_duration("ok=%v", value),
        Result::Err(err) => "err=" + err.to_string(),
    }
}

fn describe_setenv() -> string {
    match setenv("GOML_FFI_TEST", "ok") {
        Result::Ok(()) => "setenv=ok",
        Result::Err(err) => "setenv_err=" + err.to_string(),
    }
}

fn main() -> unit {
    println(describe_duration("1.5s"));
    println(describe_duration("bad"));
    println(describe_setenv());
}
`;
export {
  n as default
};
