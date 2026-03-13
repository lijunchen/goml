const n = `extern "go" "time" "Duration" type Duration

#[go_error_last]
extern "go" "time" "ParseDuration" parse_duration(s: string) -> Result[Duration, GoError]

#[go_error]
extern "go" "os" "Setenv" setenv(key: string, value: string) -> Result[unit, GoError]

extern "go" "fmt" "Sprintf" format_duration(format: string, value: Duration) -> string

fn configure_and_format(input: string) -> Result[string, GoError] {
    setenv("GOML_TRY_FFI", "ok")?;
    let value = parse_duration(input)?;
    Result::Ok(format_duration("dur=%v", value))
}

fn show(res: Result[string, GoError]) -> string {
    match res {
        Result::Ok(value) => value,
        Result::Err(err) => "err=" + err.to_string(),
    }
}

fn main() -> unit {
    println(show(configure_and_format("2s")));
    println(show(configure_and_format("bad")));
}
`;
export {
  n as default
};
