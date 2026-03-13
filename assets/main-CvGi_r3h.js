const n = `extern "go" "time" "Duration" type Duration

#[go_error_last]
extern "go" "time" "ParseDuration" parse_duration(s: string) -> Result[Duration, GoError]

extern "go" "fmt" "Sprintf" format_duration(format: string, value: Duration) -> string

enum Mode {
    Left,
    Keep,
    Right,
}

fn pick(mode: Mode, input: string) -> Result[string, GoError] {
    let value = match mode {
        Mode::Left => {
            let parsed = parse_duration(input)?;
            "left=" + format_duration("%v", parsed)
        },
        Mode::Keep => "keep",
        Mode::Right => {
            let parsed = parse_duration(input)?;
            "right=" + format_duration("%v", parsed)
        },
    };
    Result::Ok(value)
}

fn show(res: Result[string, GoError]) -> string {
    match res {
        Result::Ok(value) => value,
        Result::Err(err) => "err=" + err.to_string(),
    }
}

fn main() -> unit {
    println(show(pick(Mode::Left, "4s")));
    println(show(pick(Mode::Right, "bad")));
    println(show(pick(Mode::Keep, "ignored")));
}
`;
export {
  n as default
};
