const n = `extern "go" "time" "Time" type Moment
extern "go" "time" "Duration" type Span

extern "go" "time" "Unix" unix(secs: int32, nanos: int32) -> Moment
extern "go" "time" "Duration" nanos(nanos: int32) -> Span
extern "go" "time" "Time.Add" add_moment(base: Moment, delta: Span) -> Moment
extern "go" "time" "Time.Format" format_moment(value: Moment, layout: string) -> string

fn describe() -> string {
    let base = unix(1740823200, 0);
    let shifted = add_moment(base, nanos(90 * 1000000000));
    format_moment(shifted, "15:04:05")
}

fn main() -> unit {
    println(describe())
}
`;
export {
  n as default
};
