const n = `extern "go" "" "int" type GoInt

#[go_error_last]
extern "go" "fmt" "Print" raw_print(text: string) -> Result[GoInt, GoError]

fn touch(text: string) -> Result[unit, GoError] {
    let _ = raw_print(text)?;
    Result::Ok(())
}

fn show(res: Result[unit, GoError]) -> string {
    match res {
        Result::Ok(()) => "ok",
        Result::Err(err) => "err=" + err.to_string(),
    }
}

fn main() {
    println(show(touch("hello")));
}
`;
export {
  n as default
};
