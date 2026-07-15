const n = `fn parse(flag: bool) -> Result[int32, string] {
    if flag {
        Result::Ok(5)
    } else {
        Result::Err("bad-branch")
    }
}

fn bump(flag: bool, fallback: bool) -> Result[int32, string] {
    let value = if flag {
        parse(fallback)?
    } else {
        10
    };
    Result::Ok(value + 1)
}

fn show(res: Result[int32, string]) -> string {
    match res {
        Result::Ok(value) => "ok=" + value.to_string(),
        Result::Err(err) => "err=" + err,
    }
}

fn main() -> unit {
    println(show(bump(true, true)));
    println(show(bump(true, false)));
    println(show(bump(false, false)));
}
`;
export {
  n as default
};
