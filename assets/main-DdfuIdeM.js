const n = `fn bump(counter: Ref[int32], ok: bool) -> Result[int32, string] {
    ref_set(counter, ref_get(counter) + 1i32);
    if ok {
        Result::Ok(ref_get(counter))
    } else {
        Result::Err("bump failed")
    }
}

fn use_try(counter: Ref[int32], ok: bool) -> Result[int32, string] {
    let value = bump(counter, ok)?;
    Result::Ok(value + ref_get(counter))
}

fn show(res: Result[int32, string]) -> string {
    match res {
        Result::Ok(value) => "ok " + value.to_string(),
        Result::Err(err) => "err " + err,
    }
}

fn run(ok: bool) -> string {
    let counter = ref(0i32);
    let result = use_try(counter, ok);
    show(result) + " count=" + ref_get(counter).to_string()
}

fn main() -> unit {
    println(run(true));
    println(run(false));
}
`;
export {
  n as default
};
