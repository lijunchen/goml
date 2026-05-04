const n = `fn step(i: int32) -> Option[int32] {
    if i == 2 {
        Option::None
    } else {
        Option::Some(i + 10)
    }
}

fn accumulate(limit: int32) -> Option[int32] {
    let sum = ref(0i32);
    let i = ref(0i32);
    while ref_get(i) < limit {
        let cur = ref_get(i);
        ref_set(i, cur + 1i32);
        if cur == 1 {
            continue
        } else {
            ()
        };
        let value = step(cur)?;
        ref_set(sum, ref_get(sum) + value);
    };
    Option::Some(ref_get(sum))
}

fn show(opt: Option[int32]) -> string {
    match opt {
        Option::Some(value) => "some=" + value.to_string(),
        Option::None => "none",
    }
}

fn main() -> unit {
    println(show(accumulate(2)));
    println(show(accumulate(4)));
}
`;
export {
  n as default
};
