const n = `fn step(i: int32) -> Option[int32] {
    if i == 2 {
        Option::None
    } else {
        Option::Some(i + 10)
    }
}

fn accumulate(limit: int32) -> Option[int32] {
    let sum = Ref::new(0i32);
    let i = Ref::new(0i32);
    while i.get() < limit {
        let cur = i.get();
        i.set(cur + 1i32);
        if cur == 1 {
            continue
        } else {
            ()
        };
        let value = step(cur)?;
        sum.set(sum.get() + value);
    };
    Option::Some(sum.get())
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
