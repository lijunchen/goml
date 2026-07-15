const e = `fn main() {
    let sum = Ref::new(0i32);
    let i = Ref::new(1i32);
    while i.get() <= 100i32 {
        if i.get() == 50i32 {
            break
        } else {
            ()
        };
        sum.set(sum.get() + i.get());
        i.set(i.get() + 1i32);
    };
    let _ = print("sum up to break: ");
    let _ = println(sum.get());

    let even_sum = Ref::new(0i32);
    let j = Ref::new(1i32);
    while j.get() <= 10i32 {
        let cur = j.get();
        j.set(cur + 1i32);
        if cur == (cur / 2i32) * 2i32 {
            even_sum.set(even_sum.get() + cur);
            continue
        } else {
            ()
        };
    };
    let _ = print("even sum: ");
    let _ = println(even_sum.get());
}
`;
export {
  e as default
};
