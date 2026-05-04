const e = `fn main() -> unit {
    let i = ref(0i32);
    let total = ref(0i32);

    while if ref_get(i) == 0i32 {
        ref_set(i, 1i32);
        true
    } else if ref_get(i) < 4i32 {
        true
    } else {
        false
    } {
        ref_set(total, ref_get(total) + ref_get(i));
        if ref_get(i) == 1i32 {
            ref_set(i, 2i32);
            continue
        } else {
            ()
        };
        ref_set(i, ref_get(i) + 1i32);
    };
    println(ref_get(total));

    let j = ref(0i32);
    let total2 = ref(0i32);
    while match ref_get(j) {
        0i32 => {
            ref_set(j, 1i32);
            true
        },
        1i32 => {
            ref_set(j, 2i32);
            true
        },
        2i32 => true,
        _ => false,
    } {
        ref_set(total2, ref_get(total2) + ref_get(j));
        if ref_get(j) == 2i32 {
            break;
        } else {
            ()
        };
    };
    println(ref_get(total2));
}
`;
export {
  e as default
};
