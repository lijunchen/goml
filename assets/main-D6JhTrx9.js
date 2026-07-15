const n = `fn early(x: int32) -> int32 {
    if x < 0i32 {
        return 0i32
    } else {
        ()
    };
    if x == 0i32 {
        return 1i32
    } else {
        ()
    };
    x + 2i32
}

fn closure_early(x: int32) -> int32 {
    let f = |y: int32| {
        if y > 5i32 {
            return y
        } else {
            ()
        };
        y + 10i32
    };
    f(x)
}

fn unit_ret(flag: bool) {
    if flag {
        return
    } else {
        ()
    };
    let _ = println("after");
}

fn main() {
    let _ = print("e-1: ");
    let _ = println(early(-1i32));
    let _ = print("e0: ");
    let _ = println(early(0i32));
    let _ = print("e3: ");
    let _ = println(early(3i32));
    let _ = print("c7: ");
    let _ = println(closure_early(7i32));
    let _ = print("c2: ");
    let _ = println(closure_early(2i32));
    unit_ret(true);
    unit_ret(false);
}
`;
export {
  n as default
};
