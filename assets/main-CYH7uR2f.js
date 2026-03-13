const n = `fn my_int_equal(x: int32, y: int32) -> bool {
    !(x < y) && !(y < x)
}

fn sum(n: int32) -> int32 {
    if my_int_equal(n, 1) {
        1
    } else {
        n + sum(n - 1)
    }
}

fn main() {
    println(sum(100))
    
}
`;
export {
  n as default
};
