const n = `fn classify(n: int32) -> string {
    match n {
        -1 => "minus one",
        0 => "zero",
        1 => "one",
        _ => "other",
    }
}

fn main() {
    println(classify(-1));
    println(classify(0));
    println(classify(1));
    println(classify(42));
}
`;
export {
  n as default
};
