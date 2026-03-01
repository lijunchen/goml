const n = `package main;

enum Option[T] {
    Some(T),
    None,
}

fn main() {
    let x: Option[uint8] = Option::Some(42);
    match x {
        Option::Some(v) => string_println(uint8_to_string(v)),
        Option::None => string_println("none"),
    };
}
`;
export {
  n as default
};
