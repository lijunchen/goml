enum Color {
    Red,
    Green,
    Blue
}

fn main() {
  let local/0 = (ctor/2, ctor/0);
  match a/0 {
      (ctor/0, ctor/1) => string_print(int32_to_string(0)),
      (ctor/0, ctor/0) => string_print(int32_to_string(1)),
      (ctor/2, ctor/2) => string_print(int32_to_string(2)),
      _ => string_print(int32_to_string(3)),
  };
}

