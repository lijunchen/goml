enum T {
    A,
    B(bool, unit)
}

fn main() {
  let local/0 = ctor/1(true, ());
  match t/0 {
      ctor/0 => string_print(int32_to_string(1)),
      ctor/1(true, ()) => string_print(int32_to_string(2)),
      ctor/1(false, ()) => string_print(int32_to_string(3)),
  };
}

