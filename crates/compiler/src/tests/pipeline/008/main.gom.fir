enum T {
    A,
    B(bool, unit)
}

fn main() {
  let local/0 = ctor(def/0::v1)(true, ());
  match t/0 {
      ctor(def/0::v0) => string_print(int32_to_string(1)),
      ctor(def/0::v1)(true, ()) => string_print(int32_to_string(2)),
      ctor(def/0::v1)(false, ()) => string_print(int32_to_string(3)),
  };
}

