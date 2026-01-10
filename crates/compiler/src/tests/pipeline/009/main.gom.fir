enum T {
    A,
    B(bool, bool)
}

fn test(local/0: T) -> unit {
  match t/0 {
      ctor/0 => string_print(int32_to_string(1)),
      ctor/1(false, false) => string_print(int32_to_string(2)),
      ctor/1(false, true) => string_print(int32_to_string(3)),
      _ => string_print(int32_to_string(4)),
  };
}

fn main() {
  let _ = def/1(ctor/1(true, true));
  let _ = def/1(ctor/1(false, true));
  let _ = def/1(ctor/1(false, false));
  let _ = def/1(ctor/0);
  ();
}

