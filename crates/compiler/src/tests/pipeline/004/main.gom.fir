enum Color {
    Red,
    Green,
    Blue
}

fn main() -> bool {
  let local/0 = (ctor/2, ctor/2);
  match a/0 {
      (ctor/0, ctor/1) => true,
      (ctor/0, ctor/0) => true,
      (ctor/2, ctor/2) => {
            let _ = string_print(bool_to_string(true));
            false;
        },
      _ => false,
  };
}

