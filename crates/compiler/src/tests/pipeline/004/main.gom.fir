enum Color {
    Red,
    Green,
    Blue
}

fn main() -> bool {
  let local/0 = (ctor(def/0::v2), ctor(def/0::v2));
  match a/0 {
      (ctor(def/0::v0), ctor(def/0::v1)) => true,
      (ctor(def/0::v0), ctor(def/0::v0)) => true,
      (ctor(def/0::v2), ctor(def/0::v2)) => {
            let _ = string_print(bool_to_string(true));
            false;
        },
      _ => false,
  };
}

