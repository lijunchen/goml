package Main
file main.gom
  fn main() {
      let local/1/0 = (true, false);
      let _ = match a/0 {
          (true, local/1/1) => string_print(bool_to_string(b/1)),
          _ => (),
      };
      let local/1/2 = (true, true);
      match c/2 {
          (true, local/1/3) => string_print(bool_to_string(d/3)),
          _ => (),
      };
    }