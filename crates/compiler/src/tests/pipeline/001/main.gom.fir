package Main
file main.gom
  fn main() {
      let _ = ();
      let local/1/0 = (true, false, (true, false));
      let local/1/5 = match a/0 {
          (local/1/1, local/1/2, (local/1/3, local/1/4)) => w/4,
      };
      let _ = string_print(bool_to_string(b/5));
      ();
    }