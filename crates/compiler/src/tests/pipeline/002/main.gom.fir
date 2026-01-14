package Main
file main.gom
  fn main() {
      let local/1/0 = (true, false);
      let local/1/1 = match a/0 {
          (false, false) => (true, true),
          (false, true) => (true, false),
          (true, false) => (false, true),
          (true, true) => (false, false),
      };
      let local/1/3 = match b/1 {
          (_, local/1/2) => w/2,
      };
      let local/1/4 = match (true, b_1/3) {
          (false, false) => string_print(int32_to_string(0)),
          (false, true) => string_print(int32_to_string(1)),
          (true, false) => string_print(int32_to_string(2)),
          (true, true) => string_print(int32_to_string(3)),
      };
      string_print(unit_to_string(c/4));
    }