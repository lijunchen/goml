fn main() {
  string_println(int32_to_string(def/2(ctor(def/1::v0))));
}

enum Lib::Color {
    Red,
    Green
}

fn Lib::color_to_int(local/0: Lib::Color) -> int32 {
  match c/0 {
      ctor(def/1::v0) => 1,
      ctor(def/1::v1) => 2,
  };
}

