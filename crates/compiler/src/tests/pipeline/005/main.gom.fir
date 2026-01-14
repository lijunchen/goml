package Main
file main.gom
  enum Color {
        Red,
        Green,
        Blue
    }
    
    fn main() {
      let local/1/0 = (ctor(def/1/0::v2), ctor(def/1/0::v0));
      match a/0 {
          (ctor(def/1/0::v0), ctor(def/1/0::v1)) => string_print(int32_to_string(0)),
          (ctor(def/1/0::v0), ctor(def/1/0::v0)) => string_print(int32_to_string(1)),
          (ctor(def/1/0::v2), ctor(def/1/0::v2)) => string_print(int32_to_string(2)),
          _ => string_print(int32_to_string(3)),
      };
    }