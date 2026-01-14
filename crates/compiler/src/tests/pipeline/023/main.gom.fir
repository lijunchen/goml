package Main
file main.gom
  fn match_mixed_pair(local/1/0: (int32, string)) -> int32 {
      match pair/0 {
          (0, "zero") => 1,
          (0, _) => 2,
          (1, "one") => 3,
          (_, "zero") => 4,
          _ => 5,
      };
    }
    
    enum Mixed {
        OnlyInt(int32),
        OnlyStr(string),
        Both(int32, string)
    }
    
    fn match_mixed_enum(local/1/1: Mixed) -> int32 {
      match value/1 {
          ctor(def/1/1::v0)(0) => 6,
          ctor(def/1/1::v0)(_) => 7,
          ctor(def/1/1::v1)("zero") => 8,
          ctor(def/1/1::v1)(_) => 9,
          ctor(def/1/1::v2)(0, "zero") => 10,
          ctor(def/1/1::v2)(0, _) => 11,
          ctor(def/1/1::v2)(_, "zero") => 12,
          ctor(def/1/1::v2)(_, _) => 13,
      };
    }
    
    fn main() {
      let _ = string_println(int32_to_string(def/1/0((0, "zero"))));
      let _ = string_println(int32_to_string(def/1/0((0, "other"))));
      let _ = string_println(int32_to_string(def/1/0((1, "one"))));
      let _ = string_println(int32_to_string(def/1/0((2, "zero"))));
      let _ = string_println(int32_to_string(def/1/0((2, "two"))));
      let _ = string_println(int32_to_string(def/1/2(ctor(def/1/1::v0)(0))));
      let _ = string_println(int32_to_string(def/1/2(ctor(def/1/1::v0)(5))));
      let _ = string_println(int32_to_string(def/1/2(ctor(def/1/1::v1)("zero"))));
      let _ = string_println(int32_to_string(def/1/2(ctor(def/1/1::v1)("hello"))));
      let _ = string_println(int32_to_string(def/1/2(ctor(def/1/1::v2)(0, "zero"))));
      let _ = string_println(int32_to_string(def/1/2(ctor(def/1/1::v2)(0, "hello"))));
      let _ = string_println(int32_to_string(def/1/2(ctor(def/1/1::v2)(2, "zero"))));
      string_println(int32_to_string(def/1/2(ctor(def/1/1::v2)(3, "three"))));
    }