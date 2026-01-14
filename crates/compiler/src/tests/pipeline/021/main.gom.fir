package Main
file main.gom
  fn match_int(local/1/0: int32) -> int32 {
      match n/0 {
          0 => 10,
          1 => 20,
          _ => 30,
      };
    }
    
    fn wildcard_first(local/1/1: int32) -> int32 {
      match n/1 {
          _ => 40,
          0 => 50,
      };
    }
    
    fn wildcard_middle(local/1/2: int32) -> int32 {
      match n/2 {
          2 => 90,
          _ => 100,
          3 => 110,
      };
    }
    
    fn repeated(local/1/3: int32) -> int32 {
      match n/3 {
          1 => 60,
          1 => 70,
          _ => 80,
      };
    }
    
    fn main() {
      let _ = string_println(int32_to_string(def/1/0(0)));
      let _ = string_println(int32_to_string(def/1/0(5)));
      let _ = string_println(int32_to_string(def/1/1(0)));
      let _ = string_println(int32_to_string(def/1/1(2)));
      let _ = string_println(int32_to_string(def/1/2(2)));
      let _ = string_println(int32_to_string(def/1/2(3)));
      let _ = string_println(int32_to_string(def/1/3(1)));
      string_println(int32_to_string(def/1/3(3)));
    }