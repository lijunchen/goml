package Main
file main.gom
  fn choose(local/1/0: bool, local/1/1: int32, local/1/2: int32) -> int32 {
      if flag/0 {
          x/1;
      } else {
          y/2;
      };
    }
    
    fn main() {
      let local/1/3 = def/1/0(true, 10, 99);
      let local/1/4 = def/1/0(false, 10, 99);
      let _ = string_println("yes=" + int32_to_string(yes/3));
      let _ = string_println("no=" + int32_to_string(no/4));
      ();
    }