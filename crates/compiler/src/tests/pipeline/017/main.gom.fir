package Main
file main.gom
  trait ToString {
      fn to_string(Self) -> string;
    }
    
    impl ToString for int32 {
      fn to_string(local/1/0: int32) -> string {
        int32_to_string(self/0);
      }
    }
    
    impl ToString for bool {
      fn to_string(local/1/1: bool) -> string {
        bool_to_string(self/1);
      }
    }
    
    impl ToString for (int32, int32) {
      fn to_string(local/1/2: (int32, int32)) -> string {
        "(?, ?)";
      }
    }
    
    fn main() {
      let local/1/3 = 123;
      let _ = string_println(ToString::to_string(x/3));
      let local/1/4 = true;
      let _ = string_println(ToString::to_string(x/4));
      let local/1/5 = (3, 4);
      let _ = string_println(ToString::to_string(x/5));
      ();
    }