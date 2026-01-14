package Main
file main.gom
  fn test_nested_match(local/1/0: (bool, bool), local/1/1: (bool, bool)) {
      match x/0 {
          (true, false) => match y/1 {
                (false, true) => string_println("case1"),
                _ => string_println("case2"),
            },
          _ => match y/1 {
                (false, true) => string_println("case3"),
                _ => string_println("case4"),
            },
      };
    }
    
    fn main() {
      let _ = def/1/0((true, false), (false, true));
      let _ = def/1/0((true, false), (true, false));
      let _ = def/1/0((false, true), (false, true));
      let _ = def/1/0((false, true), (true, false));
      ();
    }