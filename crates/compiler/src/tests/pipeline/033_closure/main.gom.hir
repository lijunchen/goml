package Main
file main.gom
  fn test() {
      let local/1/0 = 3;
      let local/1/1 = 5;
      let local/1/3 = |local/1/2: int32| x/2 * y/0 * z/1;
      let _ = string_println(int32_to_string(f/3(2)));
      string_println(int32_to_string(f/3(3)));
    }
    
    fn call_int_id(local/1/4: (int32) -> int32, local/1/5: int32) -> int32 {
      f/4(v/5);
    }
    
    enum IntList {
        Nil,
        Cons(int32, IntList)
    }
    
    struct Point {
        x: int32,
        y: int32,
    }
    
    fn main() {
      let local/1/6 = 5;
      let local/1/8 = |local/1/7: int32| x/7 + base/6;
      let local/1/9 = add_base/8(7);
      let local/1/13 = |local/1/10: string, local/1/11: int32| {
          let local/1/12 = prefix/10 + int32_to_string(value/11);
          string_println(message/12);
      };
      let _ = printer/13("result: ", result/9);
      let local/1/15 = |local/1/14: int32| y/14 + result/9;
      let local/1/17 = |local/1/16: int32| z/16 * 2;
      let local/1/18 = no_capture/17(3);
      let _ = string_println(int32_to_string(doubled/18));
      let _ = def/1/0();
      let local/1/19 = ctor(def/1/2::v1)(1, ctor(def/1/2::v1)(2, ctor(def/1/2::v1)(3, ctor(def/1/2::v0))));
      let local/1/20 = Point {
          x: 10,
          y: 20
      };
      let local/1/25 = || {
          match list123/19 {
              ctor(def/1/2::v0) => string_println("Empty list"),
              ctor(def/1/2::v1)(local/1/21, local/1/22) => {
                    let _ = string_println(int32_to_string(head/21));
                    match point/20 {
                        Point { x: local/1/23, y: local/1/24 } => {
                              let _ = string_println("Point: (" + int32_to_string(x/23) + ", " + int32_to_string(y/24) + ")");
                              ();
                          },
                    };
                },
          };
      };
      let _ = play_list_and_point/25();
      ();
    }