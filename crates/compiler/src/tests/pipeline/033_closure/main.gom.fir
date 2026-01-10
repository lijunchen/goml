fn test() {
  let local/0 = 3;
  let local/1 = 5;
  let local/3 = |local/2: int32| x/2 * y/0 * z/1;
  let _ = string_println(int32_to_string(f/3(2)));
  string_println(int32_to_string(f/3(3)));
}

fn call_int_id(local/4: (int32) -> int32, local/5: int32) -> int32 {
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
  let local/6 = 5;
  let local/8 = |local/7: int32| x/7 + base/6;
  let local/9 = add_base/8(7);
  let local/13 = |local/10: string, local/11: int32| {
      let local/12 = prefix/10 + int32_to_string(value/11);
      string_println(message/12);
  };
  let _ = printer/13("result: ", result/9);
  let local/15 = |local/14: int32| y/14 + result/9;
  let local/17 = |local/16: int32| z/16 * 2;
  let local/18 = no_capture/17(3);
  let _ = string_println(int32_to_string(doubled/18));
  let _ = def/0();
  let local/19 = ctor/1(1, ctor/1(2, ctor/1(3, ctor/0)));
  let local/20 = Point {
      x: 10,
      y: 20
  };
  let local/25 = || {
      match list123/19 {
          ctor/0 => string_println("Empty list"),
          ctor/1(local/21, local/22) => {
                let _ = string_println(int32_to_string(head/21));
                match point/20 {
                    Point { x: local/23, y: local/24 } => {
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

