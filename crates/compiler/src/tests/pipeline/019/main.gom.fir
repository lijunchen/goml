struct Point {
    x: int32,
    y: int32,
}

struct Wrapper[T] {
    value: T,
}

fn make_point() -> Point {
  Point {
      x: 0,
      y: 0
  };
}

fn flip(local/0: Point) -> Point {
  let Point { x: local/1, y: local/2 } = point/0;
  Point {
      y: x/1,
      x: y/2
  };
}

fn wrap_int(local/3: int32) -> Wrapper[int32] {
  Wrapper {
      value: x/3
  };
}

fn x_add_1(local/4: Point) -> Point {
  let Point { x: local/5, y: local/6 } = p/4;
  Point {
      x: x/5 + 1,
      y: y/6
  };
}

fn get_x(local/7: Point) -> int32 {
  let Point { x: local/8, y: local/9 } = p/7;
  x/8;
}

fn get_y(local/10: Point) -> int32 {
  let Point { x: local/11, y: local/12 } = p/10;
  y/12;
}

fn point32_to_string(local/13: Point) -> string {
  let Point { x: local/14, y: local/15 } = p/13;
  "Point { x: " + int32_to_string(x/14) + ", y: " + int32_to_string(y/15) + "}";
}

fn point32_to_string2(local/16: Point) -> string {
  let Point { x: local/17, y: local/18 } = p/16;
  "Point { x: " + int32_to_string(x/17) + ", y: " + int32_to_string(y/18) + "}";
}

fn point32_to_string3(local/19: Point) -> string {
  let Point { x: local/20, y: local/21 } = p/19;
  "Point { x: " + int32_to_string(x/20) + ", y: " + int32_to_string(y/21) + "}";
}

fn point32_to_string4(local/22: Point) -> string {
  let Point { x: local/23, y: local/24 } = p/22;
  "Point { x: " + int32_to_string(x/23) + ", y: " + int32_to_string(y/24) + "}";
}

fn main() {
  let local/25 = def/2();
  let _ = string_println(def/8(start/25));
  let local/26 = def/3(Point {
        y: 2,
        x: 1
    });
  let _ = string_println(def/9(swapped/26));
  let local/27 = def/4(3);
  let local/28 = Wrapper {
      value: swapped/26
  };
  let local/29 = def/5(start/25);
  let _ = string_println(def/10(a/29));
  let local/30 = def/3(def/5(start/25));
  let _ = string_println(def/11(a/30));
  ();
}

