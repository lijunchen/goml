struct Point[U, V] {
    x: U,
    y: V,
}

impl[U, V] Point[U, V] {
  fn new(local/0: U, local/1: V) -> Point[U, V] {
    Point {
        x: x/0,
        y: y/1
    };
  }
  fn swap(local/2: Point[U, V]) -> Point[V, U] {
    Point {
        x: self/2.y,
        y: self/2.x
    };
  }
  fn get_x(local/3: Point[U, V]) -> U {
    self/3.x;
  }
}

fn main() -> unit {
  let local/4: Point[int32, string] = Point::new(10, "hello");
  let local/5: Point[string, string] = Point::new("goml", "lang");
  let local/6: Point[string, int32] = p1/4.swap();
  let local/7: int32 = p3/6.y;
  string_println(int32_to_string(x/7));
  let local/8: string = p2/5.get_x();
  string_println(x2/8);
}

