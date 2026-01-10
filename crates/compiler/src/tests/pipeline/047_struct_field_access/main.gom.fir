struct Point {
    x: int32,
    y: int32,
}

fn make_point(local/0: int32) -> Point {
  Point {
      x: base/0,
      y: base/0 + 1
  };
}

fn sum_point(local/1: Point) -> int32 {
  p/1.x + p/1.y;
}

fn main() -> int32 {
  let local/2 = def/1(5);
  let local/3 = Point {
      x: p/2.x + 1,
      y: p/2.y - 2
  };
  shifted/3.x + def/2(shifted/3);
}

