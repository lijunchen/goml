struct Point {
    x: int32,
    y: int32,
}

enum Maybe[T] {
    Just(T),
    Nothing
}

trait Display {
  fn show(Self) -> string;
}

impl Display for Point {
  fn show(local/0: Point) -> string {
    "Point";
  }
}

impl Display for Maybe[int32] {
  fn show(local/1: Maybe[int32]) -> string {
    match self/1 {
        ctor/0(local/2) => "Just(" + int32_to_string(value/2) + ")",
        ctor/1 => "Nothing",
    };
  }
}

fn make_maybe(local/3: bool) -> Maybe[int32] {
  if flag/3 {
      {
          ctor/0(42);
      }
  } else {
      {
          ctor/1;
      }
  };
}

fn main() {
  let local/4 = Point {
      x: 1,
      y: 2
  };
  let local/5 = def/5(true);
  let local/6 = def/5(false);
  let _ = string_println(Display::show(point/4));
  let _ = string_println(Display::show(some_number/5));
  let _ = string_println(Display::show(none_number/6));
  ();
}

