struct Point {
    x: int32,
    y: int32,
}

fn point32_to_string(local/0: Point) -> string {
  let Point { x: local/1, y: local/2 } = p/0;
  "Point { x: " + int32_to_string(x/1) + ", y: " + int32_to_string(y/2) + " }";
}

enum Color {
    Red,
    Green,
    Blue
}

fn color_to_string(local/3: Color) -> string {
  match c/3 {
      ctor(def/2::v0) => "Red",
      ctor(def/2::v1) => "Green",
      ctor(def/2::v2) => "Blue",
  };
}

struct Line {
    from: Point,
    to: Point,
    color: Color,
}

fn line_to_string(local/4: Line) -> string {
  let Line { from: local/5, to: local/6, color: local/7 } = l/4;
  "Line { from: " + def/1(from/5) + ", to: " + def/1(to/6) + ", color: " + def/3(color/7) + " }";
}

fn point_type(local/8: Point) -> string {
  match p/8 {
      Point { x: 0, y: 0 } => {
            "origin";
        },
      Point { x: 0, y: 1 } => {
            "up";
        },
      Point { x: 1, y: 0 } => {
            "right";
        },
      Point { x: 0, y: local/9 } => {
            match 0 < y/9 {
                true => "above",
                false => "below",
            };
        },
      _ => "unknown",
  };
}

fn main() {
  let local/10 = Point {
      x: 0,
      y: 0
  };
  let _ = string_println(def/6(p0/10));
  let local/11 = Point {
      x: 10,
      y: 10
  };
  let local/12 = Line {
      from: p0/10,
      to: p1/11,
      color: ctor(def/2::v0)
  };
  string_println(def/5(line/12));
}

