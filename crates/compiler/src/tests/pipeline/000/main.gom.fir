#[derive(ToString)]
enum Color {
    Red,
    Green,
    Blue
}

impl Color {
  fn to_string(local/0: Color) -> string {
    match self/0 {
        ctor/0 => "Color::Red",
        ctor/1 => "Color::Green",
        ctor/2 => "Color::Blue",
    }
  }
}

#[derive(ToString)]
struct Point {
    x: int32,
    y: int32,
    color: Color,
}

impl Point {
  fn to_string(local/1: Point) -> string {
    let Point { x: local/2, y: local/3, color: local/4 } = self/1;
    "Point { " + "x: " + x/2.to_string() + ", " + "y: " + y/3.to_string() + ", " + "color: " + color/4.to_string() + " }";
  }
}

impl Point {
  fn new(local/5: int32, local/6: int32, local/7: Color) -> Point {
    Point {
        x: x/5,
        y: y/6,
        color: color/7
    };
  }
}

#[derive(ToString)]
struct Line {
    from: Point,
    to: Point,
    color: Color,
}

impl Line {
  fn to_string(local/8: Line) -> string {
    let Line { from: local/9, to: local/10, color: local/11 } = self/8;
    "Line { " + "from: " + from/9.to_string() + ", " + "to: " + to/10.to_string() + ", " + "color: " + color/11.to_string() + " }";
  }
}

impl Line {
  fn new(local/12: Point, local/13: Point, local/14: Color) -> Line {
    Line {
        from: from/12,
        to: to/13,
        color: color/14
    };
  }
}

#[derive(ToString)]
enum LineList {
    Nil,
    Cons(Line, LineList)
}

impl LineList {
  fn to_string(local/15: LineList) -> string {
    match self/15 {
        ctor/3 => "LineList::Nil",
        ctor/4(local/16, local/17) => "LineList::Cons(" + __field0/16.to_string() + ", " + __field1/17.to_string() + ")",
    }
  }
}

fn main() {
  let local/18 = Point::new(10, 20, ctor/0);
  let local/19 = Point::new(30, 40, ctor/1);
  let local/20 = Line::new(from/18, to/19, ctor/2);
  let local/21 = ctor/4(line/20, ctor/3);
  string_println(lines/21.to_string());
  ();
}

