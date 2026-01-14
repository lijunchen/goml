package Main
file main.gom
  #[derive(ToString)]
enum Color {
        Red,
        Green,
        Blue
    }
    
    impl Color {
      fn to_string(local/1/0: Color) -> string {
        match self/0 {
            ctor(def/1/0::v0) => "Color::Red",
            ctor(def/1/0::v1) => "Color::Green",
            ctor(def/1/0::v2) => "Color::Blue",
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
      fn to_string(local/1/1: Point) -> string {
        let Point { x: local/1/2, y: local/1/3, color: local/1/4 } = self/1;
        "Point { " + "x: " + x/2.to_string() + ", " + "y: " + y/3.to_string() + ", " + "color: " + color/4.to_string() + " }";
      }
    }
    
    impl Point {
      fn new(local/1/5: int32, local/1/6: int32, local/1/7: Color) -> Point {
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
      fn to_string(local/1/8: Line) -> string {
        let Line { from: local/1/9, to: local/1/10, color: local/1/11 } = self/8;
        "Line { " + "from: " + from/9.to_string() + ", " + "to: " + to/10.to_string() + ", " + "color: " + color/11.to_string() + " }";
      }
    }
    
    impl Line {
      fn new(local/1/12: Point, local/1/13: Point, local/1/14: Color) -> Line {
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
      fn to_string(local/1/15: LineList) -> string {
        match self/15 {
            ctor(def/1/8::v0) => "LineList::Nil",
            ctor(def/1/8::v1)(local/1/16, local/1/17) => "LineList::Cons(" + __field0/16.to_string() + ", " + __field1/17.to_string() + ")",
        }
      }
    }
    
    fn main() {
      let local/1/18 = Point::new(10, 20, ctor(def/1/0::v0));
      let local/1/19 = Point::new(30, 40, ctor(def/1/0::v1));
      let local/1/20 = Line::new(from/18, to/19, ctor(def/1/0::v2));
      let local/1/21 = ctor(def/1/8::v1)(line/20, ctor(def/1/8::v0));
      string_println(lines/21.to_string());
      ();
    }