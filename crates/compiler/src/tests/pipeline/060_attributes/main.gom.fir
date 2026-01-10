#[derive(ToString)]
struct Point {
    x: int32,
    y: int32,
}

impl Point {
  fn to_string(local/0: Point) -> string {
    let Point { x: local/1, y: local/2 } = self/0;
    "Point { " + "x: " + x/1.to_string() + ", " + "y: " + y/2.to_string() + " }";
  }
}

#[derive(ToString)]
enum Message {
    Quit,
    Move(int32, int32),
    Write(string)
}

impl Message {
  fn to_string(local/3: Message) -> string {
    match self/3 {
        ctor(def/2::v0) => "Message::Quit",
        ctor(def/2::v1)(local/4, local/5) => "Message::Move(" + __field0/4.to_string() + ", " + __field1/5.to_string() + ")",
        ctor(def/2::v2)(local/6) => "Message::Write(" + __field0/6 + ")",
    }
  }
}

fn main() -> unit {
  let local/7 = Point {
      x: 4,
      y: 7
  };
  let local/8 = point/7.to_string();
  let local/9 = ctor(def/2::v1)(1, 2).to_string();
  let local/10 = ctor(def/2::v2)("done").to_string();
  let local/11 = ctor(def/2::v0).to_string();
  string_println(summary/8);
  string_println(mv/9);
  string_println(text/10);
  string_println(exit/11);
  ();
}

