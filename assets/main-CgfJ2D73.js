const n = `#[derive(ToString)]
struct Point {
  x: int32,
  y: int32,
}

#[derive(ToString)]
enum Message {
  Quit,
  Move(int32, int32),
  Write(string),
}

fn main() -> unit {
  let point = Point { x: 4, y: 7 };
  let summary = point.to_string();
  let mv = Message::Move(1, 2).to_string();
  let text = Message::Write("done").to_string();
  let exit = Message::Quit.to_string();
  string_println(summary);
  string_println(mv);
  string_println(text);
  string_println(exit);
}
`;
export {
  n as default
};
