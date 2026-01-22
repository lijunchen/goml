package Main
file main.gom
  #[derive(ToString)]
struct Point {
        x: int32,
        y: int32,
    }
    
    impl Point {
      fn to_string(local/1/0: Point) -> string {
        let Point { x: local/1/1, y: local/1/2 } = self/0;
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
      fn to_string(local/1/3: Message) -> string {
        match self/3 {
            ctor(def/1/2::v0) => "Message::Quit",
            ctor(def/1/2::v1)(local/1/4, local/1/5) => "Message::Move(" + __field0/4.to_string() + ", " + __field1/5.to_string() + ")",
            ctor(def/1/2::v2)(local/1/6) => "Message::Write(" + __field0/6 + ")",
        }
      }
    }
    
    fn main() -> unit {
      let local/1/7 = Point {
          x: 4,
          y: 7
      };
      let local/1/8 = point/7.to_string();
      let local/1/9 = ctor(def/1/2::v1)(1, 2).to_string();
      let local/1/10 = ctor(def/1/2::v2)("done").to_string();
      let local/1/11 = ctor(def/1/2::v0).to_string();
      string_println(summary/8);
      string_println(mv/9);
      string_println(text/10);
      string_println(exit/11);
      ();
    }