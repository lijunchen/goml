#[derive(ToJson)]
struct Point {
    x: int32,
    y: int32,
}

impl Point {
  fn to_json(local/0: Point) -> string {
    let Point { x: local/1, y: local/2 } = self/0;
    "{" + "\"x\":" + x/1.to_string() + "," + "\"y\":" + y/2.to_string() + "}";
  }
}

#[derive(ToJson)]
struct Person {
    name: string,
    age: int32,
    active: bool,
}

impl Person {
  fn to_json(local/3: Person) -> string {
    let Person { name: local/4, age: local/5, active: local/6 } = self/3;
    "{" + "\"name\":" + json_escape_string(name/4) + "," + "\"age\":" + age/5.to_string() + "," + "\"active\":" + bool_to_json(active/6) + "}";
  }
}

#[derive(ToJson)]
enum Color {
    Red,
    Green,
    Blue,
    Rgb(int32, int32, int32)
}

impl Color {
  fn to_json(local/7: Color) -> string {
    match self/7 {
        ctor(def/4::v0) => "{\"tag\":\"Red\"}",
        ctor(def/4::v1) => "{\"tag\":\"Green\"}",
        ctor(def/4::v2) => "{\"tag\":\"Blue\"}",
        ctor(def/4::v3)(local/8, local/9, local/10) => "{\"tag\":\"Rgb\",\"fields\":[" + __field0/8.to_string() + "," + __field1/9.to_string() + "," + __field2/10.to_string() + "]}",
    }
  }
}

fn main() -> unit {
  let local/11 = Point {
      x: 10,
      y: 20
  };
  let local/12 = Person {
      name: "Alice",
      age: 30,
      active: true
  };
  let local/13 = ctor(def/4::v0);
  let local/14 = ctor(def/4::v3)(255, 128, 0);
  string_println(p/11.to_json());
  string_println(person/12.to_json());
  string_println(c1/13.to_json());
  string_println(c2/14.to_json());
  ();
}

