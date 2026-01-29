const n = `// Test ToJson derive macro
#[derive(ToJson)]
struct Point {
    x: int32,
    y: int32,
}

#[derive(ToJson)]
struct Person {
    name: string,
    age: int32,
    active: bool,
}

#[derive(ToJson)]
enum Color {
    Red,
    Green,
    Blue,
    Rgb(int32, int32, int32),
}

fn main() -> unit {
    let p = Point { x: 10, y: 20 };
    let person = Person { name: "Alice", age: 30, active: true };
    let c1 = Color::Red;
    let c2 = Color::Rgb(255, 128, 0);

    string_println(p.to_json());
    string_println(person.to_json());
    string_println(c1.to_json());
    string_println(c2.to_json());
}
`;
export {
  n as default
};
