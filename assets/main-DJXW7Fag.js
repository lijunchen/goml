const n = `#[derive(Hash, Eq)]
struct Point {
    x: int32,
    y: int32,
}

#[derive(Hash, Eq)]
enum Key {
    A,
    B(int32),
    P(Point),
}

fn print_opt_int(x: Option[int32]) -> unit {
    match x {
        Option::Some(v) => println(v),
        Option::None => println("none"),
    }
}

fn main() -> unit {
    let m1 = HashMap::new();
    m1.set(Key::A, 10);
    m1.set(Key::B(1), 20);
    println(m1.len());
    print_opt_int(m1.get(Key::A));
    println(m1.contains(Key::B(1)));
    m1.remove(Key::B(1));
    println(m1.contains(Key::B(1)));
    println(m1.len());

    let m2 = HashMap::new();
    let p1 = ref(Point { x: 1, y: 2 });
    let p2 = ref(Point { x: 1, y: 2 });
    m2.set(p1, 99);
    print_opt_int(m2.get(p2));

    let m3 = HashMap::new();
    let k1 = ref(Key::B(7));
    let k2 = ref(Key::B(7));
    m3.set(k1, 123);
    print_opt_int(m3.get(k2));
}
`;
export {
  n as default
};
