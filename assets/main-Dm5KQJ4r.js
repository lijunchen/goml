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
    let m1 = hashmap_new();
    hashmap_set(m1, Key::A, 10);
    hashmap_set(m1, Key::B(1), 20);
    println(hashmap_len(m1));
    print_opt_int(hashmap_get(m1, Key::A));
    println(hashmap_contains(m1, Key::B(1)));
    hashmap_remove(m1, Key::B(1));
    println(hashmap_contains(m1, Key::B(1)));
    println(hashmap_len(m1));

    let m2 = hashmap_new();
    let p1 = ref(Point { x: 1, y: 2 });
    let p2 = ref(Point { x: 1, y: 2 });
    hashmap_set(m2, p1, 99);
    print_opt_int(hashmap_get(m2, p2));

    let m3 = hashmap_new();
    let k1 = ref(Key::B(7));
    let k2 = ref(Key::B(7));
    hashmap_set(m3, k1, 123);
    print_opt_int(hashmap_get(m3, k2));
}
`;
export {
  n as default
};
