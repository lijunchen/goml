const n = `trait Provider {
    type Item: ToString;

    fn get(Self) -> Self::Item;
}

struct Number {
    value: int32,
}

struct Box[T] {
    value: T,
}

impl Provider for Number {
    type Item = int32;

    fn get(self: Number) -> int32 {
        self.value
    }
}

impl[T: ToString] Provider for Box[T] {
    type Item = T;

    fn get(self: Box[T]) -> T {
        self.value
    }
}

fn read[P: Provider](provider: P) -> P::Item {
    provider.get()
}

fn read_as[P, T](provider: P) -> T
where
    P: Provider,
    P::Item == T,
{
    provider.get()
}

fn main() -> unit {
    string_println(read(Number { value: 42 }).to_string());
    let value: int32 = read_as(Number { value: 7 });
    string_println(value.to_string());
    string_println(read(Box { value: "generic" }));
    string_println(Provider::get(Number { value: 11 }).to_string())
}
`;
export {
  n as default
};
