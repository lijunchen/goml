const n = `trait Mark {
    fn marked(Self) -> string;
}

trait Source {
    type Item;

    fn get(Self) -> Self::Item;
}

struct LeftSource {
    value: int32,
}

struct RightSource {
    value: int32,
}

impl Mark for int32 {
    fn marked(self: int32) -> string {
        "m" + self.to_string()
    }
}

impl Source for LeftSource {
    type Item = int32;

    fn get(self: LeftSource) -> int32 {
        self.value
    }
}

impl Source for RightSource {
    type Item = int32;

    fn get(self: RightSource) -> int32 {
        self.value
    }
}

fn combine[A: Source, B: Source](left: A, right: B) -> string
where
    A::Item == B::Item,
    B::Item: Mark,
{
    Source::get(left).marked() + ":" + Source::get(right).marked()
}

fn main() -> unit {
    println(combine(
        LeftSource { value: 3 },
        RightSource { value: 4 },
    ));
}
`;
export {
  n as default
};
