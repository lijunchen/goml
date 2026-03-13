const n = `trait A {
    fn pick(Self) -> int32;
}

trait B {
    fn pick(Self) -> int32;
}

struct S {}

impl A for S {
    fn pick(self: S) -> int32 {
        10
    }
}

impl B for S {
    fn pick(self: S) -> int32 {
        20
    }
}

fn pick_a[T: A + B](x: T) -> int32 {
    A::pick(x)
}

fn pick_b[T: A + B](x: T) -> int32 {
    B::pick(x)
}

fn main() {
    println(int32_to_string(pick_a(S {})));
    println(int32_to_string(pick_b(S {})));
}
`;
export {
  n as default
};
