const n = `trait Arith {
    fn add(Self, Self) -> int32;
    fn less(Self, Self) -> bool;
}

impl Arith for int32 {
    fn add(self: int32, other: int32) -> int32 {
        self + other
    }

    fn less(self: int32, other: int32) -> bool {
        self < other
    }
}

trait ToString {
    fn to_string(Self) -> string;
}

impl ToString for int32 {
    fn to_string(self: int32) -> string {
        int32_to_string(self)
    }
}

impl ToString for bool {
    fn to_string(self: bool) -> string {
        bool_to_string(self)
    }
}

trait Output {
    fn output(Self) -> unit;
}

impl Output for int32 {
    fn output(self: int32) -> unit {
        string_println(ToString::to_string(self))
    }
}

impl Output for bool {
    fn output(self: bool) -> unit {
        string_println(ToString::to_string(self))
    }
}

fn id[T](x: T) -> T {
    x
}

fn main() {
    let a = id(1);
    let b = id(2);
    let c = Arith::add(a, b);
    let _ = Output::output(c);

    let a = id(3);
    let b = id(4);
    let c = Arith::less(a, b);
    let _ = Output::output(c);

    let _ = id("abc");
    let _ = id(true);
    
    ()
}`;
export {
  n as default
};
