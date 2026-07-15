const n = `trait Measure {
    fn measure(Self) -> int32;
}

impl Measure for Vec[int32] {
    fn measure(self: Vec[int32]) -> int32 {
        self.len()
    }
}

fn read_measure[T: Measure](value: T) -> int32 {
    Measure::measure(value)
}

fn main() -> unit {
    let values = vec_new();
    println(read_measure(values).to_string())
}
`;
export {
  n as default
};
