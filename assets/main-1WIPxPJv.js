const n = `struct S {
    value: int32,
}

impl ToString for S {
    fn to_string(self: S) -> string {
        "S(" + self.value.to_string() + ")"
    }
}

fn main() -> unit {
    let s = S { value: 7 };
    println(s.to_string());
    ()
}
`;
export {
  n as default
};
