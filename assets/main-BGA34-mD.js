const n = `trait Render {
    fn format(Self) -> string;
}

struct Boxed {
    value: int32,
}

impl Boxed {
    fn format(self: Self) -> string {
        "inherent"
    }
}

impl Render for Boxed {
    fn format(self: Boxed) -> string {
        self.value.to_string()
    }
}

fn main() {
    println(Boxed::format(Boxed { value: 9 }));
    println(Render::format(Boxed { value: 9 }));
}
`;
export {
  n as default
};
