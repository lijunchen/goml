const n = `trait Display {
    fn show(Self) -> string;
}

impl Display for int32 {
    fn show(self: int32) -> string {
        self.to_string()
    }
}

fn render(x: dyn Display) -> string {
    Display::show(x)
}

fn main() -> unit {
    let v: Vec[dyn Display] = Vec::new();
    let v = v.push(10i32);
    let v = v.push(20i32);
    let s: Slice[dyn Display] = v.slice(0i32, 2i32);
    println(render(s[0i32]));
    let t = s.sub(1i32, 2i32);
    println(render(t[0i32]));
}
`;
export {
  n as default
};
