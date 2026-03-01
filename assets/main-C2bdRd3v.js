const n = `struct Point { x: int32, y: int32 }

struct Flag { value: bool }

struct Counter { cell: Ref[int32] }

trait Display {
    fn show(Self) -> string;
    fn show_with(Self, string, string) -> string;
    fn tick(Self) -> unit;
    fn bump(Self, int32) -> int32;
}

impl Display for Point {
    fn show(self: Point) -> string {
        "Point(" + int32_to_string(self.x) + "," + int32_to_string(self.y) + ")"
    }

    fn show_with(self: Point, prefix: string, suffix: string) -> string {
        prefix + "Point(" + int32_to_string(self.x) + "," + int32_to_string(self.y) + ")" + suffix
    }

    fn tick(self: Point) -> unit {
        ()
    }

    fn bump(self: Point, delta: int32) -> int32 {
        self.x + self.y + delta
    }
}

impl Display for Flag {
    fn show(self: Flag) -> string {
        if self.value { "Flag(true)" } else { "Flag(false)" }
    }

    fn show_with(self: Flag, prefix: string, suffix: string) -> string {
        if self.value { prefix + "Flag(true)" + suffix } else { prefix + "Flag(false)" + suffix }
    }

    fn tick(self: Flag) -> unit {
        ()
    }

    fn bump(self: Flag, delta: int32) -> int32 {
        if self.value { delta } else { -delta }
    }
}

impl Display for Counter {
    fn show(self: Counter) -> string {
        "Counter(" + int32_to_string(ref_get(self.cell)) + ")"
    }

    fn show_with(self: Counter, prefix: string, suffix: string) -> string {
        prefix + "Counter(" + int32_to_string(ref_get(self.cell)) + ")" + suffix
    }

    fn tick(self: Counter) -> unit {
        let next = ref_get(self.cell) + 1;
        let _ = ref_set(self.cell, next);
        ()
    }

    fn bump(self: Counter, delta: int32) -> int32 {
        let next = ref_get(self.cell) + delta;
        let _ = ref_set(self.cell, next);
        next
    }
}

fn show_dyn(x: dyn Display) -> string {
    Display::show_with(x, "<", ">")
}

fn call_via_closure(x: dyn Display, tag: string) -> string {
    let f: (dyn Display, string) -> string =
        |v: dyn Display, t: string| Display::show_with(v, t, t);
    f(x, tag)
}

fn make_renderer(tag: string) -> (dyn Display) -> string {
    |x: dyn Display| Display::show_with(x, tag, tag)
}

fn bump_and_show(x: dyn Display, delta: int32) -> string {
    let _ = Display::tick(x);
    Display::show_with(x, "[", "]") + ":" + int32_to_string(Display::bump(x, delta))
}

fn main() -> unit {
    let p1 = Point { x: 1, y: 2 };
    let p2 = Point { x: 3, y: 4 };
    let f1 = Flag { value: true };
    let f2 = Flag { value: false };
    let c = Counter { cell: ref(10) };

    let dp1: dyn Display = p1;
    let dp2: dyn Display = p2;
    let df1: dyn Display = f1;
    let df2: dyn Display = f2;
    let dc: dyn Display = c;

    let render_star = make_renderer("*");
    let render_angle = make_renderer("<");

    let s0 = show_dyn(dp2);
    let s1 = call_via_closure(df2, "*");
    let s2 = render_star(dp1) + "|" + render_angle(df1);

    let v: Vec[dyn Display] = vec_new();
    let v = vec_push(v, dp1);
    let v = vec_push(v, df1);
    let v = vec_push(v, dc);
    let vlen = vec_len(v);
    let delta = match vlen {
        2 => 3,
        _ => 5,
    };

    let _ = string_println(s0);
    let _ = string_println(s1);
    let _ = string_println(s2);

    let i: Ref[int32] = ref(0);
    let _ = while ref_get(i) < 3 {
        let line = bump_and_show(dc, delta);
        string_println(line);
        let _ = ref_set(i, ref_get(i) + 1);
    };

    let _ = string_println("len:" + int32_to_string(vlen));
    let _ = string_println("delta:" + int32_to_string(delta));
    ()
}
`;
export {
  n as default
};
