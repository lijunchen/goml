struct Shape {
}

impl Shape {
  fn name(local/0: Self) -> string {
    "Shape";
  }
  fn rename(local/1: Self, local/2: string) -> string {
    self/1.name() + suffix/2;
  }
  fn join(local/3: Self, local/4: string, local/5: string) -> string {
    left/4 + self/3.name() + right/5;
  }
}

fn announce(local/6: Shape) -> unit {
  let local/7 = shape/6.name();
  let local/8 = shape/6.rename("!");
  let local/9 = shape/6.join(base/7, with_suffix/8);
  let _ = string_println(combined/9);
  ();
}

fn main() -> unit {
  let local/10 = Shape {};
  def/2(shape/10);
  ();
}

