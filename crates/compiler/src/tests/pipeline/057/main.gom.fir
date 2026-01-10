struct S {
}

impl S {
  fn method1(local/0: Self) -> unit {
    ();
  }
  fn method2(local/1: Self) -> string {
    "S";
  }
}

fn main() {
  let local/2 = S {};
  s/2.method1();
}

