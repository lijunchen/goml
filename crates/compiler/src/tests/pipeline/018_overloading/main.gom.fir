trait Arith {
  fn add(Self, Self) -> int32;
  fn less(Self, Self) -> bool;
}

impl Arith for int32 {
  fn add(local/0: int32, local/1: int32) -> int32 {
    self/0 + other/1;
  }
  fn less(local/2: int32, local/3: int32) -> bool {
    self/2 < other/3;
  }
}

trait ToString {
  fn to_string(Self) -> string;
}

impl ToString for int32 {
  fn to_string(local/4: int32) -> string {
    int32_to_string(self/4);
  }
}

impl ToString for bool {
  fn to_string(local/5: bool) -> string {
    bool_to_string(self/5);
  }
}

trait Output {
  fn output(Self) -> unit;
}

impl Output for int32 {
  fn output(local/6: int32) -> unit {
    string_println(ToString::to_string(self/6));
  }
}

impl Output for bool {
  fn output(local/7: bool) -> unit {
    string_println(ToString::to_string(self/7));
  }
}

fn id(local/8: T) -> T {
  x/8;
}

fn main() {
  let local/9 = def/8(1);
  let local/10 = def/8(2);
  let local/11 = Arith::add(a/9, b/10);
  let _ = Output::output(c/11);
  let local/12 = def/8(3);
  let local/13 = def/8(4);
  let local/14 = Arith::less(a/12, b/13);
  let _ = Output::output(c/14);
  let _ = def/8("abc");
  let _ = def/8(true);
  ();
}

