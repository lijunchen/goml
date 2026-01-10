trait ToString {
  fn to_string(Self) -> string;
}

impl ToString for int32 {
  fn to_string(local/0: int32) -> string {
    int32_to_string(self/0);
  }
}

impl ToString for bool {
  fn to_string(local/1: bool) -> string {
    bool_to_string(self/1);
  }
}

impl ToString for (int32, int32) {
  fn to_string(local/2: (int32, int32)) -> string {
    "(?, ?)";
  }
}

fn main() {
  let local/3 = 123;
  let _ = string_println(ToString::to_string(x/3));
  let local/4 = true;
  let _ = string_println(ToString::to_string(x/4));
  let local/5 = (3, 4);
  let _ = string_println(ToString::to_string(x/5));
  ();
}

