fn my_int_equal(local/0: int32, local/1: int32) -> bool {
  !x/0 < y/1 && !y/1 < x/0;
}

fn sum(local/2: int32) -> int32 {
  if def/0(n/2, 1) {
      1;
  } else {
      n/2 + def/1(n/2 - 1);
  };
}

fn main() {
  string_println(int32_to_string(def/1(100)));
}

