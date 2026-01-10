fn fib(local/0: int32) -> int32 {
  match x/0 < 2 {
      false => def/0(x/0 - 1) + def/0(x/0 - 2),
      true => 1,
  };
}

fn main() {
  string_print(int32_to_string(def/0(10)));
}

