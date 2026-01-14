package Main
file main.gom
  fn fib(local/1/0: int32) -> int32 {
      match x/0 < 2 {
          false => def/1/0(x/0 - 1) + def/1/0(x/0 - 2),
          true => 1,
      };
    }
    
    fn main() {
      string_print(int32_to_string(def/1/0(10)));
    }