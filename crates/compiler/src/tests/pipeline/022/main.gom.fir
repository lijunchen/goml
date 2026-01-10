fn match_string(local/0: string) -> int32 {
  match s/0 {
      "hello" => 1,
      "world" => 2,
      _ => 3,
  };
}

fn wildcard_position(local/1: string) -> int32 {
  match s/1 {
      _ => 4,
      "world" => 5,
  };
}

fn repeated_string(local/2: string) -> int32 {
  match s/2 {
      "hello" => 6,
      "hello" => 7,
      _ => 8,
  };
}

fn main() {
  let _ = string_println(int32_to_string(def/0("hello")));
  let _ = string_println(int32_to_string(def/0("planet")));
  let _ = string_println(int32_to_string(def/1("world")));
  let _ = string_println(int32_to_string(def/1("sun")));
  let _ = string_println(int32_to_string(def/2("hello")));
  string_println(int32_to_string(def/2("mars")));
}

