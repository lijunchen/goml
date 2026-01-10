fn main() {
  let local/0 = (true, true);
  match a/0 {
      (true, false) => string_print(int32_to_string(123)),
      (false, true) => string_print(int32_to_string(456)),
      _ => string_print(int32_to_string(789)),
  };
}

