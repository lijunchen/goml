fn main() {
  let local/0 = (true, false);
  let _ = match a/0 {
      (true, local/1) => string_print(bool_to_string(b/1)),
      _ => (),
  };
  let local/2 = (true, true);
  match c/2 {
      (true, local/3) => string_print(bool_to_string(d/3)),
      _ => (),
  };
}

