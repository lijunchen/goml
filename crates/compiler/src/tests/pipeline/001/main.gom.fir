fn main() {
  let _ = ();
  let local/0 = (true, false, (true, false));
  let local/5 = match a/0 {
      (local/1, local/2, (local/3, local/4)) => w/4,
  };
  let _ = string_print(bool_to_string(b/5));
  ();
}

