fn choose(local/0: bool, local/1: int32, local/2: int32) -> int32 {
  if flag/0 {
      {
          x/1;
      }
  } else {
      {
          y/2;
      }
  };
}

fn main() {
  let local/3 = def/0(true, 10, 99);
  let local/4 = def/0(false, 10, 99);
  let _ = string_println("yes=" + int32_to_string(yes/3));
  let _ = string_println("no=" + int32_to_string(no/4));
  ();
}

