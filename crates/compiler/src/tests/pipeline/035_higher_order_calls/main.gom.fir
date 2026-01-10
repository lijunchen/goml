fn double(local/0: int32) -> int32 {
  x/0 * 2;
}

fn increment(local/1: int32) -> int32 {
  x/1 + 1;
}

fn chooser(local/2: bool) -> (int32) -> int32 {
  if flag/2 {
      {
          def/0;
      }
  } else {
      {
          def/1;
      }
  };
}

fn main() {
  let local/3 = [def/0, def/1];
  let local/4 = array_get(xs/3, 0);
  let local/5 = array_get(xs/3, 1);
  let _ = string_println(int32_to_string(g/5(f/4(10))));
  let local/6 = def/2(true);
  let local/7 = chosen/6(5);
  let local/8 = def/2(false)(5);
  let local/9 = string_println;
  let _ = printer/9(int32_to_string(applied/7));
  let _ = printer/9(int32_to_string(direct/8));
  ();
}

