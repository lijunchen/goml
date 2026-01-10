fn make_counter() -> (() -> int32, () -> unit) {
  let local/0 = ref(0);
  let local/2 = || {
      let local/1 = ref_get(cell/0) + 1;
      let _ = ref_set(cell/0, next/1);
      next/1;
  };
  let local/3 = || {
      let _ = ref_set(cell/0, 0);
      ();
  };
  (next/2, reset/3);
}

fn main() {
  let local/4 = def/0();
  let (local/5, local/6) = counter/4;
  let local/7 = next/5();
  let local/8 = next/5();
  let _ = reset/6();
  let local/9 = next/5();
  let local/10 = def/0();
  let (local/11, _) = new_counter/10;
  let local/12 = new_next/11();
  let _ = string_println(int32_to_string(first/7));
  let _ = string_println(int32_to_string(second/8));
  let _ = string_println(int32_to_string(third/9));
  let _ = string_println(int32_to_string(fourth/12));
  ();
}

