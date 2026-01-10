fn double(local/0: int32) -> int32 {
  x/0 * 2;
}

fn increment(local/1: int32) -> int32 {
  x/1 + 1;
}

fn apply_once(local/2: (int32) -> int32, local/3: int32) -> int32 {
  f/2(value/3);
}

fn compose(local/4: (int32) -> int32, local/5: (int32) -> int32, local/6: int32) -> int32 {
  f/4(g/5(value/6));
}

fn main() {
  let local/7 = def/0;
  let local/8 = def/2(local/7, 4);
  let local/9 = def/3(def/0, def/1, first/8);
  let local/11 = |local/10: int32| def/2(def/1, value/10);
  let local/12 = closure_apply/11(composed/9);
  let local/15 = |local/13: (int32) -> int32, local/14: int32| def/2(func_to_call/13, value/14);
  let local/16 = global_invoker/15(def/0, 3);
  let local/18 = |local/17: int32| def/3(def/0, def/1, value/17);
  let local/19 = composer_closure/18(5);
  let local/20 = string_println;
  let _ = printer/20(int32_to_string(composed/9));
  let _ = printer/20(int32_to_string(closure_result/12));
  let _ = printer/20(int32_to_string(invoked_with_global/16));
  let _ = printer/20(int32_to_string(composed_by_closure/19));
  ();
}

