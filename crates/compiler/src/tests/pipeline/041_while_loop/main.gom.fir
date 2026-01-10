fn sum_to(local/0: int32) -> int32 {
  let local/1 = ref(0);
  let local/2 = ref(0);
  let _ = while ref_get(i/2) < limit/0 {
      {
          let local/3 = ref_get(i/2);
          let _ = ref_set(acc/1, ref_get(acc/1) + current/3);
          let _ = ref_set(i/2, current/3 + 1);
          ();
      }
  };
  ref_get(acc/1);
}

fn sum_even(local/4: int32) -> int32 {
  let local/5 = ref(0);
  let local/6 = ref(0);
  let local/7 = ref(true);
  let _ = while ref_get(i/6) < limit/4 {
      {
          let local/8 = ref_get(i/6);
          let _ = ref_set(i/6, current/8 + 1);
          let local/9 = ref_get(is_even/7);
          let _ = ref_set(is_even/7, !add_now/9);
          if add_now/9 {
              {
                  ref_set(acc/5, ref_get(acc/5) + current/8);
              }
          } else {
              {
                  ();
              }
          };
      }
  };
  ref_get(acc/5);
}

fn main() {
  let local/10 = def/0(5);
  let local/11 = def/1(6);
  let _ = string_println("sum_to(5)=" + int32_to_string(first/10));
  let _ = string_println("sum_even(6)=" + int32_to_string(evens/11));
  ();
}

