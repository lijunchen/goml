const e = `fn sum_to(limit: int32) -> int32 {
  let acc = ref(0);
  let i = ref(0);
  let _ = (while ref_get(i) < limit {
    let current = ref_get(i);
    let _ = ref_set(acc, ref_get(acc) + current);
    let _ = ref_set(i, current + 1);
    ()
  });
  ref_get(acc)
}

fn sum_even(limit: int32) -> int32 {
  let acc = ref(0);
  let i = ref(0);
  let is_even = ref(true);
  let _ = (while ref_get(i) < limit {
    let current = ref_get(i);
    let _ = ref_set(i, current + 1);
    let add_now = ref_get(is_even);
    let _ = ref_set(is_even, !add_now);
    if add_now {
      ref_set(acc, ref_get(acc) + current)
    } else {
      ()
    }
  });
  ref_get(acc)
}

fn main() {
  let first = sum_to(5);
  let evens = sum_even(6);
  let _ = string_println("sum_to(5)=" + int32_to_string(first));
  let _ = string_println("sum_even(6)=" + int32_to_string(evens));
  ()
}
`;
export {
  e as default
};
