fn bump(local/0: Ref[int32]) -> int32 {
  let _ = ref_set(cell/0, ref_get(cell/0) + 1);
  ref_get(cell/0);
}

fn flip(local/1: Ref[bool]) -> bool {
  let local/2 = ref_get(flag/1);
  let _ = ref_set(flag/1, !current/2);
  ref_get(flag/1);
}

fn nested_total(local/3: Ref[Ref[int32]]) -> int32 {
  let local/4 = ref_get(cell/3);
  let local/5 = ref_get(inner/4);
  let _ = ref_set(inner/4, before/5 + 2);
  before/5 + ref_get(inner/4);
}

fn alias_bump(local/6: Ref[int32]) -> int32 {
  let local/7 = cell/6;
  let _ = ref_set(alias/7, ref_get(alias/7) + 5);
  ref_get(alias/7);
}

fn pair_sum() -> int32 {
  let local/8 = ref(4);
  let local/9 = ref(6);
  let _ = ref_set(first/8, ref_get(first/8) + ref_get(second/9));
  ref_get(first/8) + ref_get(second/9);
}

fn reassign_nested(local/10: Ref[Ref[int32]]) -> int32 {
  let local/11 = ref_get(nested/10);
  let _ = ref_set(inner/11, ref_get(inner/11) + 7);
  ref_get(inner/11);
}

fn main() {
  let local/12 = ref(39);
  let local/13 = ref(false);
  let local/14 = ref(ref(3));
  let local/15 = def/0(counter/12);
  let local/16 = def/1(toggler/13);
  let local/17 = def/1(toggler/13);
  let local/18 = ref_get(nested/14);
  let _ = ref_set(inner/18, ref_get(inner/18) + bumped/15);
  let local/19 = def/2(nested/14);
  let local/20 = def/3(counter/12);
  let local/21 = def/4();
  let local/22 = def/5(nested/14);
  let local/23 = !false;
  let _ = string_println(int32_to_string(bumped/15 + ref_get(counter/12)));
  let _ = string_println(int32_to_string(nested_total_val/19 + alias_total/20 + reassigned/22));
  let _ = string_println(int32_to_string(pair_total/21));
  let _ = string_println(bool_to_string(flipped/16 && flipped_again/17 && bool_check/23));
  ();
}

