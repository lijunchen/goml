fn main() -> unit {
  let local/0: Vec[int32] = vec_new();
  let local/1 = vec_push(vi/0, 42);
  let local/2 = vec_get(vi/1, 0);
  let local/3 = vec_len(vi/1);
  let local/4: Vec[string] = vec_new();
  let local/5 = vec_push(vs/4, "hello");
  let local/6 = vec_push(vs/5, "world");
  let local/7 = vec_get(vs/6, 1);
  let local/8 = vec_len(vs/6);
  let local/9: Vec[bool] = vec_new();
  let local/10 = vec_push(vb/9, true);
  let local/11 = vec_push(vb/10, false);
  let local/12 = vec_get(vb/11, 0);
  let local/13 = vec_len(vb/11);
  let _ = string_println(int32_to_string(val_i/2));
  let _ = string_println(int32_to_string(len_i/3));
  let _ = string_println(val_s/7);
  let _ = string_println(int32_to_string(len_s/8));
  let _ = string_println(bool_to_string(val_b/12));
  let _ = string_println(int32_to_string(len_b/13));
  ();
}

