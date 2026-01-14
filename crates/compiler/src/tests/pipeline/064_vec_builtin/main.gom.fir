package Main
file main.gom
  fn main() -> unit {
      let local/1/0: Vec[int32] = vec_new();
      let local/1/1 = vec_push(v/0, 10);
      let local/1/2 = vec_push(v/1, 20);
      let local/1/3 = vec_push(v/2, 30);
      let local/1/4 = vec_get(v/3, 0);
      let local/1/5 = vec_get(v/3, 1);
      let local/1/6 = vec_get(v/3, 2);
      let local/1/7 = vec_len(v/3);
      let _ = string_println(int32_to_string(first/4));
      let _ = string_println(int32_to_string(second/5));
      let _ = string_println(int32_to_string(third/6));
      let _ = string_println(int32_to_string(len/7));
      ();
    }