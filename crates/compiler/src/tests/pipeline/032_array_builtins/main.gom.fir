fn update_array(local/0: [int32; 3]) -> [int32; 3] {
  array_set(arr/0, 1, 42);
}

fn read_array(local/1: [int32; 3]) -> int32 {
  array_get(arr/1, 1);
}

fn main() {
  let local/2 = [1, 2, 3];
  let local/3 = def/0(arr/2);
  let local/4 = def/1(updated/3);
  let _ = string_println(int32_to_string(value/4));
  ();
}

