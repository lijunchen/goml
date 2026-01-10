fn main() {
  let local/0: int32 = 1i32;
  let local/1: int8 = 1i8;
  let _ = string_println("int32: " + int32_to_string(x/0));
  let _ = string_println("int8: " + int8_to_string(y/1));
  ();
}

