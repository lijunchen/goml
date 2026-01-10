fn a_value() -> int8 {
  90i8;
}

fn b_value() -> int8 {
  -20i8;
}

fn c_value() -> int8 {
  3i8;
}

fn show_int8(local/0: string, local/1: int8) {
  let _ = string_println(label/0 + int8_to_string(value/1));
  ();
}

fn show_bool(local/2: string, local/3: bool) {
  let _ = string_println(label/2 + bool_to_string(value/3));
  ();
}

fn main() {
  let local/4 = def/0();
  let local/5 = def/1();
  let local/6 = def/2();
  let local/7 = a/4 + b/5;
  let local/8 = a/4 - c/6;
  let local/9 = b/5 * c/6;
  let local/10 = a/4 / c/6;
  let local/11 = -b/5;
  let local/12 = b/5 < a/4;
  let _ = def/3("a=", a/4);
  let _ = def/3("b=", b/5);
  let _ = def/3("c=", c/6);
  let _ = def/3("sum=", sum/7);
  let _ = def/3("diff=", diff/8);
  let _ = def/3("prod=", prod/9);
  let _ = def/3("quot=", quot/10);
  let _ = def/3("neg=", neg/11);
  let _ = def/4("b<a=", less/12);
  ();
}

