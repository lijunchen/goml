fn test_int_comparisons() {
  let local/0: int32 = 10;
  let local/1: int32 = 20;
  let local/2: int32 = 10;
  let local/3 = a/0 < b/1;
  let _ = string_println("10 < 20: " + bool_to_string(less/3));
  let local/4 = b/1 > a/0;
  let _ = string_println("20 > 10: " + bool_to_string(greater/4));
  let local/5 = a/0 <= b/1;
  let _ = string_println("10 <= 20: " + bool_to_string(less_eq1/5));
  let local/6 = a/0 <= c/2;
  let _ = string_println("10 <= 10: " + bool_to_string(less_eq2/6));
  let local/7 = b/1 >= a/0;
  let _ = string_println("20 >= 10: " + bool_to_string(greater_eq1/7));
  let local/8 = c/2 >= a/0;
  let _ = string_println("10 >= 10: " + bool_to_string(greater_eq2/8));
  let local/9 = a/0 == c/2;
  let _ = string_println("10 == 10: " + bool_to_string(eq1/9));
  let local/10 = a/0 == b/1;
  let _ = string_println("10 == 20: " + bool_to_string(eq2/10));
  let local/11 = a/0 != b/1;
  let _ = string_println("10 != 20: " + bool_to_string(neq1/11));
  let local/12 = a/0 != c/2;
  let _ = string_println("10 != 10: " + bool_to_string(neq2/12));
  ();
}

fn test_float_comparisons() {
  let local/13: float64 = 3.14;
  let local/14: float64 = 2.71;
  let local/15: float64 = 3.14;
  let local/16 = y/14 < x/13;
  let _ = string_println("2.71 < 3.14: " + bool_to_string(less/16));
  let local/17 = x/13 > y/14;
  let _ = string_println("3.14 > 2.71: " + bool_to_string(greater/17));
  let local/18 = y/14 <= x/13;
  let _ = string_println("2.71 <= 3.14: " + bool_to_string(less_eq1/18));
  let local/19 = x/13 <= z/15;
  let _ = string_println("3.14 <= 3.14: " + bool_to_string(less_eq2/19));
  let local/20 = x/13 >= y/14;
  let _ = string_println("3.14 >= 2.71: " + bool_to_string(greater_eq1/20));
  let local/21 = z/15 >= x/13;
  let _ = string_println("3.14 >= 3.14: " + bool_to_string(greater_eq2/21));
  let local/22 = x/13 == z/15;
  let _ = string_println("3.14 == 3.14: " + bool_to_string(eq1/22));
  let local/23 = x/13 == y/14;
  let _ = string_println("3.14 == 2.71: " + bool_to_string(eq2/23));
  let local/24 = x/13 != y/14;
  let _ = string_println("3.14 != 2.71: " + bool_to_string(neq1/24));
  let local/25 = x/13 != z/15;
  let _ = string_println("3.14 != 3.14: " + bool_to_string(neq2/25));
  ();
}

fn main() {
  let _ = string_println("=== Integer Comparisons ===");
  let _ = def/0();
  let _ = string_println("");
  let _ = string_println("=== Float Comparisons ===");
  let _ = def/1();
  ();
}

