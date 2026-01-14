package Main
file main.gom
  fn test_int_comparisons() {
      let local/1/0: int32 = 10;
      let local/1/1: int32 = 20;
      let local/1/2: int32 = 10;
      let local/1/3 = a/0 < b/1;
      let _ = string_println("10 < 20: " + bool_to_string(less/3));
      let local/1/4 = b/1 > a/0;
      let _ = string_println("20 > 10: " + bool_to_string(greater/4));
      let local/1/5 = a/0 <= b/1;
      let _ = string_println("10 <= 20: " + bool_to_string(less_eq1/5));
      let local/1/6 = a/0 <= c/2;
      let _ = string_println("10 <= 10: " + bool_to_string(less_eq2/6));
      let local/1/7 = b/1 >= a/0;
      let _ = string_println("20 >= 10: " + bool_to_string(greater_eq1/7));
      let local/1/8 = c/2 >= a/0;
      let _ = string_println("10 >= 10: " + bool_to_string(greater_eq2/8));
      let local/1/9 = a/0 == c/2;
      let _ = string_println("10 == 10: " + bool_to_string(eq1/9));
      let local/1/10 = a/0 == b/1;
      let _ = string_println("10 == 20: " + bool_to_string(eq2/10));
      let local/1/11 = a/0 != b/1;
      let _ = string_println("10 != 20: " + bool_to_string(neq1/11));
      let local/1/12 = a/0 != c/2;
      let _ = string_println("10 != 10: " + bool_to_string(neq2/12));
      ();
    }
    
    fn test_float_comparisons() {
      let local/1/13: float64 = 3.14;
      let local/1/14: float64 = 2.71;
      let local/1/15: float64 = 3.14;
      let local/1/16 = y/14 < x/13;
      let _ = string_println("2.71 < 3.14: " + bool_to_string(less/16));
      let local/1/17 = x/13 > y/14;
      let _ = string_println("3.14 > 2.71: " + bool_to_string(greater/17));
      let local/1/18 = y/14 <= x/13;
      let _ = string_println("2.71 <= 3.14: " + bool_to_string(less_eq1/18));
      let local/1/19 = x/13 <= z/15;
      let _ = string_println("3.14 <= 3.14: " + bool_to_string(less_eq2/19));
      let local/1/20 = x/13 >= y/14;
      let _ = string_println("3.14 >= 2.71: " + bool_to_string(greater_eq1/20));
      let local/1/21 = z/15 >= x/13;
      let _ = string_println("3.14 >= 3.14: " + bool_to_string(greater_eq2/21));
      let local/1/22 = x/13 == z/15;
      let _ = string_println("3.14 == 3.14: " + bool_to_string(eq1/22));
      let local/1/23 = x/13 == y/14;
      let _ = string_println("3.14 == 2.71: " + bool_to_string(eq2/23));
      let local/1/24 = x/13 != y/14;
      let _ = string_println("3.14 != 2.71: " + bool_to_string(neq1/24));
      let local/1/25 = x/13 != z/15;
      let _ = string_println("3.14 != 3.14: " + bool_to_string(neq2/25));
      ();
    }
    
    fn main() {
      let _ = string_println("=== Integer Comparisons ===");
      let _ = def/1/0();
      let _ = string_println("");
      let _ = string_println("=== Float Comparisons ===");
      let _ = def/1/1();
      ();
    }