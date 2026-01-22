package Main
file main.gom
  fn show_int(local/1/0: string, local/1/1: int32) {
      let _ = string_println(label/0 + int32_to_string(value/1));
      ();
    }
    
    fn show_bool(local/1/2: string, local/1/3: bool) {
      let _ = string_println(label/2 + bool_to_string(value/3));
      ();
    }
    
    fn main() {
      let local/1/4 = 10;
      let local/1/5 = base/4 + 5;
      let local/1/6 = sum/5 - 3;
      let local/1/7 = diff/6 * 2;
      let local/1/8 = prod/7 / 4;
      let _ = def/1/0("sum=", sum/5);
      let _ = def/1/0("diff=", diff/6);
      let _ = def/1/0("prod=", prod/7);
      let _ = def/1/0("quot=", quot/8);
      let local/1/9 = true && false;
      let local/1/10 = true || false;
      let local/1/11 = !false;
      let local/1/12 = !and_result/9 && match sum/5 + prod/7 * base/4 - prod/7 / 2 {
          0 => false,
          _ => true,
      } || !match diff/6 - quot/8 + base/4 - sum/5 / 2 {
          0 => false,
          _ => true,
      };
      let _ = def/1/1("and=", and_result/9);
      let _ = def/1/1("or=", or_result/10);
      let _ = def/1/1("not=", not_result/11);
      let _ = def/1/1("mixed=", mixed/12);
      ();
    }