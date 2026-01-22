package Main
file main.gom
  enum IntList {
        Nil,
        Cons(int32, IntList)
    }
    
    fn print_int_list(local/1/0: IntList) {
      match xs/0 {
          ctor(def/1/0::v0) => string_print("Nil"),
          ctor(def/1/0::v1)(local/1/1, local/1/2) => {
                let _ = string_print("Cons");
                let _ = string_print("(");
                let _ = string_print(int32_to_string(x/1));
                let _ = string_print(", ");
                let _ = def/1/1(xs/2);
                let _ = string_print(")");
                ();
            },
      };
    }
    
    fn int_list_rev_aux(local/1/3: IntList, local/1/4: IntList) -> IntList {
      match xs/3 {
          ctor(def/1/0::v0) => acc/4,
          ctor(def/1/0::v1)(local/1/5, local/1/6) => def/1/2(tail/6, ctor(def/1/0::v1)(head/5, acc/4)),
      };
    }
    
    fn int_list_rev(local/1/7: IntList) -> IntList {
      def/1/2(xs/7, ctor(def/1/0::v0));
    }
    
    fn int_list_length(local/1/8: IntList) -> int32 {
      match xs/8 {
          ctor(def/1/0::v0) => 0,
          ctor(def/1/0::v1)(_, local/1/9) => 1 + def/1/4(xs/9),
      };
    }
    
    fn print_int_list_length(local/1/10: IntList) {
      let _ = string_print("Length: ");
      let _ = string_println(int32_to_string(def/1/4(xs/10)));
      ();
    }
    
    fn main() {
      let local/1/11 = ctor(def/1/0::v0);
      let _ = def/1/1(x/11);
      let _ = string_println("");
      let _ = def/1/5(x/11);
      let local/1/12 = ctor(def/1/0::v1)(1, ctor(def/1/0::v0));
      let _ = def/1/1(x/12);
      let _ = string_println("");
      let _ = def/1/5(x/12);
      let local/1/13 = ctor(def/1/0::v1)(1, ctor(def/1/0::v1)(2, ctor(def/1/0::v1)(3, ctor(def/1/0::v0))));
      let _ = def/1/1(x/13);
      let _ = string_println("");
      let _ = def/1/5(x/13);
      let local/1/14 = def/1/3(x/13);
      let _ = def/1/1(y/14);
      let _ = string_println("");
      ();
    }