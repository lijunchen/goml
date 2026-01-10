enum IntList {
    Nil,
    Cons(int32, IntList)
}

fn print_int_list(local/0: IntList) {
  match xs/0 {
      ctor/0 => string_print("Nil"),
      ctor/1(local/1, local/2) => {
            let _ = string_print("Cons");
            let _ = string_print("(");
            let _ = string_print(int32_to_string(x/1));
            let _ = string_print(", ");
            let _ = def/1(xs/2);
            let _ = string_print(")");
            ();
        },
  };
}

fn int_list_rev_aux(local/3: IntList, local/4: IntList) -> IntList {
  match xs/3 {
      ctor/0 => acc/4,
      ctor/1(local/5, local/6) => def/2(tail/6, ctor/1(head/5, acc/4)),
  };
}

fn int_list_rev(local/7: IntList) -> IntList {
  def/2(xs/7, ctor/0);
}

fn int_list_length(local/8: IntList) -> int32 {
  match xs/8 {
      ctor/0 => 0,
      ctor/1(_, local/9) => 1 + def/4(xs/9),
  };
}

fn print_int_list_length(local/10: IntList) {
  let _ = string_print("Length: ");
  let _ = string_println(int32_to_string(def/4(xs/10)));
  ();
}

fn main() {
  let local/11 = ctor/0;
  let _ = def/1(x/11);
  let _ = string_println("");
  let _ = def/5(x/11);
  let local/12 = ctor/1(1, ctor/0);
  let _ = def/1(x/12);
  let _ = string_println("");
  let _ = def/5(x/12);
  let local/13 = ctor/1(1, ctor/1(2, ctor/1(3, ctor/0)));
  let _ = def/1(x/13);
  let _ = string_println("");
  let _ = def/5(x/13);
  let local/14 = def/3(x/13);
  let _ = def/1(y/14);
  let _ = string_println("");
  ();
}

