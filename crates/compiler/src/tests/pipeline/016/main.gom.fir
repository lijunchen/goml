enum List[T] {
    Nil,
    Cons(T, List[T])
}

fn list_length(local/0: List[T]) -> int32 {
  match xs/0 {
      ctor/0 => 0,
      ctor/1(_, local/1) => 1 + def/1(tail/1),
  };
}

fn int_list_length(local/2: List[int32]) -> int32 {
  match xs/2 {
      ctor/0 => 0,
      ctor/1(_, local/3) => 1 + def/2(tail/3),
  };
}

fn main() {
  let local/4 = ctor/1(1, ctor/0);
  let local/5 = def/1(x/4);
  let _ = string_println(int32_to_string(length/5));
  let local/6 = ctor/1(1, ctor/1(2, ctor/0));
  let local/7 = def/1(x/6);
  let _ = string_println(int32_to_string(length/7));
  let local/8 = ctor/1(0, ctor/1(1, ctor/1(2, ctor/0)));
  let local/9 = def/2(x/8);
  let _ = string_println(int32_to_string(length/9));
  let local/10 = ctor/1((), ctor/0);
  let local/11 = def/1(x/10);
  let _ = string_println(int32_to_string(length/11));
  let local/12 = ctor/1((), ctor/1((), ctor/0));
  let local/13 = def/1(x/12);
  let _ = string_println(int32_to_string(length/13));
  let local/14 = ctor/1(true, ctor/1(false, ctor/0));
  let local/15 = def/1(x/14);
  let _ = string_println(int32_to_string(length/15));
  ();
}

