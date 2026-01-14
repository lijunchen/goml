package Main
file main.gom
  enum List[T] {
        Nil,
        Cons(T, List[T])
    }
    
    fn list_length(local/1/0: List[T]) -> int32 {
      match xs/0 {
          ctor(def/1/0::v0) => 0,
          ctor(def/1/0::v1)(_, local/1/1) => 1 + def/1/1(tail/1),
      };
    }
    
    fn int_list_length(local/1/2: List[int32]) -> int32 {
      match xs/2 {
          ctor(def/1/0::v0) => 0,
          ctor(def/1/0::v1)(_, local/1/3) => 1 + def/1/2(tail/3),
      };
    }
    
    fn main() {
      let local/1/4 = ctor(def/1/0::v1)(1, ctor(def/1/0::v0));
      let local/1/5 = def/1/1(x/4);
      let _ = string_println(int32_to_string(length/5));
      let local/1/6 = ctor(def/1/0::v1)(1, ctor(def/1/0::v1)(2, ctor(def/1/0::v0)));
      let local/1/7 = def/1/1(x/6);
      let _ = string_println(int32_to_string(length/7));
      let local/1/8 = ctor(def/1/0::v1)(0, ctor(def/1/0::v1)(1, ctor(def/1/0::v1)(2, ctor(def/1/0::v0))));
      let local/1/9 = def/1/2(x/8);
      let _ = string_println(int32_to_string(length/9));
      let local/1/10 = ctor(def/1/0::v1)((), ctor(def/1/0::v0));
      let local/1/11 = def/1/1(x/10);
      let _ = string_println(int32_to_string(length/11));
      let local/1/12 = ctor(def/1/0::v1)((), ctor(def/1/0::v1)((), ctor(def/1/0::v0)));
      let local/1/13 = def/1/1(x/12);
      let _ = string_println(int32_to_string(length/13));
      let local/1/14 = ctor(def/1/0::v1)(true, ctor(def/1/0::v1)(false, ctor(def/1/0::v0)));
      let local/1/15 = def/1/1(x/14);
      let _ = string_println(int32_to_string(length/15));
      ();
    }