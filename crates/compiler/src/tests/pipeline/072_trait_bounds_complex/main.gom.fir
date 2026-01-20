package Main
file main.gom
  trait Display {
      fn show(Self) -> string;
    }
    
    trait Debug {
      fn show(Self) -> string;
    }
    
    trait Eq {
      fn eq(Self, Self) -> bool;
    }
    
    trait Hash {
      fn hash(Self) -> int32;
    }
    
    trait Add {
      fn add(Self, Self) -> Self;
    }
    
    trait Scale {
      fn scale(Self, int32) -> Self;
    }
    
    trait Inspect {
      fn inspect(Self) -> string;
    }
    
    struct Boxed {
        value: int32,
    }
    
    impl Display for int32 {
      fn show(local/1/0: int32) -> string {
        int32_to_string(self/0);
      }
    }
    
    impl Debug for int32 {
      fn show(local/1/1: int32) -> string {
        "i32(" + int32_to_string(self/1) + ")";
      }
    }
    
    impl Eq for int32 {
      fn eq(local/1/2: int32, local/1/3: int32) -> bool {
        self/2 == other/3;
      }
    }
    
    impl Hash for int32 {
      fn hash(local/1/4: int32) -> int32 {
        self/4 * 16777619 + 216613626;
      }
    }
    
    impl Add for int32 {
      fn add(local/1/5: int32, local/1/6: int32) -> int32 {
        self/5 + other/6;
      }
    }
    
    impl Scale for int32 {
      fn scale(local/1/7: int32, local/1/8: int32) -> int32 {
        self/7 * factor/8;
      }
    }
    
    impl Inspect for int32 {
      fn inspect(local/1/9: int32) -> string {
        "<" + int32_to_string(self/9) + ">";
      }
    }
    
    impl Display for Boxed {
      fn show(local/1/10: Boxed) -> string {
        "Boxed(" + int32_to_string(self/10.value) + ")";
      }
    }
    
    impl Debug for Boxed {
      fn show(local/1/11: Boxed) -> string {
        "Boxed{value=" + int32_to_string(self/11.value) + "}";
      }
    }
    
    impl Eq for Boxed {
      fn eq(local/1/12: Boxed, local/1/13: Boxed) -> bool {
        self/12.value == other/13.value;
      }
    }
    
    impl Hash for Boxed {
      fn hash(local/1/14: Boxed) -> int32 {
        self/14.value * 31 + 7 * 1315423911;
      }
    }
    
    impl Add for Boxed {
      fn add(local/1/15: Boxed, local/1/16: Boxed) -> Boxed {
        Boxed {
            value: self/15.value + other/16.value
        };
      }
    }
    
    impl Scale for Boxed {
      fn scale(local/1/17: Boxed, local/1/18: int32) -> Boxed {
        Boxed {
            value: self/17.value * factor/18
        };
      }
    }
    
    impl Inspect for Boxed {
      fn inspect(local/1/19: Boxed) -> string {
        "[" + int32_to_string(self/19.value) + "]";
      }
    }
    
    fn bool_text(local/1/20: bool) -> string {
      if x/20 {
          "true";
      } else {
          "false";
      };
    }
    
    fn combine_and_render(local/1/21: T, local/1/22: T) -> string {
      let local/1/23 = a/21.add(b/22).scale(2);
      let local/1/24 = Eq::eq(a/21, b/22);
      let local/1/25 = a/21.hash() + b/22.hash();
      let local/1/26 = Debug::show(scaled/23);
      let local/1/27 = Display::show(scaled/23);
      dbg/26 + " | " + disp/27 + " | eq=" + def/1/22(same/24) + " | hash=" + int32_to_string(sum_hash/25);
    }
    
    fn sum_and_inspect(local/1/28: T, local/1/29: T, local/1/30: T) -> string {
      let local/1/31 = x/28.add(y/29).add(z/30);
      let local/1/32 = total/31.hash();
      total/31.inspect() + " @" + int32_to_string(h/32);
    }
    
    fn main() {
      let _ = string_println(def/1/23(10, 32));
      let _ = string_println(def/1/23(Boxed {
              value: 3
          }, Boxed {
              value: 4
          }));
      let _ = string_println(def/1/24(1, 2, 3));
      let _ = string_println(def/1/24(Boxed {
              value: 5
          }, Boxed {
              value: 6
          }, Boxed {
              value: 7
          }));
      ();
    }