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
    
    fn show_both(local/1/21: T) -> string {
      Debug::show(x/21) + " / " + Display::show(x/21);
    }
    
    fn tag_text(local/1/22: Q) -> string {
      Debug::show(tag/22) + "#" + int32_to_string(tag/22.hash());
    }
    
    fn combine_scaled(local/1/23: T, local/1/24: T, local/1/25: int32) -> T {
      a/23.add(b/24).scale(factor/25);
    }
    
    fn report_pair(local/1/26: Q, local/1/27: T, local/1/28: T, local/1/29: T) -> string {
      let local/1/30 = Eq::eq(a/27, b/28);
      let local/1/31 = def/1/24(tag/26);
      let local/1/32 = def/1/23(combined/29);
      let local/1/33 = combined/29.hash();
      header/31 + " " + repr/32 + " | eq=" + def/1/22(same/30) + " | hash=" + int32_to_string(h/33);
    }
    
    fn full_report(local/1/34: Q, local/1/35: T, local/1/36: T) -> string {
      let local/1/37 = def/1/25(a/35, b/36, 2);
      def/1/26(tag/34, a/35, b/36, combined/37);
    }
    
    fn sum_and_tag(local/1/38: Q, local/1/39: T, local/1/40: T, local/1/41: T) -> string {
      let local/1/42 = x/39.add(y/40).add(z/41);
      let local/1/43 = def/1/24(tag/38);
      let local/1/44 = total/42.hash();
      header/43 + " " + total/42.inspect() + " @" + int32_to_string(h/44);
    }
    
    fn main() {
      let _ = string_println(def/1/27(7, 10, 32));
      let _ = string_println(def/1/27(Boxed {
              value: 99
          }, Boxed {
              value: 3
          }, Boxed {
              value: 4
          }));
      let _ = string_println(def/1/28(0, 1, 2, 3));
      let _ = string_println(def/1/28(Boxed {
              value: 1
          }, Boxed {
              value: 5
          }, Boxed {
              value: 6
          }, Boxed {
              value: 7
          }));
      ();
    }