package Main
file main.gom
  struct Point {
        x: int32,
        y: int32,
    }
    
    struct Flag {
        value: bool,
    }
    
    struct Counter {
        cell: Ref[int32],
    }
    
    trait Display {
      fn show(Self) -> string;
      fn show_with(Self, string, string) -> string;
      fn tick(Self) -> unit;
      fn bump(Self, int32) -> int32;
    }
    
    impl Display for Point {
      fn show(local/1/0: Point) -> string {
        "Point(" + int32_to_string(self/0.x) + "," + int32_to_string(self/0.y) + ")";
      }
      fn show_with(local/1/1: Point, local/1/2: string, local/1/3: string) -> string {
        prefix/2 + "Point(" + int32_to_string(self/1.x) + "," + int32_to_string(self/1.y) + ")" + suffix/3;
      }
      fn tick(local/1/4: Point) -> unit {
        ();
      }
      fn bump(local/1/5: Point, local/1/6: int32) -> int32 {
        self/5.x + self/5.y + delta/6;
      }
    }
    
    impl Display for Flag {
      fn show(local/1/7: Flag) -> string {
        if self/7.value {
            "Flag(true)";
        } else {
            "Flag(false)";
        };
      }
      fn show_with(local/1/8: Flag, local/1/9: string, local/1/10: string) -> string {
        if self/8.value {
            prefix/9 + "Flag(true)" + suffix/10;
        } else {
            prefix/9 + "Flag(false)" + suffix/10;
        };
      }
      fn tick(local/1/11: Flag) -> unit {
        ();
      }
      fn bump(local/1/12: Flag, local/1/13: int32) -> int32 {
        if self/12.value {
            delta/13;
        } else {
            -delta/13;
        };
      }
    }
    
    impl Display for Counter {
      fn show(local/1/14: Counter) -> string {
        "Counter(" + int32_to_string(ref_get(self/14.cell)) + ")";
      }
      fn show_with(local/1/15: Counter, local/1/16: string, local/1/17: string) -> string {
        prefix/16 + "Counter(" + int32_to_string(ref_get(self/15.cell)) + ")" + suffix/17;
      }
      fn tick(local/1/18: Counter) -> unit {
        let local/1/19 = ref_get(self/18.cell) + 1;
        let _ = ref_set(self/18.cell, next/19);
        ();
      }
      fn bump(local/1/20: Counter, local/1/21: int32) -> int32 {
        let local/1/22 = ref_get(self/20.cell) + delta/21;
        let _ = ref_set(self/20.cell, next/22);
        next/22;
      }
    }
    
    fn show_dyn(local/1/23: dyn Display) -> string {
      Display::show_with(x/23, "<", ">");
    }
    
    fn call_via_closure(local/1/24: dyn Display, local/1/25: string) -> string {
      let local/1/28: (dyn Display, string) -> string = |local/1/26: dyn Display, local/1/27: string| Display::show_with(v/26, t/27, t/27);
      f/28(x/24, tag/25);
    }
    
    fn make_renderer(local/1/29: string) -> (dyn Display) -> string {
      |local/1/30: dyn Display| Display::show_with(x/30, tag/29, tag/29);
    }
    
    fn bump_and_show(local/1/31: dyn Display, local/1/32: int32) -> string {
      let _ = Display::tick(x/31);
      Display::show_with(x/31, "[", "]") + ":" + int32_to_string(Display::bump(x/31, delta/32));
    }
    
    fn main() -> unit {
      let local/1/33 = Point {
          x: 1,
          y: 2
      };
      let local/1/34 = Point {
          x: 3,
          y: 4
      };
      let local/1/35 = Flag {
          value: true
      };
      let local/1/36 = Flag {
          value: false
      };
      let local/1/37 = Counter {
          cell: ref(10)
      };
      let local/1/38: dyn Display = p1/33;
      let local/1/39: dyn Display = p2/34;
      let local/1/40: dyn Display = f1/35;
      let local/1/41: dyn Display = f2/36;
      let local/1/42: dyn Display = c/37;
      let local/1/43 = def/1/9("*");
      let local/1/44 = def/1/9("<");
      let local/1/45 = def/1/7(dp2/39);
      let local/1/46 = def/1/8(df2/41, "*");
      let local/1/47 = render_star/43(dp1/38) + "|" + render_angle/44(df1/40);
      let local/1/48: Vec[dyn Display] = vec_new();
      let local/1/49 = vec_push(v/48, dp1/38);
      let local/1/50 = vec_push(v/49, df1/40);
      let local/1/51 = vec_push(v/50, dc/42);
      let local/1/52 = vec_len(v/51);
      let local/1/53 = match vlen/52 {
          2 => 3,
          _ => 5,
      };
      let _ = string_println(s0/45);
      let _ = string_println(s1/46);
      let _ = string_println(s2/47);
      let local/1/54: Ref[int32] = ref(0);
      let _ = while ref_get(i/54) < 3 {
          {
              let local/1/55 = def/1/10(dc/42, delta/53);
              string_println(line/55);
              let _ = ref_set(i/54, ref_get(i/54) + 1);
              ();
          }
      };
      let _ = string_println("len:" + int32_to_string(vlen/52));
      let _ = string_println("delta:" + int32_to_string(delta/53));
      ();
    }