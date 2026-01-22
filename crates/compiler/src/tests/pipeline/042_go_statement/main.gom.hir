package Main
file main.gom
  fn child(local/1/0: Ref[int32]) -> unit {
      ref_set(signal/0, 1);
    }
    
    fn main() -> unit {
      let local/1/1 = ref(0);
      go || {
          def/1/0(signal/1);
      };
      while ref_get(signal/1) < 1 {
          {
              ();
          }
      };
      string_println("main");
      ();
    }