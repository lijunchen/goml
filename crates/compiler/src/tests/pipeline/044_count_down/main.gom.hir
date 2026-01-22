package Main
file main.gom
  extern type Duration
    
    extern "go" "time" sleep(d: Duration) -> unit
    
    extern "go" "time" duration(nanos: int32) -> Duration
    
    fn main() {
      let local/1/0 = ref(0);
      go || {
          while ref_get(counter/0) < 10 {
              {
                  string_println("hello");
                  def/1/1(def/1/2(1000));
                  ref_set(counter/0, ref_get(counter/0) + 1);
                  ();
              }
          };
      };
      while ref_get(counter/0) < 10 {
          {
              ();
          }
      };
    }