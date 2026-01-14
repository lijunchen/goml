package Main
file main.gom
  fn double(local/1/0: int32) -> int32 {
      x/0 * 2;
    }
    
    fn increment(local/1/1: int32) -> int32 {
      x/1 + 1;
    }
    
    fn apply_once(local/1/2: (int32) -> int32, local/1/3: int32) -> int32 {
      f/2(value/3);
    }
    
    fn compose(local/1/4: (int32) -> int32, local/1/5: (int32) -> int32, local/1/6: int32) -> int32 {
      f/4(g/5(value/6));
    }
    
    fn main() {
      let local/1/7 = def/1/0;
      let local/1/8 = def/1/2(local/7, 4);
      let local/1/9 = def/1/3(def/1/0, def/1/1, first/8);
      let local/1/11 = |local/1/10: int32| def/1/2(def/1/1, value/10);
      let local/1/12 = closure_apply/11(composed/9);
      let local/1/15 = |local/1/13: (int32) -> int32, local/1/14: int32| def/1/2(func_to_call/13, value/14);
      let local/1/16 = global_invoker/15(def/1/0, 3);
      let local/1/18 = |local/1/17: int32| def/1/3(def/1/0, def/1/1, value/17);
      let local/1/19 = composer_closure/18(5);
      let local/1/20 = string_println;
      let _ = printer/20(int32_to_string(composed/9));
      let _ = printer/20(int32_to_string(closure_result/12));
      let _ = printer/20(int32_to_string(invoked_with_global/16));
      let _ = printer/20(int32_to_string(composed_by_closure/19));
      ();
    }