package Main
file main.gom
  fn make_array() -> [int32; 3] {
      [1, 2, 3];
    }
    
    fn main() {
      let local/1/0 = def/1/0();
      let local/1/1 = [4, 5, 6];
      let _ = string_print("array literal");
      let _ = arr/0;
      let _ = inline/1;
      ();
    }