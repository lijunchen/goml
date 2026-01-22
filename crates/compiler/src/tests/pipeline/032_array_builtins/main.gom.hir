package Main
file main.gom
  fn update_array(local/1/0: [int32; 3]) -> [int32; 3] {
      array_set(arr/0, 1, 42);
    }
    
    fn read_array(local/1/1: [int32; 3]) -> int32 {
      array_get(arr/1, 1);
    }
    
    fn main() {
      let local/1/2 = [1, 2, 3];
      let local/1/3 = def/1/0(arr/2);
      let local/1/4 = def/1/1(updated/3);
      let _ = string_println(int32_to_string(value/4));
      ();
    }