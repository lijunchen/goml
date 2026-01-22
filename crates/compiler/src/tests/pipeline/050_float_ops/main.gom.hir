package Main
file main.gom
  fn show32(local/1/0: string, local/1/1: float32) {
      let local/1/2 = label/0 + float32_to_string(value/1);
      let _ = string_println(message/2);
      ();
    }
    
    fn show64(local/1/3: string, local/1/4: float64) {
      let local/1/5 = label/3 + float64_to_string(value/4);
      let _ = string_println(message/5);
      ();
    }
    
    fn lerp32(local/1/6: float32, local/1/7: float32, local/1/8: float32) -> float32 {
      let local/1/9 = b/7 - a/6;
      a/6 + delta/9 * weight/8;
    }
    
    fn midpoint_energy(local/1/10: float64, local/1/11: float64) -> float64 {
      let local/1/12 = x/10 * x/10 + y/11 * y/11;
      sum/12 / 2.0f64;
    }
    
    fn main() {
      let local/1/13: float32 = 1.25f32;
      let local/1/14: float32 = 5.75f32;
      let local/1/15: float32 = 0.5f32;
      let local/1/16: float32 = 2.0f32;
      let local/1/17 = def/1/2(start32/13, end32/14, half/15);
      let local/1/18 = -end32/14;
      let local/1/19 = end32/14 / scale/16;
      let local/1/20 = start32/13 < end32/14;
      let local/1/21: float64 = 6.5f64;
      let local/1/22: float64 = 3.5f64;
      let local/1/23: float64 = 0.25f64;
      let local/1/24: float64 = def/1/3(dx/21, dy/22);
      let local/1/25 = -dx/21;
      let local/1/26: float64 = energy/24 + dy/22 - dx/21 * quarter/23;
      let local/1/27: float64 = 4.0f64;
      let local/1/28 = adjusted/26 < threshold/27;
      def/1/0("mid32=", mid32/17);
      def/1/0("neg_end32=", neg_end32/18);
      def/1/0("ratio32=", ratio32/19);
      let _ = string_println("less32=" + bool_to_string(less32/20));
      def/1/1("energy=", energy/24);
      def/1/1("neg_dx=", neg_dx/25);
      def/1/1("adjusted=", adjusted/26);
      let _ = string_println("less64=" + bool_to_string(less64/28));
      ();
    }