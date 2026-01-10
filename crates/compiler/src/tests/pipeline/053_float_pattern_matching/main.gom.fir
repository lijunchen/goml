enum FloatEvent {
    Sample32(string, float32),
    Sample64(string, float64)
}

fn summarize(local/0: FloatEvent) -> string {
  match event/0 {
      ctor(def/0::v0)(local/1, local/2) => label/1 + float32_to_string(value/2),
      ctor(def/0::v1)(local/3, local/4) => label/3 + float64_to_string(value/4),
  };
}

fn compare(local/5: (float32, float64)) -> string {
  match values/5 {
      (local/6, local/7) => {
            let local/8: float32 = 1.0f32;
            let local/9: float64 = 5.0f64;
            let local/10 = left/6 < limit32/8;
            let local/11 = right/7 < limit64/9;
            "left<1?=" + bool_to_string(less_left/10) + ",right<5?=" + bool_to_string(less_right/11);
        },
  };
}

fn main() {
  let local/12: float32 = 0.5f32;
  let local/13: float32 = 2.25f32;
  let local/14: float64 = 9.5f64;
  let local/15 = ctor(def/0::v0)("f32=", first_value/12);
  let local/16 = ctor(def/0::v0)("f32_b=", second_value/13);
  let local/17 = ctor(def/0::v1)("f64=", third_value/14);
  let local/18: (float32, float64) = (0.75f32, 4.0f64);
  let local/19: (float32, float64) = (1.5f32, 7.25f64);
  let local/20 = def/1(first/15) + def/1(second/16) + def/1(third/17) + def/2(tuple/18) + def/2(tuple_other/19);
  let _ = string_println(message/20);
  ();
}

