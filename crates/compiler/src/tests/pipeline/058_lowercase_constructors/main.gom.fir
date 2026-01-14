package Main
file main.gom
  enum option[T] {
        some(T),
        none
    }
    
    struct point {
        x: int32,
        y: int32,
    }
    
    fn make_some(local/1/0: int32) -> option[int32] {
      ctor(def/1/0::v0)(value/0);
    }
    
    fn build_point(local/1/1: int32, local/1/2: int32) -> point {
      point {
          x: x/1,
          y: y/2
      };
    }
    
    fn magnitude(local/1/3: point) -> int32 {
      let point { x: local/1/4, y: local/1/5 } = p/3;
      x/4 + y/5;
    }
    
    fn main() -> int32 {
      let ctor(def/1/0::v0)(local/1/6) = def/1/2(5);
      let local/1/7 = def/1/3(result/6, 7);
      match ctor(def/1/0::v0)(pt/7.x) {
          ctor(def/1/0::v0)(local/1/8) => value/8 + def/1/4(pt/7),
          ctor(def/1/0::v1) => 0,
      };
    }