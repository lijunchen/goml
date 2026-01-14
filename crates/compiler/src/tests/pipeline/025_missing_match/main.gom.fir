package Main
file main.gom
  struct Point {
        x: int32,
        y: int32,
    }
    
    fn main() {
      let local/1/0 = Point {
          x: 0,
          y: 0
      };
      let Point { x: 10, y: 8 } = p0/0;
      ();
    }