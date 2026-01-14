package Main
file main.gom
  struct Point {
        x: int32,
        y: int32,
    }
    
    enum Shape {
        Unit,
        Location(Point)
    }
    
    trait TypeName {
      fn type_name(Self) -> string;
    }
    
    impl TypeName for Point {
      fn type_name(local/1/0: Self) -> string {
        let Point { x: local/1/1, y: local/1/2 } = self/0;
        let local/1/3 = "Point(" + int32_to_string(x/1);
        prefix/3 + ", " + int32_to_string(y/2) + ")";
      }
    }
    
    impl TypeName for Shape {
      fn type_name(local/1/4: Self) -> string {
        match self/4 {
            ctor(def/1/1::v0) => "Unit",
            ctor(def/1/1::v1)(local/1/5) => "Shape::" + TypeName::type_name(point/5),
        };
      }
    }
    
    fn show_point(local/1/6: Point) -> string {
      TypeName::type_name(point/6);
    }
    
    fn show_shape(local/1/7: Shape) -> string {
      TypeName::type_name(shape/7);
    }
    
    fn main() -> unit {
      let local/1/8 = Point {
          x: 7,
          y: 9
      };
      let _ = string_println(def/1/5(point/8));
      let local/1/9 = ctor(def/1/1::v0);
      let _ = string_println(def/1/6(unit_shape/9));
      let local/1/10 = ctor(def/1/1::v1)(Point {
          x: 1,
          y: 2
      });
      let _ = string_println(def/1/6(location_shape/10));
      ();
    }