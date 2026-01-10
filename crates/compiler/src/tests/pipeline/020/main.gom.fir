struct Point {
    x: int32,
    y: int32,
}

struct Wrapper[T] {
    value: T,
}

enum Shape[T] {
    Dot(Point),
    Wrapped(Wrapper[T]),
    Origin
}

fn bounce_int(local/0: Shape[int32]) -> Shape[int32] {
  match shape/0 {
      ctor/0(local/1) => ctor/0(point/1),
      ctor/1(local/2) => ctor/1(inner/2),
      ctor/2 => ctor/2,
  };
}

fn wrap_unit(local/3: Wrapper[unit]) -> Shape[unit] {
  ctor/1(value/3);
}

fn pick(local/4: bool, local/5: Point, local/6: Wrapper[int32]) -> Shape[int32] {
  match flag/4 {
      true => ctor/0(point/5),
      false => ctor/1(wrapper/6),
  };
}

fn describe(local/7: Shape[T]) -> int32 {
  match shape/7 {
      ctor/0(_) => 1,
      ctor/1(_) => 2,
      ctor/2 => 0,
  };
}

fn point32_to_string(local/8: Point) -> string {
  let Point { x: local/9, y: local/10 } = point/8;
  let local/11 = "Point { x: " + int32_to_string(x/9);
  let local/12 = with_x/11 + ", y: ";
  let local/13 = with_y_label/12 + int32_to_string(y/10);
  with_y/13 + " }";
}

fn wrapper_int32_to_string(local/14: Wrapper[int32]) -> string {
  let Wrapper { value: local/15 } = wrapper/14;
  let local/16 = "Wrapper[int32] { value: " + int32_to_string(value/15);
  prefix/16 + " }";
}

fn wrapper_unit_to_string(local/17: Wrapper[unit]) -> string {
  let Wrapper { value: local/18 } = wrapper/17;
  let local/19 = "Wrapper[unit] { value: " + unit_to_string(value/18);
  prefix/19 + " }";
}

fn shape_int32_to_string(local/20: Shape[int32]) -> string {
  match shape/20 {
      ctor/0(local/21) => {
            let local/22 = "Shape::Dot(" + def/7(point/21);
            prefix/22 + ")";
        },
      ctor/1(local/23) => {
            let local/24 = "Shape::Wrapped(" + def/8(wrapper/23);
            prefix/24 + ")";
        },
      ctor/2 => "Shape::Origin",
  };
}

fn shape_unit_to_string(local/25: Shape[unit]) -> string {
  match shape/25 {
      ctor/0(local/26) => {
            let local/27 = "Shape::Dot(" + def/7(point/26);
            prefix/27 + ")";
        },
      ctor/1(local/28) => {
            let local/29 = "Shape::Wrapped(" + def/9(wrapper/28);
            prefix/29 + ")";
        },
      ctor/2 => "Shape::Origin",
  };
}

fn main() {
  let _ = string_println(def/7(Point(3, 4)));
  let _ = string_println(def/8(Wrapper(7)));
  let _ = string_println(def/9(Wrapper(())));
  let local/30 = def/3(ctor/2);
  let _ = string_println(def/10(ctor/0(Point(3, 4))));
  let _ = string_println(def/10(ctor/1(Wrapper(7))));
  let _ = string_println(def/10(bounced_origin/30));
  let _ = string_println(def/11(ctor/0(Point(3, 4))));
  let _ = string_println(def/11(ctor/1(Wrapper(()))));
  let _ = string_println(def/11(ctor/2));
  let _ = def/6(def/3(ctor/2));
  string_println("struct enums!");
}

