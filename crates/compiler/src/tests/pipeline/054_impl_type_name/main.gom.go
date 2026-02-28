package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Point struct {
    x int32
    y int32
}

type Shape interface {
    isShape()
}

type Unit struct {}

func (_ Unit) isShape() {}

type Location struct {
    _0 Point
}

func (_ Location) isShape() {}

func _goml_trait_impl_TypeName_Point_type_name(self__0 Point) string {
    var mtmp0 Point
    var x1 int32
    var x2 int32
    var y__2 int32
    var x__1 int32
    var t7 string
    var prefix__3 string
    var t8 string
    var t9 string
    var t10 string
    var t11 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            mtmp0 = self__0
            x1 = mtmp0.x
            x2 = mtmp0.y
            y__2 = x2
            x__1 = x1
            t7 = int32_to_string(x__1)
            prefix__3 = "Point(" + t7
            t8 = prefix__3 + ", "
            t9 = int32_to_string(y__2)
            t10 = t8 + t9
            t11 = t10 + ")"
            return t11
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_TypeName_Shape_type_name(self__4 Shape) string {
    var jp13 string
    var x3 Point
    var point__5 Point
    var t14 string
    var t15 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch self__4.(type) {
            case Unit:
                pc = 2
            case Location:
                pc = 3
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return jp13
        case 2:
            jp13 = "Unit"
            pc = 1
        case 3:
            x3 = self__4.(Location)._0
            point__5 = x3
            t14 = _goml_trait_impl_TypeName_Point_type_name(point__5)
            t15 = "Shape::" + t14
            jp13 = t15
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func show_point(point__6 Point) string {
    var t16 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t16 = _goml_trait_impl_TypeName_Point_type_name(point__6)
            return t16
        default:
            panic("invalid pc")
        }
    }
}

func show_shape(shape__7 Shape) string {
    var t17 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t17 = _goml_trait_impl_TypeName_Shape_type_name(shape__7)
            return t17
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var point__8 Point
    var t18 string
    var mtmp4 struct{}
    var unit_shape__9 Shape
    var t19 string
    var mtmp5 struct{}
    var t20 Point
    var location_shape__10 Shape
    var t21 string
    var mtmp6 struct{}
    _ = mtmp4
    _ = mtmp5
    _ = mtmp6
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            point__8 = Point{
                x: 7,
                y: 9,
            }
            t18 = show_point(point__8)
            string_println(t18)
            unit_shape__9 = Unit{}
            t19 = show_shape(unit_shape__9)
            string_println(t19)
            t20 = Point{
                x: 1,
                y: 2,
            }
            location_shape__10 = Location{
                _0: t20,
            }
            t21 = show_shape(location_shape__10)
            string_println(t21)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
