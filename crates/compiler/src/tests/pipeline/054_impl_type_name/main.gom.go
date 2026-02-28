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
    var mtmp0 Point = self__0
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var y__2 int32 = x2
    var x__1 int32 = x1
    var t7 string = int32_to_string(x__1)
    var prefix__3 string = "Point(" + t7
    var t8 string = prefix__3 + ", "
    var t9 string = int32_to_string(y__2)
    var t10 string = t8 + t9
    var t11 string = t10 + ")"
    return t11
}

func _goml_trait_impl_TypeName_Shape_type_name(self__4 Shape) string {
    var jp13 string
    switch self__4.(type) {
    case Unit:
        jp13 = "Unit"
    case Location:
        var x3 Point = self__4.(Location)._0
        var point__5 Point = x3
        var t14 string = _goml_trait_impl_TypeName_Point_type_name(point__5)
        var t15 string = "Shape::" + t14
        jp13 = t15
    default:
        panic("non-exhaustive match")
    }
    return jp13
}

func show_point(point__6 Point) string {
    var t16 string = _goml_trait_impl_TypeName_Point_type_name(point__6)
    return t16
}

func show_shape(shape__7 Shape) string {
    var t17 string = _goml_trait_impl_TypeName_Shape_type_name(shape__7)
    return t17
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t18 string = show_point(point__8)
    string_println(t18)
    var unit_shape__9 Shape = Unit{}
    var t19 string = show_shape(unit_shape__9)
    string_println(t19)
    var t20 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t20,
    }
    var t21 string = show_shape(location_shape__10)
    string_println(t21)
    return struct{}{}
}

func main() {
    main0()
}
