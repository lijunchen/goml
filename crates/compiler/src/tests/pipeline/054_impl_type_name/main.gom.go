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

type GoError = error

func _goml_trait_impl_TypeName_Point_type_name(self__0 Point) string {
    var retv8 string
    var mtmp0 Point = self__0
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var y__2 int32 = x2
    var x__1 int32 = x1
    var t9 string = int32_to_string(x__1)
    var prefix__3 string = "Point(" + t9
    var t10 string = prefix__3 + ", "
    var t11 string = int32_to_string(y__2)
    var t12 string = t10 + t11
    var t13 string = t12 + ")"
    retv8 = t13
    return retv8
}

func _goml_trait_impl_TypeName_Shape_type_name(self__4 Shape) string {
    var retv15 string
    var jp17 string
    switch self__4.(type) {
    case Unit:
        jp17 = "Unit"
    case Location:
        var x3 Point = self__4.(Location)._0
        var point__5 Point = x3
        var t18 string = _goml_trait_impl_TypeName_Point_type_name(point__5)
        var t19 string = "Shape::" + t18
        jp17 = t19
    default:
        panic("non-exhaustive match")
    }
    retv15 = jp17
    return retv15
}

func show_point(point__6 Point) string {
    var retv21 string
    var t22 string = _goml_trait_impl_TypeName_Point_type_name(point__6)
    retv21 = t22
    return retv21
}

func show_shape(shape__7 Shape) string {
    var retv24 string
    var t25 string = _goml_trait_impl_TypeName_Shape_type_name(shape__7)
    retv24 = t25
    return retv24
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t27 string = show_point(point__8)
    string_println(t27)
    var unit_shape__9 Shape = Unit{}
    var t28 string = show_shape(unit_shape__9)
    string_println(t28)
    var t29 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t29,
    }
    var t30 string = show_shape(location_shape__10)
    string_println(t30)
    return struct{}{}
}

func main() {
    main0()
}
