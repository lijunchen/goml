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
    var ret20 string
    var mtmp0 Point = self__0
    var x1 int32 = mtmp0.x
    var x2 int32 = mtmp0.y
    var y__2 int32 = x2
    var x__1 int32 = x1
    var t11 string = int32_to_string(x__1)
    var prefix__3 string = "Point(" + t11
    var t13 string = prefix__3 + ", "
    var t14 string = int32_to_string(y__2)
    var t12 string = t13 + t14
    ret20 = t12 + ")"
    return ret20
}

func _goml_trait_impl_TypeName_Shape_type_name(self__4 Shape) string {
    var ret21 string
    switch self__4 := self__4.(type) {
    case Unit:
        ret21 = "Unit"
    case Location:
        var x3 Point = self__4._0
        var point__5 Point = x3
        var t15 string = _goml_trait_impl_TypeName_Point_type_name(point__5)
        ret21 = "Shape::" + t15
    }
    return ret21
}

func show_point(point__6 Point) string {
    var ret22 string
    ret22 = _goml_trait_impl_TypeName_Point_type_name(point__6)
    return ret22
}

func show_shape(shape__7 Shape) string {
    var ret23 string
    ret23 = _goml_trait_impl_TypeName_Shape_type_name(shape__7)
    return ret23
}

func main0() struct{} {
    var ret24 struct{}
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t16 string = show_point(point__8)
    string_println(t16)
    var unit_shape__9 Shape = Unit{}
    var t17 string = show_shape(unit_shape__9)
    string_println(t17)
    var t18 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t18,
    }
    var t19 string = show_shape(location_shape__10)
    string_println(t19)
    ret24 = struct{}{}
    return ret24
}

func main() {
    main0()
}
