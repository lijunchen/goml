package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
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

func _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(self__0 Point) string {
    var retv12 string
    var mtmp4 Point = self__0
    var x5 int32 = mtmp4.x
    var x6 int32 = mtmp4.y
    var y__2 int32 = x6
    var x__1 int32 = x5
    var t13 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var prefix__3 string = "Point(" + t13
    var t14 string = prefix__3 + ", "
    var t15 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t16 string = t14 + t15
    var t17 string = t16 + ")"
    retv12 = t17
    return retv12
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    var retv19 string
    var jp21 string
    switch self__4.(type) {
    case Unit:
        jp21 = "Unit"
    case Location:
        var x7 Point = self__4.(Location)._0
        var point__5 Point = x7
        var t22 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__5)
        var t23 string = "Shape::" + t22
        jp21 = t23
    default:
        panic("non-exhaustive match")
    }
    retv19 = jp21
    return retv19
}

func show_point(point__6 Point) string {
    var retv25 string
    var t26 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__6)
    retv25 = t26
    return retv25
}

func show_shape(shape__7 Shape) string {
    var retv28 string
    var t29 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(shape__7)
    retv28 = t29
    return retv28
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t31 string = show_point(point__8)
    println__T_string(t31)
    var unit_shape__9 Shape = Unit{}
    var t32 string = show_shape(unit_shape__9)
    println__T_string(t32)
    var t33 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t33,
    }
    var t34 string = show_shape(location_shape__10)
    println__T_string(t34)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv36 string
    var t37 string = _goml_runtime_core_int32_to_string(self__2)
    retv36 = t37
    return retv36
}

func println__T_string(value__1 string) struct{} {
    var t39 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t39)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv42 string
    retv42 = self__9
    return retv42
}

func main() {
    main0()
}
