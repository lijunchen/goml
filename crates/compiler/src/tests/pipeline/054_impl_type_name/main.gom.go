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
    var retv15 string
    var mtmp7 Point = self__0
    var x8 int32 = mtmp7.x
    var x9 int32 = mtmp7.y
    var y__2 int32 = x9
    var x__1 int32 = x8
    var t16 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var prefix__3 string = "Point(" + t16
    var t17 string = prefix__3 + ", "
    var t18 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t19 string = t17 + t18
    var t20 string = t19 + ")"
    retv15 = t20
    return retv15
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    var retv22 string
    var jp24 string
    switch self__4.(type) {
    case Unit:
        jp24 = "Unit"
    case Location:
        var x10 Point = self__4.(Location)._0
        var point__5 Point = x10
        var t25 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__5)
        var t26 string = "Shape::" + t25
        jp24 = t26
    default:
        panic("non-exhaustive match")
    }
    retv22 = jp24
    return retv22
}

func show_point(point__6 Point) string {
    var retv28 string
    var t29 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__6)
    retv28 = t29
    return retv28
}

func show_shape(shape__7 Shape) string {
    var retv31 string
    var t32 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(shape__7)
    retv31 = t32
    return retv31
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t34 string = show_point(point__8)
    println__T_string(t34)
    var unit_shape__9 Shape = Unit{}
    var t35 string = show_shape(unit_shape__9)
    println__T_string(t35)
    var t36 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t36,
    }
    var t37 string = show_shape(location_shape__10)
    println__T_string(t37)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv39 string
    var t40 string = _goml_runtime_core_int32_to_string(self__2)
    retv39 = t40
    return retv39
}

func println__T_string(value__1 string) struct{} {
    var t42 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t42)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv45 string
    retv45 = self__9
    return retv45
}

func main() {
    main0()
}
