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
    var retv116 string
    var mtmp108 Point = self__0
    var x109 int32 = mtmp108.x
    var x110 int32 = mtmp108.y
    var y__2 int32 = x110
    var x__1 int32 = x109
    var t117 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var prefix__3 string = "Point(" + t117
    var t118 string = prefix__3 + ", "
    var t119 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t120 string = t118 + t119
    var t121 string = t120 + ")"
    retv116 = t121
    return retv116
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    var retv123 string
    var jp125 string
    switch self__4.(type) {
    case Unit:
        jp125 = "Unit"
    case Location:
        var x111 Point = self__4.(Location)._0
        var point__5 Point = x111
        var t126 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__5)
        var t127 string = "Shape::" + t126
        jp125 = t127
    default:
        panic("non-exhaustive match")
    }
    retv123 = jp125
    return retv123
}

func show_point(point__6 Point) string {
    var retv129 string
    var t130 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__6)
    retv129 = t130
    return retv129
}

func show_shape(shape__7 Shape) string {
    var retv132 string
    var t133 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(shape__7)
    retv132 = t133
    return retv132
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t135 string = show_point(point__8)
    println__T_string(t135)
    var unit_shape__9 Shape = Unit{}
    var t136 string = show_shape(unit_shape__9)
    println__T_string(t136)
    var t137 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t137,
    }
    var t138 string = show_shape(location_shape__10)
    println__T_string(t138)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv140 string
    var t141 string = _goml_runtime_core_int32_to_string(self__6)
    retv140 = t141
    return retv140
}

func println__T_string(value__1 string) struct{} {
    var t143 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t143)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv146 string
    retv146 = self__38
    return retv146
}

func main() {
    main0()
}
