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
    var retv163 string
    var mtmp155 Point = self__0
    var x156 int32 = mtmp155.x
    var x157 int32 = mtmp155.y
    var y__2 int32 = x157
    var x__1 int32 = x156
    var t164 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var prefix__3 string = "Point(" + t164
    var t165 string = prefix__3 + ", "
    var t166 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t167 string = t165 + t166
    var t168 string = t167 + ")"
    retv163 = t168
    return retv163
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    var retv170 string
    var jp172 string
    switch self__4.(type) {
    case Unit:
        jp172 = "Unit"
    case Location:
        var x158 Point = self__4.(Location)._0
        var point__5 Point = x158
        var t173 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__5)
        var t174 string = "Shape::" + t173
        jp172 = t174
    default:
        panic("non-exhaustive match")
    }
    retv170 = jp172
    return retv170
}

func show_point(point__6 Point) string {
    var retv176 string
    var t177 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__6)
    retv176 = t177
    return retv176
}

func show_shape(shape__7 Shape) string {
    var retv179 string
    var t180 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(shape__7)
    retv179 = t180
    return retv179
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t182 string = show_point(point__8)
    println__T_string(t182)
    var unit_shape__9 Shape = Unit{}
    var t183 string = show_shape(unit_shape__9)
    println__T_string(t183)
    var t184 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t184,
    }
    var t185 string = show_shape(location_shape__10)
    println__T_string(t185)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv187 string
    var t188 string = _goml_runtime_core_int32_to_string(self__6)
    retv187 = t188
    return retv187
}

func println__T_string(value__1 string) struct{} {
    var t190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t190)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv193 string
    retv193 = self__38
    return retv193
}

func main() {
    main0()
}
