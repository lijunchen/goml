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
    var retv160 string
    var mtmp152 Point = self__0
    var x153 int32 = mtmp152.x
    var x154 int32 = mtmp152.y
    var y__2 int32 = x154
    var x__1 int32 = x153
    var t161 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var prefix__3 string = "Point(" + t161
    var t162 string = prefix__3 + ", "
    var t163 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t164 string = t162 + t163
    var t165 string = t164 + ")"
    retv160 = t165
    return retv160
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    var retv167 string
    var jp169 string
    switch self__4.(type) {
    case Unit:
        jp169 = "Unit"
    case Location:
        var x155 Point = self__4.(Location)._0
        var point__5 Point = x155
        var t170 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__5)
        var t171 string = "Shape::" + t170
        jp169 = t171
    default:
        panic("non-exhaustive match")
    }
    retv167 = jp169
    return retv167
}

func show_point(point__6 Point) string {
    var retv173 string
    var t174 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__6)
    retv173 = t174
    return retv173
}

func show_shape(shape__7 Shape) string {
    var retv176 string
    var t177 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(shape__7)
    retv176 = t177
    return retv176
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t179 string = show_point(point__8)
    println__T_string(t179)
    var unit_shape__9 Shape = Unit{}
    var t180 string = show_shape(unit_shape__9)
    println__T_string(t180)
    var t181 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t181,
    }
    var t182 string = show_shape(location_shape__10)
    println__T_string(t182)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv184 string
    var t185 string = _goml_runtime_core_int32_to_string(self__6)
    retv184 = t185
    return retv184
}

func println__T_string(value__1 string) struct{} {
    var t187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t187)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv190 string
    retv190 = self__38
    return retv190
}

func main() {
    main0()
}
