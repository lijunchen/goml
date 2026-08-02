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
    var x156 int32 = self__0.x
    var x157 int32 = self__0.y
    var t164 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x156)
    var prefix__3 string = "Point(" + t164
    var t165 string = prefix__3 + ", "
    var t166 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x157)
    var t167 string = t165 + t166
    var t168 string = t167 + ")"
    return t168
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    switch self__4.(type) {
    case Unit:
        return "Unit"
    case Location:
        var x158 Point = self__4.(Location)._0
        var t173 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(x158)
        var t174 string = "Shape::" + t173
        return t174
    default:
        panic("non-exhaustive match")
    }
}

func show_point(point__6 Point) string {
    var t177 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__6)
    return t177
}

func show_shape(shape__7 Shape) string {
    var t180 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(shape__7)
    return t180
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
    var t188 string = _goml_runtime_core_int32_to_string(self__6)
    return t188
}

func println__T_string(value__1 string) struct{} {
    var t190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t190)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
