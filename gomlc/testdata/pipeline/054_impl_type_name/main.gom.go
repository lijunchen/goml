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
    var t164 string
    var inline197 string = _goml_runtime_core_int32_to_string(x156)
    t164 = inline197
    var prefix__3 string = "Point(" + t164
    var t165 string = prefix__3 + ", "
    var t166 string
    var inline195 string = _goml_runtime_core_int32_to_string(x157)
    t166 = inline195
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
        var t173 string
        var inline200 int32 = x158.x
        var inline201 int32 = x158.y
        var inline204 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline200)
        var inline205 string = "Point(" + inline204
        var inline206 string = inline205 + ", "
        var inline207 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline201)
        var inline208 string = inline206 + inline207
        var inline209 string = inline208 + ")"
        t173 = inline209
        var t174 string = "Shape::" + t173
        return t174
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t182 string
    var inline241 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__8)
    t182 = inline241
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline238)
    var unit_shape__9 Shape = Unit{}
    var t183 string
    var inline236 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(unit_shape__9)
    t183 = inline236
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline233)
    var t184 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t184,
    }
    var t185 string
    var inline231 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(location_shape__10)
    t185 = inline231
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
    _goml_runtime_core_string_println(inline228)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t188 string = _goml_runtime_core_int32_to_string(self__6)
    return t188
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
