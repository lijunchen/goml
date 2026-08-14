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
    var x188 int32 = self__0.x
    var x189 int32 = self__0.y
    var t196 string
    var inline229 string = _goml_runtime_core_int32_to_string(x188)
    t196 = inline229
    var prefix__3 string = "Point(" + t196
    var t197 string = prefix__3 + ", "
    var t198 string
    var inline227 string = _goml_runtime_core_int32_to_string(x189)
    t198 = inline227
    var t199 string = t197 + t198
    var t200 string = t199 + ")"
    return t200
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    switch self__4.(type) {
    case Unit:
        return "Unit"
    case Location:
        var x190 Point = self__4.(Location)._0
        var t205 string
        var inline232 int32 = x190.x
        var inline233 int32 = x190.y
        var inline236 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline232)
        var inline237 string = "Point(" + inline236
        var inline238 string = inline237 + ", "
        var inline239 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline233)
        var inline240 string = inline238 + inline239
        var inline241 string = inline240 + ")"
        t205 = inline241
        var t206 string = "Shape::" + t205
        return t206
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t214 string
    var inline273 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__8)
    t214 = inline273
    var inline270 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline270)
    var unit_shape__9 Shape = Unit{}
    var t215 string
    var inline268 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(unit_shape__9)
    t215 = inline268
    var inline265 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline265)
    var t216 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t216,
    }
    var t217 string
    var inline263 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(location_shape__10)
    t217 = inline263
    var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline260)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t220 string = _goml_runtime_core_int32_to_string(self__33)
    return t220
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
