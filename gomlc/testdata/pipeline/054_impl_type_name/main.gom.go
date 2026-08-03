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
    var x178 int32 = self__0.x
    var x179 int32 = self__0.y
    var t186 string
    var inline219 string = _goml_runtime_core_int32_to_string(x178)
    t186 = inline219
    var prefix__3 string = "Point(" + t186
    var t187 string = prefix__3 + ", "
    var t188 string
    var inline217 string = _goml_runtime_core_int32_to_string(x179)
    t188 = inline217
    var t189 string = t187 + t188
    var t190 string = t189 + ")"
    return t190
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    switch self__4.(type) {
    case Unit:
        return "Unit"
    case Location:
        var x180 Point = self__4.(Location)._0
        var t195 string
        var inline222 int32 = x180.x
        var inline223 int32 = x180.y
        var inline226 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline222)
        var inline227 string = "Point(" + inline226
        var inline228 string = inline227 + ", "
        var inline229 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline223)
        var inline230 string = inline228 + inline229
        var inline231 string = inline230 + ")"
        t195 = inline231
        var t196 string = "Shape::" + t195
        return t196
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t204 string
    var inline263 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__8)
    t204 = inline263
    var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline260)
    var unit_shape__9 Shape = Unit{}
    var t205 string
    var inline258 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(unit_shape__9)
    t205 = inline258
    var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline255)
    var t206 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t206,
    }
    var t207 string
    var inline253 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(location_shape__10)
    t207 = inline253
    var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline250)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t210 string = _goml_runtime_core_int32_to_string(self__35)
    return t210
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
