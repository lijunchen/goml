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
    var x183 int32 = self__0.x
    var x184 int32 = self__0.y
    var t191 string
    var inline224 string = _goml_runtime_core_int32_to_string(x183)
    t191 = inline224
    var prefix__3 string = "Point(" + t191
    var t192 string = prefix__3 + ", "
    var t193 string
    var inline222 string = _goml_runtime_core_int32_to_string(x184)
    t193 = inline222
    var t194 string = t192 + t193
    var t195 string = t194 + ")"
    return t195
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    switch self__4.(type) {
    case Unit:
        return "Unit"
    case Location:
        var x185 Point = self__4.(Location)._0
        var t200 string
        var inline227 int32 = x185.x
        var inline228 int32 = x185.y
        var inline231 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline227)
        var inline232 string = "Point(" + inline231
        var inline233 string = inline232 + ", "
        var inline234 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline228)
        var inline235 string = inline233 + inline234
        var inline236 string = inline235 + ")"
        t200 = inline236
        var t201 string = "Shape::" + t200
        return t201
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t209 string
    var inline268 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__8)
    t209 = inline268
    var inline265 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline265)
    var unit_shape__9 Shape = Unit{}
    var t210 string
    var inline263 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(unit_shape__9)
    t210 = inline263
    var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline260)
    var t211 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t211,
    }
    var t212 string
    var inline258 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(location_shape__10)
    t212 = inline258
    var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
    _goml_runtime_core_string_println(inline255)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t215 string = _goml_runtime_core_int32_to_string(self__33)
    return t215
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
