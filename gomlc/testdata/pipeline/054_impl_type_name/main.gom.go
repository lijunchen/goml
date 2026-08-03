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
    var x137 int32 = self__0.x
    var x138 int32 = self__0.y
    var t145 string
    var inline178 string = _goml_runtime_core_int32_to_string(x137)
    t145 = inline178
    var prefix__3 string = "Point(" + t145
    var t146 string = prefix__3 + ", "
    var t147 string
    var inline176 string = _goml_runtime_core_int32_to_string(x138)
    t147 = inline176
    var t148 string = t146 + t147
    var t149 string = t148 + ")"
    return t149
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    switch self__4.(type) {
    case Unit:
        return "Unit"
    case Location:
        var x139 Point = self__4.(Location)._0
        var t154 string
        var inline181 int32 = x139.x
        var inline182 int32 = x139.y
        var inline185 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline181)
        var inline186 string = "Point(" + inline185
        var inline187 string = inline186 + ", "
        var inline188 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline182)
        var inline189 string = inline187 + inline188
        var inline190 string = inline189 + ")"
        t154 = inline190
        var t155 string = "Shape::" + t154
        return t155
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t163 string
    var inline222 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__8)
    t163 = inline222
    var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t163)
    _goml_runtime_core_string_println(inline219)
    var unit_shape__9 Shape = Unit{}
    var t164 string
    var inline217 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(unit_shape__9)
    t164 = inline217
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t164)
    _goml_runtime_core_string_println(inline214)
    var t165 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t165,
    }
    var t166 string
    var inline212 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(location_shape__10)
    t166 = inline212
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
    _goml_runtime_core_string_println(inline209)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t169 string = _goml_runtime_core_int32_to_string(self__35)
    return t169
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
