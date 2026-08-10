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
    var x173 int32 = self__0.x
    var x174 int32 = self__0.y
    var t181 string
    var inline214 string = _goml_runtime_core_int32_to_string(x173)
    t181 = inline214
    var prefix__3 string = "Point(" + t181
    var t182 string = prefix__3 + ", "
    var t183 string
    var inline212 string = _goml_runtime_core_int32_to_string(x174)
    t183 = inline212
    var t184 string = t182 + t183
    var t185 string = t184 + ")"
    return t185
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    switch self__4.(type) {
    case Unit:
        return "Unit"
    case Location:
        var x175 Point = self__4.(Location)._0
        var t190 string
        var inline217 int32 = x175.x
        var inline218 int32 = x175.y
        var inline221 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline217)
        var inline222 string = "Point(" + inline221
        var inline223 string = inline222 + ", "
        var inline224 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline218)
        var inline225 string = inline223 + inline224
        var inline226 string = inline225 + ")"
        t190 = inline226
        var t191 string = "Shape::" + t190
        return t191
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t199 string
    var inline258 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__8)
    t199 = inline258
    var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline255)
    var unit_shape__9 Shape = Unit{}
    var t200 string
    var inline253 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(unit_shape__9)
    t200 = inline253
    var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline250)
    var t201 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t201,
    }
    var t202 string
    var inline248 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(location_shape__10)
    t202 = inline248
    var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline245)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t205 string = _goml_runtime_core_int32_to_string(self__33)
    return t205
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
