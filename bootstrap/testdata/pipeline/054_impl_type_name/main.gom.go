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
    var retv72 string
    var mtmp64 Point = self__0
    var x65 int32 = mtmp64.x
    var x66 int32 = mtmp64.y
    var y__2 int32 = x66
    var x__1 int32 = x65
    var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var prefix__3 string = "Point(" + t73
    var t74 string = prefix__3 + ", "
    var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t76 string = t74 + t75
    var t77 string = t76 + ")"
    retv72 = t77
    return retv72
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    var retv79 string
    var jp81 string
    switch self__4.(type) {
    case Unit:
        jp81 = "Unit"
    case Location:
        var x67 Point = self__4.(Location)._0
        var point__5 Point = x67
        var t82 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__5)
        var t83 string = "Shape::" + t82
        jp81 = t83
    default:
        panic("non-exhaustive match")
    }
    retv79 = jp81
    return retv79
}

func show_point(point__6 Point) string {
    var retv85 string
    var t86 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__6)
    retv85 = t86
    return retv85
}

func show_shape(shape__7 Shape) string {
    var retv88 string
    var t89 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(shape__7)
    retv88 = t89
    return retv88
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t91 string = show_point(point__8)
    println__T_string(t91)
    var unit_shape__9 Shape = Unit{}
    var t92 string = show_shape(unit_shape__9)
    println__T_string(t92)
    var t93 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t93,
    }
    var t94 string = show_shape(location_shape__10)
    println__T_string(t94)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv96 string
    var t97 string = _goml_runtime_core_int32_to_string(self__6)
    retv96 = t97
    return retv96
}

func println__T_string(value__1 string) struct{} {
    var t99 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t99)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv102 string
    retv102 = self__38
    return retv102
}

func main() {
    main0()
}
