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
    var retv66 string
    var mtmp58 Point = self__0
    var x59 int32 = mtmp58.x
    var x60 int32 = mtmp58.y
    var y__2 int32 = x60
    var x__1 int32 = x59
    var t67 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var prefix__3 string = "Point(" + t67
    var t68 string = prefix__3 + ", "
    var t69 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t70 string = t68 + t69
    var t71 string = t70 + ")"
    retv66 = t71
    return retv66
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    var retv73 string
    var jp75 string
    switch self__4.(type) {
    case Unit:
        jp75 = "Unit"
    case Location:
        var x61 Point = self__4.(Location)._0
        var point__5 Point = x61
        var t76 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__5)
        var t77 string = "Shape::" + t76
        jp75 = t77
    default:
        panic("non-exhaustive match")
    }
    retv73 = jp75
    return retv73
}

func show_point(point__6 Point) string {
    var retv79 string
    var t80 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__6)
    retv79 = t80
    return retv79
}

func show_shape(shape__7 Shape) string {
    var retv82 string
    var t83 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(shape__7)
    retv82 = t83
    return retv82
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t85 string = show_point(point__8)
    println__T_string(t85)
    var unit_shape__9 Shape = Unit{}
    var t86 string = show_shape(unit_shape__9)
    println__T_string(t86)
    var t87 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t87,
    }
    var t88 string = show_shape(location_shape__10)
    println__T_string(t88)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv90 string
    var t91 string = _goml_runtime_core_int32_to_string(self__2)
    retv90 = t91
    return retv90
}

func println__T_string(value__1 string) struct{} {
    var t93 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t93)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv96 string
    retv96 = self__34
    return retv96
}

func main() {
    main0()
}
