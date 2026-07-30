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
    var retv76 string
    var mtmp68 Point = self__0
    var x69 int32 = mtmp68.x
    var x70 int32 = mtmp68.y
    var y__2 int32 = x70
    var x__1 int32 = x69
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var prefix__3 string = "Point(" + t77
    var t78 string = prefix__3 + ", "
    var t79 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t80 string = t78 + t79
    var t81 string = t80 + ")"
    retv76 = t81
    return retv76
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    var retv83 string
    var jp85 string
    switch self__4.(type) {
    case Unit:
        jp85 = "Unit"
    case Location:
        var x71 Point = self__4.(Location)._0
        var point__5 Point = x71
        var t86 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__5)
        var t87 string = "Shape::" + t86
        jp85 = t87
    default:
        panic("non-exhaustive match")
    }
    retv83 = jp85
    return retv83
}

func show_point(point__6 Point) string {
    var retv89 string
    var t90 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__6)
    retv89 = t90
    return retv89
}

func show_shape(shape__7 Shape) string {
    var retv92 string
    var t93 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(shape__7)
    retv92 = t93
    return retv92
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t95 string = show_point(point__8)
    println__T_string(t95)
    var unit_shape__9 Shape = Unit{}
    var t96 string = show_shape(unit_shape__9)
    println__T_string(t96)
    var t97 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t97,
    }
    var t98 string = show_shape(location_shape__10)
    println__T_string(t98)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv100 string
    var t101 string = _goml_runtime_core_int32_to_string(self__6)
    retv100 = t101
    return retv100
}

func println__T_string(value__1 string) struct{} {
    var t103 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t103)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv106 string
    retv106 = self__38
    return retv106
}

func main() {
    main0()
}
