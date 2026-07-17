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
    var retv69 string
    var mtmp61 Point = self__0
    var x62 int32 = mtmp61.x
    var x63 int32 = mtmp61.y
    var y__2 int32 = x63
    var x__1 int32 = x62
    var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var prefix__3 string = "Point(" + t70
    var t71 string = prefix__3 + ", "
    var t72 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t73 string = t71 + t72
    var t74 string = t73 + ")"
    retv69 = t74
    return retv69
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    var retv76 string
    var jp78 string
    switch self__4.(type) {
    case Unit:
        jp78 = "Unit"
    case Location:
        var x64 Point = self__4.(Location)._0
        var point__5 Point = x64
        var t79 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__5)
        var t80 string = "Shape::" + t79
        jp78 = t80
    default:
        panic("non-exhaustive match")
    }
    retv76 = jp78
    return retv76
}

func show_point(point__6 Point) string {
    var retv82 string
    var t83 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__6)
    retv82 = t83
    return retv82
}

func show_shape(shape__7 Shape) string {
    var retv85 string
    var t86 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(shape__7)
    retv85 = t86
    return retv85
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t88 string = show_point(point__8)
    println__T_string(t88)
    var unit_shape__9 Shape = Unit{}
    var t89 string = show_shape(unit_shape__9)
    println__T_string(t89)
    var t90 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t90,
    }
    var t91 string = show_shape(location_shape__10)
    println__T_string(t91)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv93 string
    var t94 string = _goml_runtime_core_int32_to_string(self__5)
    retv93 = t94
    return retv93
}

func println__T_string(value__1 string) struct{} {
    var t96 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t96)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv99 string
    retv99 = self__37
    return retv99
}

func main() {
    main0()
}
