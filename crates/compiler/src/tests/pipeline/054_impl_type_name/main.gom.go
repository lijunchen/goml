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
    var retv30 string
    var mtmp22 Point = self__0
    var x23 int32 = mtmp22.x
    var x24 int32 = mtmp22.y
    var y__2 int32 = x24
    var x__1 int32 = x23
    var t31 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
    var prefix__3 string = "Point(" + t31
    var t32 string = prefix__3 + ", "
    var t33 string = _goml_m_inherent_i_int32_i_int32_i_to__string(y__2)
    var t34 string = t32 + t33
    var t35 string = t34 + ")"
    retv30 = t35
    return retv30
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    var retv37 string
    var jp39 string
    switch self__4.(type) {
    case Unit:
        jp39 = "Unit"
    case Location:
        var x25 Point = self__4.(Location)._0
        var point__5 Point = x25
        var t40 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__5)
        var t41 string = "Shape::" + t40
        jp39 = t41
    default:
        panic("non-exhaustive match")
    }
    retv37 = jp39
    return retv37
}

func show_point(point__6 Point) string {
    var retv43 string
    var t44 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__6)
    retv43 = t44
    return retv43
}

func show_shape(shape__7 Shape) string {
    var retv46 string
    var t47 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(shape__7)
    retv46 = t47
    return retv46
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t49 string = show_point(point__8)
    println__T_string(t49)
    var unit_shape__9 Shape = Unit{}
    var t50 string = show_shape(unit_shape__9)
    println__T_string(t50)
    var t51 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Location{
        _0: t51,
    }
    var t52 string = show_shape(location_shape__10)
    println__T_string(t52)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv54 string
    var t55 string = _goml_runtime_core_int32_to_string(self__2)
    retv54 = t55
    return retv54
}

func println__T_string(value__1 string) struct{} {
    var t57 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t57)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv60 string
    retv60 = self__9
    return retv60
}

func main() {
    main0()
}
