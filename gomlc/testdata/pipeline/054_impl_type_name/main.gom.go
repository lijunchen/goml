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

type Ordering int32

type Shape struct {
    _tag int32
    _v1_0 Point
}

func _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(self__0 Point) string {
    var x409 int32 = self__0.x
    var x410 int32 = self__0.y
    var t417 string
    var inline450 string = _goml_runtime_core_int32_to_string(x409)
    t417 = inline450
    var prefix__3 string = "Point(" + t417
    var t418 string = prefix__3 + ", "
    var t419 string
    var inline448 string = _goml_runtime_core_int32_to_string(x410)
    t419 = inline448
    var t420 string = t418 + t419
    var t421 string = t420 + ")"
    return t421
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    switch self__4._tag {
    case 0:
        return "Unit"
    case 1:
        var x411 Point = self__4._v1_0
        var t426 string
        var inline453 int32 = x411.x
        var inline454 int32 = x411.y
        var inline457 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline453)
        var inline458 string = "Point(" + inline457
        var inline459 string = inline458 + ", "
        var inline460 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline454)
        var inline461 string = inline459 + inline460
        var inline462 string = inline461 + ")"
        t426 = inline462
        var t427 string = "Shape::" + t426
        return t427
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t435 string
    var inline494 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__8)
    t435 = inline494
    var inline491 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
    _goml_runtime_core_string_println(inline491)
    var unit_shape__9 Shape = Shape{
        _tag: 0,
    }
    var t436 string
    var inline489 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(unit_shape__9)
    t436 = inline489
    var inline486 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
    _goml_runtime_core_string_println(inline486)
    var t437 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Shape{
        _tag: 1,
        _v1_0: t437,
    }
    var t438 string
    var inline484 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(location_shape__10)
    t438 = inline484
    var inline481 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
    _goml_runtime_core_string_println(inline481)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t441 string = _goml_runtime_core_int32_to_string(self__33)
    return t441
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
