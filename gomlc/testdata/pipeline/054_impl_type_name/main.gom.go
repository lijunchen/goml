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
    var x412 int32 = self__0.x
    var x413 int32 = self__0.y
    var t420 string
    var inline453 string = _goml_runtime_core_int32_to_string(x412)
    t420 = inline453
    var prefix__3 string = "Point(" + t420
    var t421 string = prefix__3 + ", "
    var t422 string
    var inline451 string = _goml_runtime_core_int32_to_string(x413)
    t422 = inline451
    var t423 string = t421 + t422
    var t424 string = t423 + ")"
    return t424
}

func _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(self__4 Shape) string {
    switch self__4._tag {
    case 0:
        return "Unit"
    case 1:
        var x414 Point = self__4._v1_0
        var t429 string
        var inline456 int32 = x414.x
        var inline457 int32 = x414.y
        var inline460 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline456)
        var inline461 string = "Point(" + inline460
        var inline462 string = inline461 + ", "
        var inline463 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline457)
        var inline464 string = inline462 + inline463
        var inline465 string = inline464 + ")"
        t429 = inline465
        var t430 string = "Shape::" + t429
        return t430
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var point__8 Point = Point{
        x: 7,
        y: 9,
    }
    var t438 string
    var inline497 string = _goml_m_trait__impl_i_TypeName_i_Point_i_type__name(point__8)
    t438 = inline497
    var inline494 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
    _goml_runtime_core_string_println(inline494)
    var unit_shape__9 Shape = Shape{
        _tag: 0,
    }
    var t439 string
    var inline492 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(unit_shape__9)
    t439 = inline492
    var inline489 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t439)
    _goml_runtime_core_string_println(inline489)
    var t440 Point = Point{
        x: 1,
        y: 2,
    }
    var location_shape__10 Shape = Shape{
        _tag: 1,
        _v1_0: t440,
    }
    var t441 string
    var inline487 string = _goml_m_trait__impl_i_TypeName_i_Shape_i_type__name(location_shape__10)
    t441 = inline487
    var inline484 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
    _goml_runtime_core_string_println(inline484)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__33 int32) string {
    var t444 string = _goml_runtime_core_int32_to_string(self__33)
    return t444
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
