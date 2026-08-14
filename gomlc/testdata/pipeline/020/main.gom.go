package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_unit_to_string(x struct{}) string {
    return "()"
}

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

type Wrapper__int32 struct {
    value int32
}

type Wrapper__unit struct {
    value struct{}
}

type Ordering int32

type Shape__int32 interface {
    isShape__int32()
}

type Shape__int32_Dot struct {
    _0 Point
}

func (_ Shape__int32_Dot) isShape__int32() {}

type Shape__int32_Wrapped struct {
    _0 Wrapper__int32
}

func (_ Shape__int32_Wrapped) isShape__int32() {}

type Shape__int32_Origin struct {}

func (_ Shape__int32_Origin) isShape__int32() {}

type Shape__unit interface {
    isShape__unit()
}

type Shape__unit_Dot struct {
    _0 Point
}

func (_ Shape__unit_Dot) isShape__unit() {}

type Shape__unit_Wrapped struct {
    _0 Wrapper__unit
}

func (_ Shape__unit_Wrapped) isShape__unit() {}

type Shape__unit_Origin struct {}

func (_ Shape__unit_Origin) isShape__unit() {}

func bounce_int(shape__0 Shape__int32) Shape__int32 {
    switch shape__0.(type) {
    case Shape__int32_Dot:
        var x408 Point = shape__0.(Shape__int32_Dot)._0
        var t437 Shape__int32 = Shape__int32_Dot{
            _0: x408,
        }
        return t437
    case Shape__int32_Wrapped:
        var x409 Wrapper__int32 = shape__0.(Shape__int32_Wrapped)._0
        var t438 Shape__int32 = Shape__int32_Wrapped{
            _0: x409,
        }
        return t438
    case Shape__int32_Origin:
        return Shape__int32_Origin{}
    default:
        panic("non-exhaustive match")
    }
}

func point32_to_string(point__8 Point) string {
    var x413 int32 = point__8.x
    var x414 int32 = point__8.y
    var t450 string
    var inline518 string = _goml_runtime_core_int32_to_string(x413)
    t450 = inline518
    var with_x__11 string = "Point { x: " + t450
    var with_y_label__12 string = with_x__11 + ", y: "
    var t451 string
    var inline516 string = _goml_runtime_core_int32_to_string(x414)
    t451 = inline516
    var with_y__13 string = with_y_label__12 + t451
    var t452 string = with_y__13 + " }"
    return t452
}

func wrapper_int32_to_string(wrapper__14 Wrapper__int32) string {
    var x416 int32 = wrapper__14.value
    var t455 string
    var inline520 string = _goml_runtime_core_int32_to_string(x416)
    t455 = inline520
    var prefix__16 string = "Wrapper[int32] { value: " + t455
    var t456 string = prefix__16 + " }"
    return t456
}

func wrapper_unit_to_string(wrapper__17 Wrapper__unit) string {
    var x418 struct{} = wrapper__17.value
    var t459 string
    var inline522 string = _goml_runtime_core_unit_to_string(x418)
    t459 = inline522
    var prefix__19 string = "Wrapper[unit] { value: " + t459
    var t460 string = prefix__19 + " }"
    return t460
}

func shape_int32_to_string(shape__20 Shape__int32) string {
    switch shape__20.(type) {
    case Shape__int32_Dot:
        var x419 Point = shape__20.(Shape__int32_Dot)._0
        var t465 string
        var inline525 int32 = x419.x
        var inline526 int32 = x419.y
        var inline529 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline525)
        var inline530 string = "Point { x: " + inline529
        var inline531 string = inline530 + ", y: "
        var inline532 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline526)
        var inline533 string = inline531 + inline532
        var inline534 string = inline533 + " }"
        t465 = inline534
        var prefix__22 string = "Shape::Dot(" + t465
        var t466 string = prefix__22 + ")"
        return t466
    case Shape__int32_Wrapped:
        var x420 Wrapper__int32 = shape__20.(Shape__int32_Wrapped)._0
        var t467 string
        var inline537 int32 = x420.value
        var inline539 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline537)
        var inline540 string = "Wrapper[int32] { value: " + inline539
        var inline541 string = inline540 + " }"
        t467 = inline541
        var prefix__24 string = "Shape::Wrapped(" + t467
        var t468 string = prefix__24 + ")"
        return t468
    case Shape__int32_Origin:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func shape_unit_to_string(shape__25 Shape__unit) string {
    switch shape__25.(type) {
    case Shape__unit_Dot:
        var x421 Point = shape__25.(Shape__unit_Dot)._0
        var t473 string
        var inline544 int32 = x421.x
        var inline545 int32 = x421.y
        var inline548 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline544)
        var inline549 string = "Point { x: " + inline548
        var inline550 string = inline549 + ", y: "
        var inline551 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline545)
        var inline552 string = inline550 + inline551
        var inline553 string = inline552 + " }"
        t473 = inline553
        var prefix__27 string = "Shape::Dot(" + t473
        var t474 string = prefix__27 + ")"
        return t474
    case Shape__unit_Wrapped:
        var x422 Wrapper__unit = shape__25.(Shape__unit_Wrapped)._0
        var t475 string
        var inline556 struct{} = x422.value
        var inline558 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline556)
        var inline559 string = "Wrapper[unit] { value: " + inline558
        var inline560 string = inline559 + " }"
        t475 = inline560
        var prefix__29 string = "Shape::Wrapped(" + t475
        var t476 string = prefix__29 + ")"
        return t476
    case Shape__unit_Origin:
        return "Shape::Origin"
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t478 Point = Point{
        x: 3,
        y: 4,
    }
    var t479 string = point32_to_string(t478)
    println__T_string(t479)
    var t480 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t481 string = wrapper_int32_to_string(t480)
    println__T_string(t481)
    var t482 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t483 string = wrapper_unit_to_string(t482)
    println__T_string(t483)
    var bounced_origin__30 Shape__int32 = bounce_int(Shape__int32_Origin{})
    var t484 Point = Point{
        x: 3,
        y: 4,
    }
    var t485 Shape__int32 = Shape__int32_Dot{
        _0: t484,
    }
    var t486 string = shape_int32_to_string(t485)
    println__T_string(t486)
    var t487 Wrapper__int32 = Wrapper__int32{
        value: 7,
    }
    var t488 Shape__int32 = Shape__int32_Wrapped{
        _0: t487,
    }
    var t489 string = shape_int32_to_string(t488)
    var inline597 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t489)
    _goml_runtime_core_string_println(inline597)
    var t490 string = shape_int32_to_string(bounced_origin__30)
    var inline594 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t490)
    _goml_runtime_core_string_println(inline594)
    var t491 Point = Point{
        x: 3,
        y: 4,
    }
    var t492 Shape__unit = Shape__unit_Dot{
        _0: t491,
    }
    var t493 string = shape_unit_to_string(t492)
    var inline591 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t493)
    _goml_runtime_core_string_println(inline591)
    var t494 Wrapper__unit = Wrapper__unit{
        value: struct{}{},
    }
    var t495 Shape__unit = Shape__unit_Wrapped{
        _0: t494,
    }
    var t496 string = shape_unit_to_string(t495)
    var inline588 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t496)
    _goml_runtime_core_string_println(inline588)
    var t497 string
    t497 = "Shape::Origin"
    var inline574 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t497)
    _goml_runtime_core_string_println(inline574)
    var t498 Shape__int32
    t498 = Shape__int32_Origin{}
    switch t498.(type) {
    case Shape__int32_Dot:
    case Shape__int32_Wrapped:
    case Shape__int32_Origin:
    default:
        panic("non-exhaustive match")
    }
    var inline562 string = "struct enums!"
    var inline563 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline562)
    _goml_runtime_core_string_println(inline563)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t502 string = _goml_runtime_core_int32_to_string(self__33)
    return t502
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__147 struct{}) string {
    var t505 string = _goml_runtime_core_unit_to_string(self__147)
    return t505
}

func println__T_string(value__1 string) struct{} {
    var t507 string
    t507 = value__1
    _goml_runtime_core_string_println(t507)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
