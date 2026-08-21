package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
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
    label string
}

type Ordering int32

type State__int32 struct {
    _tag int32
    _v1_0 int32
    _v2_0 int32
}

type State__Point interface {
    isState__Point()
}

type State__Point_Idle struct {}

func (_ State__Point_Idle) isState__Point() {}

type State__Point_Value struct {
    _0 Point
}

func (_ State__Point_Value) isState__Point() {}

type State__Point_Named struct {
    _0 Point
}

func (_ State__Point_Named) isState__Point() {}

type State__int struct {
    _tag int32
    _v1_0 int
    _v2_0 int
}

type dyn__Debug_vtable struct {
    debug func(any) string
}

type dyn__Debug struct {
    data any
    vtable *dyn__Debug_vtable
}

func dyn__Debug__wrap__int__debug(self any) string {
    return _goml_m_trait__impl_i_Debug_i_int_i_debug(self.(int))
}

func dyn__Debug__vtable__int() *dyn__Debug_vtable {
    return &dyn__Debug_vtable{
        debug: dyn__Debug__wrap__int__debug,
    }
}

func _goml_m_trait__impl_i_Debug_i_Point_i_debug(self__0 Point) string {
    var x412 int32 = self__0.x
    var x413 string = self__0.label
    var t422 string = "Point { " + "x: "
    var t423 string
    var inline497 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x412)
    t423 = inline497
    var t424 string = t422 + t423
    var t425 string = t424 + ", "
    var t426 string = t425 + "label: "
    var t427 string
    var inline495 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x413)
    t427 = inline495
    var t428 string = t426 + t427
    var t429 string = t428 + " }"
    return t429
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 3,
        label: "east",
    }
    var idle__8 State__int32 = State__int32{
        _tag: 0,
    }
    var t434 string = _goml_m_trait__impl_i_Debug_i_Point_i_debug(point__7)
    var inline537 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
    _goml_runtime_core_string_println(inline537)
    var t435 string = _goml_m_trait__impl_i_Debug_i_State____int32_i_debug(idle__8)
    var inline534 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
    _goml_runtime_core_string_println(inline534)
    var t436 string
    var inline524 string = _goml_m_trait__impl_i_Debug_i_Point_i_debug(point__7)
    var inline525 string = "State::Value(" + inline524
    var inline526 string = inline525 + ")"
    t436 = inline526
    var inline519 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
    _goml_runtime_core_string_println(inline519)
    var t437 string
    var inline512 int = 7
    var inline514 string = "State::Named { " + "value: "
    var inline515 string = _goml_m_trait__impl_i_Debug_i_int_i_debug(inline512)
    var inline516 string = inline514 + inline515
    var inline517 string = inline516 + " }"
    t437 = inline517
    var inline504 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
    _goml_runtime_core_string_println(inline504)
    var t438 dyn__Debug = dyn__Debug{
        data: int(9),
        vtable: dyn__Debug__vtable__int(),
    }
    var t439 string
    var inline502 string = t438.vtable.debug(t438.data)
    t439 = inline502
    var inline499 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t439)
    _goml_runtime_core_string_println(inline499)
    return struct{}{}
}

func _goml_m_trait__impl_i_Debug_i_State____int32_i_debug(self__3 State__int32) string {
    switch self__3._tag {
    case 0:
        return "State::Idle"
    case 1:
        var x414 int32 = self__3._v1_0
        var t454 string
        var inline544 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x414)
        t454 = inline544
        var t455 string = "State::Value(" + t454
        var t456 string = t455 + ")"
        return t456
    case 2:
        var x415 int32 = self__3._v2_0
        var t457 string = "State::Named { " + "value: "
        var t458 string
        var inline546 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x415)
        t458 = inline546
        var t459 string = t457 + t458
        var t460 string = t459 + " }"
        return t460
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Debug_i_int_i_debug(self__166 int) string {
    var inline580 string = _goml_runtime_core_int_to_string(self__166)
    return inline580
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t488 string = _goml_runtime_core_int32_to_string(self__154)
    return t488
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
