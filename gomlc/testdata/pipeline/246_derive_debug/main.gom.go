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
    var x409 int32 = self__0.x
    var x410 string = self__0.label
    var t419 string = "Point { " + "x: "
    var t420 string
    var inline494 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x409)
    t420 = inline494
    var t421 string = t419 + t420
    var t422 string = t421 + ", "
    var t423 string = t422 + "label: "
    var t424 string
    var inline492 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x410)
    t424 = inline492
    var t425 string = t423 + t424
    var t426 string = t425 + " }"
    return t426
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 3,
        label: "east",
    }
    var idle__8 State__int32 = State__int32{
        _tag: 0,
    }
    var t431 string = _goml_m_trait__impl_i_Debug_i_Point_i_debug(point__7)
    var inline534 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t431)
    _goml_runtime_core_string_println(inline534)
    var t432 string = _goml_m_trait__impl_i_Debug_i_State____int32_i_debug(idle__8)
    var inline531 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
    _goml_runtime_core_string_println(inline531)
    var t433 string
    var inline521 string = _goml_m_trait__impl_i_Debug_i_Point_i_debug(point__7)
    var inline522 string = "State::Value(" + inline521
    var inline523 string = inline522 + ")"
    t433 = inline523
    var inline516 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
    _goml_runtime_core_string_println(inline516)
    var t434 string
    var inline509 int = 7
    var inline511 string = "State::Named { " + "value: "
    var inline512 string = _goml_m_trait__impl_i_Debug_i_int_i_debug(inline509)
    var inline513 string = inline511 + inline512
    var inline514 string = inline513 + " }"
    t434 = inline514
    var inline501 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
    _goml_runtime_core_string_println(inline501)
    var t435 dyn__Debug = dyn__Debug{
        data: int(9),
        vtable: dyn__Debug__vtable__int(),
    }
    var t436 string
    var inline499 string = t435.vtable.debug(t435.data)
    t436 = inline499
    var inline496 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
    _goml_runtime_core_string_println(inline496)
    return struct{}{}
}

func _goml_m_trait__impl_i_Debug_i_State____int32_i_debug(self__3 State__int32) string {
    switch self__3._tag {
    case 0:
        return "State::Idle"
    case 1:
        var x411 int32 = self__3._v1_0
        var t451 string
        var inline541 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x411)
        t451 = inline541
        var t452 string = "State::Value(" + t451
        var t453 string = t452 + ")"
        return t453
    case 2:
        var x412 int32 = self__3._v2_0
        var t454 string = "State::Named { " + "value: "
        var t455 string
        var inline543 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x412)
        t455 = inline543
        var t456 string = t454 + t455
        var t457 string = t456 + " }"
        return t457
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Debug_i_int_i_debug(self__166 int) string {
    var inline577 string = _goml_runtime_core_int_to_string(self__166)
    return inline577
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t485 string = _goml_runtime_core_int32_to_string(self__154)
    return t485
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
