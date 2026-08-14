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

type State__int32 interface {
    isState__int32()
}

type State__int32_Idle struct {}

func (_ State__int32_Idle) isState__int32() {}

type State__int32_Value struct {
    _0 int32
}

func (_ State__int32_Value) isState__int32() {}

type State__int32_Named struct {
    _0 int32
}

func (_ State__int32_Named) isState__int32() {}

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

type State__int interface {
    isState__int()
}

type State__int_Idle struct {}

func (_ State__int_Idle) isState__int() {}

type State__int_Value struct {
    _0 int
}

func (_ State__int_Value) isState__int() {}

type State__int_Named struct {
    _0 int
}

func (_ State__int_Named) isState__int() {}

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
    var x188 int32 = self__0.x
    var x189 string = self__0.label
    var t198 string = "Point { " + "x: "
    var t199 string
    var inline273 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x188)
    t199 = inline273
    var t200 string = t198 + t199
    var t201 string = t200 + ", "
    var t202 string = t201 + "label: "
    var t203 string
    var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x189)
    t203 = inline271
    var t204 string = t202 + t203
    var t205 string = t204 + " }"
    return t205
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 3,
        label: "east",
    }
    var idle__8 State__int32 = State__int32_Idle{}
    var t210 string = _goml_m_trait__impl_i_Debug_i_Point_i_debug(point__7)
    var inline313 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline313)
    var t211 string = _goml_m_trait__impl_i_Debug_i_State____int32_i_debug(idle__8)
    var inline310 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline310)
    var t212 string
    var inline300 string = _goml_m_trait__impl_i_Debug_i_Point_i_debug(point__7)
    var inline301 string = "State::Value(" + inline300
    var inline302 string = inline301 + ")"
    t212 = inline302
    var inline295 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
    _goml_runtime_core_string_println(inline295)
    var t213 string
    var inline288 int = 7
    var inline290 string = "State::Named { " + "value: "
    var inline291 string = _goml_m_trait__impl_i_Debug_i_int_i_debug(inline288)
    var inline292 string = inline290 + inline291
    var inline293 string = inline292 + " }"
    t213 = inline293
    var inline280 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t213)
    _goml_runtime_core_string_println(inline280)
    var t214 dyn__Debug = dyn__Debug{
        data: int(9),
        vtable: dyn__Debug__vtable__int(),
    }
    var t215 string
    var inline278 string = t214.vtable.debug(t214.data)
    t215 = inline278
    var inline275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline275)
    return struct{}{}
}

func _goml_m_trait__impl_i_Debug_i_State____int32_i_debug(self__3 State__int32) string {
    switch self__3.(type) {
    case State__int32_Idle:
        return "State::Idle"
    case State__int32_Value:
        var x190 int32 = self__3.(State__int32_Value)._0
        var t230 string
        var inline320 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x190)
        t230 = inline320
        var t231 string = "State::Value(" + t230
        var t232 string = t231 + ")"
        return t232
    case State__int32_Named:
        var x191 int32 = self__3.(State__int32_Named)._0
        var t233 string = "State::Named { " + "value: "
        var t234 string
        var inline322 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x191)
        t234 = inline322
        var t235 string = t233 + t234
        var t236 string = t235 + " }"
        return t236
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Debug_i_int_i_debug(self__82 int) string {
    var inline356 string = _goml_runtime_core_int_to_string(self__82)
    return inline356
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t264 string = _goml_runtime_core_int32_to_string(self__70)
    return t264
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
