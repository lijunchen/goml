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
    var x173 int32 = self__0.x
    var x174 string = self__0.label
    var t183 string = "Point { " + "x: "
    var t184 string
    var inline258 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x173)
    t184 = inline258
    var t185 string = t183 + t184
    var t186 string = t185 + ", "
    var t187 string = t186 + "label: "
    var t188 string
    var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x174)
    t188 = inline256
    var t189 string = t187 + t188
    var t190 string = t189 + " }"
    return t190
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 3,
        label: "east",
    }
    var idle__8 State__int32 = State__int32_Idle{}
    var t195 string = _goml_m_trait__impl_i_Debug_i_Point_i_debug(point__7)
    var inline298 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline298)
    var t196 string = _goml_m_trait__impl_i_Debug_i_State____int32_i_debug(idle__8)
    var inline295 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline295)
    var t197 string
    var inline285 string = _goml_m_trait__impl_i_Debug_i_Point_i_debug(point__7)
    var inline286 string = "State::Value(" + inline285
    var inline287 string = inline286 + ")"
    t197 = inline287
    var inline280 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline280)
    var t198 string
    var inline273 int = 7
    var inline275 string = "State::Named { " + "value: "
    var inline276 string = _goml_m_trait__impl_i_Debug_i_int_i_debug(inline273)
    var inline277 string = inline275 + inline276
    var inline278 string = inline277 + " }"
    t198 = inline278
    var inline265 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline265)
    var t199 dyn__Debug = dyn__Debug{
        data: int(9),
        vtable: dyn__Debug__vtable__int(),
    }
    var t200 string
    var inline263 string = t199.vtable.debug(t199.data)
    t200 = inline263
    var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline260)
    return struct{}{}
}

func _goml_m_trait__impl_i_Debug_i_State____int32_i_debug(self__3 State__int32) string {
    switch self__3.(type) {
    case State__int32_Idle:
        return "State::Idle"
    case State__int32_Value:
        var x175 int32 = self__3.(State__int32_Value)._0
        var t215 string
        var inline305 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x175)
        t215 = inline305
        var t216 string = "State::Value(" + t215
        var t217 string = t216 + ")"
        return t217
    case State__int32_Named:
        var x176 int32 = self__3.(State__int32_Named)._0
        var t218 string = "State::Named { " + "value: "
        var t219 string
        var inline307 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x176)
        t219 = inline307
        var t220 string = t218 + t219
        var t221 string = t220 + " }"
        return t221
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Debug_i_int_i_debug(self__84 int) string {
    var inline341 string = _goml_runtime_core_int_to_string(self__84)
    return inline341
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t249 string = _goml_runtime_core_int32_to_string(self__72)
    return t249
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
