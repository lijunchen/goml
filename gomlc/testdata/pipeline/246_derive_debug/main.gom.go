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
    var x183 int32 = self__0.x
    var x184 string = self__0.label
    var t193 string = "Point { " + "x: "
    var t194 string
    var inline268 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x183)
    t194 = inline268
    var t195 string = t193 + t194
    var t196 string = t195 + ", "
    var t197 string = t196 + "label: "
    var t198 string
    var inline266 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x184)
    t198 = inline266
    var t199 string = t197 + t198
    var t200 string = t199 + " }"
    return t200
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 3,
        label: "east",
    }
    var idle__8 State__int32 = State__int32_Idle{}
    var t205 string = _goml_m_trait__impl_i_Debug_i_Point_i_debug(point__7)
    var inline308 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline308)
    var t206 string = _goml_m_trait__impl_i_Debug_i_State____int32_i_debug(idle__8)
    var inline305 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline305)
    var t207 string
    var inline295 string = _goml_m_trait__impl_i_Debug_i_Point_i_debug(point__7)
    var inline296 string = "State::Value(" + inline295
    var inline297 string = inline296 + ")"
    t207 = inline297
    var inline290 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline290)
    var t208 string
    var inline283 int = 7
    var inline285 string = "State::Named { " + "value: "
    var inline286 string = _goml_m_trait__impl_i_Debug_i_int_i_debug(inline283)
    var inline287 string = inline285 + inline286
    var inline288 string = inline287 + " }"
    t208 = inline288
    var inline275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline275)
    var t209 dyn__Debug = dyn__Debug{
        data: int(9),
        vtable: dyn__Debug__vtable__int(),
    }
    var t210 string
    var inline273 string = t209.vtable.debug(t209.data)
    t210 = inline273
    var inline270 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
    _goml_runtime_core_string_println(inline270)
    return struct{}{}
}

func _goml_m_trait__impl_i_Debug_i_State____int32_i_debug(self__3 State__int32) string {
    switch self__3.(type) {
    case State__int32_Idle:
        return "State::Idle"
    case State__int32_Value:
        var x185 int32 = self__3.(State__int32_Value)._0
        var t225 string
        var inline315 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x185)
        t225 = inline315
        var t226 string = "State::Value(" + t225
        var t227 string = t226 + ")"
        return t227
    case State__int32_Named:
        var x186 int32 = self__3.(State__int32_Named)._0
        var t228 string = "State::Named { " + "value: "
        var t229 string
        var inline317 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x186)
        t229 = inline317
        var t230 string = t228 + t229
        var t231 string = t230 + " }"
        return t231
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Debug_i_int_i_debug(self__82 int) string {
    var inline351 string = _goml_runtime_core_int_to_string(self__82)
    return inline351
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t259 string = _goml_runtime_core_int32_to_string(self__70)
    return t259
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
