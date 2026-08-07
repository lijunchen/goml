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
    var x137 int32 = self__0.x
    var x138 string = self__0.label
    var t147 string = "Point { " + "x: "
    var t148 string
    var inline222 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x137)
    t148 = inline222
    var t149 string = t147 + t148
    var t150 string = t149 + ", "
    var t151 string = t150 + "label: "
    var t152 string
    var inline220 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x138)
    t152 = inline220
    var t153 string = t151 + t152
    var t154 string = t153 + " }"
    return t154
}

func main0() struct{} {
    var point__7 Point = Point{
        x: 3,
        label: "east",
    }
    var idle__8 State__int32 = State__int32_Idle{}
    var t159 string = _goml_m_trait__impl_i_Debug_i_Point_i_debug(point__7)
    var inline262 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t159)
    _goml_runtime_core_string_println(inline262)
    var t160 string = _goml_m_trait__impl_i_Debug_i_State____int32_i_debug(idle__8)
    var inline259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
    _goml_runtime_core_string_println(inline259)
    var t161 string
    var inline249 string = _goml_m_trait__impl_i_Debug_i_Point_i_debug(point__7)
    var inline250 string = "State::Value(" + inline249
    var inline251 string = inline250 + ")"
    t161 = inline251
    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t161)
    _goml_runtime_core_string_println(inline244)
    var t162 string
    var inline237 int = 7
    var inline239 string = "State::Named { " + "value: "
    var inline240 string = _goml_m_trait__impl_i_Debug_i_int_i_debug(inline237)
    var inline241 string = inline239 + inline240
    var inline242 string = inline241 + " }"
    t162 = inline242
    var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t162)
    _goml_runtime_core_string_println(inline229)
    var t163 dyn__Debug = dyn__Debug{
        data: int(9),
        vtable: dyn__Debug__vtable__int(),
    }
    var t164 string
    var inline227 string = t163.vtable.debug(t163.data)
    t164 = inline227
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t164)
    _goml_runtime_core_string_println(inline224)
    return struct{}{}
}

func _goml_m_trait__impl_i_Debug_i_State____int32_i_debug(self__3 State__int32) string {
    switch self__3.(type) {
    case State__int32_Idle:
        return "State::Idle"
    case State__int32_Value:
        var x139 int32 = self__3.(State__int32_Value)._0
        var t179 string
        var inline269 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x139)
        t179 = inline269
        var t180 string = "State::Value(" + t179
        var t181 string = t180 + ")"
        return t181
    case State__int32_Named:
        var x140 int32 = self__3.(State__int32_Named)._0
        var t182 string = "State::Named { " + "value: "
        var t183 string
        var inline271 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x140)
        t183 = inline271
        var t184 string = t182 + t183
        var t185 string = t184 + " }"
        return t185
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Debug_i_int_i_debug(self__84 int) string {
    var inline305 string = _goml_runtime_core_int_to_string(self__84)
    return inline305
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t213 string = _goml_runtime_core_int32_to_string(self__72)
    return t213
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
