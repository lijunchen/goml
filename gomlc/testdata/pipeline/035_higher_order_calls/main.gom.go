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

func array_get__Array_2_20Fn1_5int32_to_5int32(arr [2]func(int32) int32, index int) func(int32) int32 {
    return arr[index]
}

func double(x__0 int32) int32 {
    var t182 int32 = x__0 * 2
    return t182
}

func increment(x__1 int32) int32 {
    var t185 int32 = x__1 + 1
    return t185
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32 = [2]func(int32) int32{double, increment}
    var f__4 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 0)
    var g__5 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 1)
    var t191 int32 = f__4(10)
    var t192 int32 = g__5(t191)
    var inline225 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t192)
    _goml_runtime_core_string_println(inline225)
    var chosen__6 func(int32) int32
    var inline223 bool = true
    if inline223 {
        chosen__6 = double
    } else {
        chosen__6 = increment
    }
    var applied__7 int32 = chosen__6(5)
    var t193 func(int32) int32
    var inline221 bool = false
    if inline221 {
        t193 = double
    } else {
        t193 = increment
    }
    var direct__8 int32 = t193(5)
    var t194 string
    var inline219 string = _goml_runtime_core_int32_to_string(applied__7)
    t194 = inline219
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline216)
    var t195 string
    var inline214 string = _goml_runtime_core_int32_to_string(direct__8)
    t195 = inline214
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline211)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t207 string = _goml_runtime_core_int32_to_string(self__72)
    return t207
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
