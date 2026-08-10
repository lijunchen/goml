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
    var t177 int32 = x__0 * 2
    return t177
}

func increment(x__1 int32) int32 {
    var t180 int32 = x__1 + 1
    return t180
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32 = [2]func(int32) int32{double, increment}
    var f__4 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 0)
    var g__5 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 1)
    var t186 int32 = f__4(10)
    var t187 int32 = g__5(t186)
    var inline220 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t187)
    _goml_runtime_core_string_println(inline220)
    var chosen__6 func(int32) int32
    var inline218 bool = true
    if inline218 {
        chosen__6 = double
    } else {
        chosen__6 = increment
    }
    var applied__7 int32 = chosen__6(5)
    var t188 func(int32) int32
    var inline216 bool = false
    if inline216 {
        t188 = double
    } else {
        t188 = increment
    }
    var direct__8 int32 = t188(5)
    var t189 string
    var inline214 string = _goml_runtime_core_int32_to_string(applied__7)
    t189 = inline214
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline211)
    var t190 string
    var inline209 string = _goml_runtime_core_int32_to_string(direct__8)
    t190 = inline209
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline206)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t202 string = _goml_runtime_core_int32_to_string(self__70)
    return t202
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
