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
    var t192 int32 = x__0 * 2
    return t192
}

func increment(x__1 int32) int32 {
    var t195 int32 = x__1 + 1
    return t195
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32 = [2]func(int32) int32{double, increment}
    var f__4 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 0)
    var g__5 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 1)
    var t201 int32 = f__4(10)
    var t202 int32 = g__5(t201)
    var inline235 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t202)
    _goml_runtime_core_string_println(inline235)
    var chosen__6 func(int32) int32
    var inline233 bool = true
    if inline233 {
        chosen__6 = double
    } else {
        chosen__6 = increment
    }
    var applied__7 int32 = chosen__6(5)
    var t203 func(int32) int32
    var inline231 bool = false
    if inline231 {
        t203 = double
    } else {
        t203 = increment
    }
    var direct__8 int32 = t203(5)
    var t204 string
    var inline229 string = _goml_runtime_core_int32_to_string(applied__7)
    t204 = inline229
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline226)
    var t205 string
    var inline224 string = _goml_runtime_core_int32_to_string(direct__8)
    t205 = inline224
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline221)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t217 string = _goml_runtime_core_int32_to_string(self__70)
    return t217
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
