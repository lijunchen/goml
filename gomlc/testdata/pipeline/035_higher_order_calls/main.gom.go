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
    var t141 int32 = x__0 * 2
    return t141
}

func increment(x__1 int32) int32 {
    var t144 int32 = x__1 + 1
    return t144
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32 = [2]func(int32) int32{double, increment}
    var f__4 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 0)
    var g__5 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 1)
    var t150 int32 = f__4(10)
    var t151 int32 = g__5(t150)
    var inline184 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t151)
    _goml_runtime_core_string_println(inline184)
    var chosen__6 func(int32) int32
    var inline182 bool = true
    if inline182 {
        chosen__6 = double
    } else {
        chosen__6 = increment
    }
    var applied__7 int32 = chosen__6(5)
    var t152 func(int32) int32
    var inline180 bool = false
    if inline180 {
        t152 = double
    } else {
        t152 = increment
    }
    var direct__8 int32 = t152(5)
    var t153 string
    var inline178 string = _goml_runtime_core_int32_to_string(applied__7)
    t153 = inline178
    var inline175 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t153)
    _goml_runtime_core_string_println(inline175)
    var t154 string
    var inline173 string = _goml_runtime_core_int32_to_string(direct__8)
    t154 = inline173
    var inline170 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t154)
    _goml_runtime_core_string_println(inline170)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t166 string = _goml_runtime_core_int32_to_string(self__72)
    return t166
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
