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
    var t160 int32 = x__0 * 2
    return t160
}

func increment(x__1 int32) int32 {
    var t163 int32 = x__1 + 1
    return t163
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32 = [2]func(int32) int32{double, increment}
    var f__4 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 0)
    var g__5 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 1)
    var t169 int32 = f__4(10)
    var t170 int32 = g__5(t169)
    var inline203 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t170)
    _goml_runtime_core_string_println(inline203)
    var chosen__6 func(int32) int32
    var inline201 bool = true
    if inline201 {
        chosen__6 = double
    } else {
        chosen__6 = increment
    }
    var applied__7 int32 = chosen__6(5)
    var t171 func(int32) int32
    var inline199 bool = false
    if inline199 {
        t171 = double
    } else {
        t171 = increment
    }
    var direct__8 int32 = t171(5)
    var t172 string
    var inline197 string = _goml_runtime_core_int32_to_string(applied__7)
    t172 = inline197
    var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t172)
    _goml_runtime_core_string_println(inline194)
    var t173 string
    var inline192 string = _goml_runtime_core_int32_to_string(direct__8)
    t173 = inline192
    var inline189 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t173)
    _goml_runtime_core_string_println(inline189)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t185 string = _goml_runtime_core_int32_to_string(self__43)
    return t185
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
