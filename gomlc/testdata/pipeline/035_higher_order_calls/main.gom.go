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
    var retv156 int32
    var t157 int32 = x__0 * 2
    retv156 = t157
    return retv156
}

func increment(x__1 int32) int32 {
    var retv159 int32
    var t160 int32 = x__1 + 1
    retv159 = t160
    return retv159
}

func chooser(flag__2 bool) func(int32) int32 {
    var retv162 func(int32) int32
    var jp164 func(int32) int32
    if flag__2 {
        jp164 = double
    } else {
        jp164 = increment
    }
    retv162 = jp164
    return retv162
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32 = [2]func(int32) int32{double, increment}
    var f__4 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 0)
    var g__5 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 1)
    var t166 int32 = f__4(10)
    var t167 int32 = g__5(t166)
    println__T_int32(t167)
    var chosen__6 func(int32) int32 = chooser(true)
    var applied__7 int32 = chosen__6(5)
    var t168 func(int32) int32 = chooser(false)
    var direct__8 int32 = t168(5)
    var printer__9 func(string) struct{} = println__T_string
    var t169 string = _goml_m_inherent_i_int32_i_int32_i_to__string(applied__7)
    printer__9(t169)
    var t170 string = _goml_m_inherent_i_int32_i_int32_i_to__string(direct__8)
    printer__9(t170)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t172 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t172)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t175 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t175)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv178 string
    var t179 string = _goml_runtime_core_int32_to_string(self__6)
    retv178 = t179
    return retv178
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv181 string
    var t182 string = _goml_runtime_core_int32_to_string(self__43)
    retv181 = t182
    return retv181
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv184 string
    retv184 = self__38
    return retv184
}

func main() {
    main0()
}
