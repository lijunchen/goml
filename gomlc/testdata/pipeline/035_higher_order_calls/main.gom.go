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
    var retv112 int32
    var t113 int32 = x__0 * 2
    retv112 = t113
    return retv112
}

func increment(x__1 int32) int32 {
    var retv115 int32
    var t116 int32 = x__1 + 1
    retv115 = t116
    return retv115
}

func chooser(flag__2 bool) func(int32) int32 {
    var retv118 func(int32) int32
    var jp120 func(int32) int32
    if flag__2 {
        jp120 = double
    } else {
        jp120 = increment
    }
    retv118 = jp120
    return retv118
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32 = [2]func(int32) int32{double, increment}
    var f__4 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 0)
    var g__5 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 1)
    var t122 int32 = f__4(10)
    var t123 int32 = g__5(t122)
    println__T_int32(t123)
    var chosen__6 func(int32) int32 = chooser(true)
    var applied__7 int32 = chosen__6(5)
    var t124 func(int32) int32 = chooser(false)
    var direct__8 int32 = t124(5)
    var printer__9 func(string) struct{} = println__T_string
    var t125 string = _goml_m_inherent_i_int32_i_int32_i_to__string(applied__7)
    printer__9(t125)
    var t126 string = _goml_m_inherent_i_int32_i_int32_i_to__string(direct__8)
    printer__9(t126)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t128 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t128)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t131 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t131)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv134 string
    var t135 string = _goml_runtime_core_int32_to_string(self__6)
    retv134 = t135
    return retv134
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv137 string
    var t138 string = _goml_runtime_core_int32_to_string(self__43)
    retv137 = t138
    return retv137
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv140 string
    retv140 = self__38
    return retv140
}

func main() {
    main0()
}
