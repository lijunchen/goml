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
    var retv72 int32
    var t73 int32 = x__0 * 2
    retv72 = t73
    return retv72
}

func increment(x__1 int32) int32 {
    var retv75 int32
    var t76 int32 = x__1 + 1
    retv75 = t76
    return retv75
}

func chooser(flag__2 bool) func(int32) int32 {
    var retv78 func(int32) int32
    var jp80 func(int32) int32
    if flag__2 {
        jp80 = double
    } else {
        jp80 = increment
    }
    retv78 = jp80
    return retv78
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32 = [2]func(int32) int32{double, increment}
    var f__4 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 0)
    var g__5 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 1)
    var t82 int32 = f__4(10)
    var t83 int32 = g__5(t82)
    println__T_int32(t83)
    var chosen__6 func(int32) int32 = chooser(true)
    var applied__7 int32 = chosen__6(5)
    var t84 func(int32) int32 = chooser(false)
    var direct__8 int32 = t84(5)
    var printer__9 func(string) struct{} = println__T_string
    var t85 string = _goml_m_inherent_i_int32_i_int32_i_to__string(applied__7)
    printer__9(t85)
    var t86 string = _goml_m_inherent_i_int32_i_int32_i_to__string(direct__8)
    printer__9(t86)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t88 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t88)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t91 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t91)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv94 string
    var t95 string = _goml_runtime_core_int32_to_string(self__6)
    retv94 = t95
    return retv94
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv97 string
    var t98 string = _goml_runtime_core_int32_to_string(self__43)
    retv97 = t98
    return retv97
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv100 string
    retv100 = self__38
    return retv100
}

func main() {
    main0()
}
