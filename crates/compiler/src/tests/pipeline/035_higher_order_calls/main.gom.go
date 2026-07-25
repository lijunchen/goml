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
    var retv68 int32
    var t69 int32 = x__0 * 2
    retv68 = t69
    return retv68
}

func increment(x__1 int32) int32 {
    var retv71 int32
    var t72 int32 = x__1 + 1
    retv71 = t72
    return retv71
}

func chooser(flag__2 bool) func(int32) int32 {
    var retv74 func(int32) int32
    var jp76 func(int32) int32
    if flag__2 {
        jp76 = double
    } else {
        jp76 = increment
    }
    retv74 = jp76
    return retv74
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32 = [2]func(int32) int32{double, increment}
    var f__4 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 0)
    var g__5 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 1)
    var t78 int32 = f__4(10)
    var t79 int32 = g__5(t78)
    println__T_int32(t79)
    var chosen__6 func(int32) int32 = chooser(true)
    var applied__7 int32 = chosen__6(5)
    var t80 func(int32) int32 = chooser(false)
    var direct__8 int32 = t80(5)
    var printer__9 func(string) struct{} = println__T_string
    var t81 string = _goml_m_inherent_i_int32_i_int32_i_to__string(applied__7)
    printer__9(t81)
    var t82 string = _goml_m_inherent_i_int32_i_int32_i_to__string(direct__8)
    printer__9(t82)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t84 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t84)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t87 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t87)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv90 string
    var t91 string = _goml_runtime_core_int32_to_string(self__6)
    retv90 = t91
    return retv90
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv93 string
    var t94 string = _goml_runtime_core_int32_to_string(self__43)
    retv93 = t94
    return retv93
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv96 string
    retv96 = self__38
    return retv96
}

func main() {
    main0()
}
