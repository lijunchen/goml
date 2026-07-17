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

func array_get__Array_2_20Fn1_5int32_to_5int32(arr [2]func(int32) int32, index int32) func(int32) int32 {
    return arr[index]
}

func double(x__0 int32) int32 {
    var retv62 int32
    var t63 int32 = x__0 * 2
    retv62 = t63
    return retv62
}

func increment(x__1 int32) int32 {
    var retv65 int32
    var t66 int32 = x__1 + 1
    retv65 = t66
    return retv65
}

func chooser(flag__2 bool) func(int32) int32 {
    var retv68 func(int32) int32
    var jp70 func(int32) int32
    if flag__2 {
        jp70 = double
    } else {
        jp70 = increment
    }
    retv68 = jp70
    return retv68
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32 = [2]func(int32) int32{double, increment}
    var f__4 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 0)
    var g__5 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 1)
    var t72 int32 = f__4(10)
    var t73 int32 = g__5(t72)
    println__T_int32(t73)
    var chosen__6 func(int32) int32 = chooser(true)
    var applied__7 int32 = chosen__6(5)
    var t74 func(int32) int32 = chooser(false)
    var direct__8 int32 = t74(5)
    var printer__9 func(string) struct{} = println__T_string
    var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(applied__7)
    printer__9(t75)
    var t76 string = _goml_m_inherent_i_int32_i_int32_i_to__string(direct__8)
    printer__9(t76)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t78 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t78)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t81 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t81)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv84 string
    var t85 string = _goml_runtime_core_int32_to_string(self__2)
    retv84 = t85
    return retv84
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv87 string
    var t88 string = _goml_runtime_core_int32_to_string(self__38)
    retv87 = t88
    return retv87
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv90 string
    retv90 = self__34
    return retv90
}

func main() {
    main0()
}
