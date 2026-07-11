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
    var retv26 int32
    var t27 int32 = x__0 * 2
    retv26 = t27
    return retv26
}

func increment(x__1 int32) int32 {
    var retv29 int32
    var t30 int32 = x__1 + 1
    retv29 = t30
    return retv29
}

func chooser(flag__2 bool) func(int32) int32 {
    var retv32 func(int32) int32
    var jp34 func(int32) int32
    if flag__2 {
        jp34 = double
    } else {
        jp34 = increment
    }
    retv32 = jp34
    return retv32
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32 = [2]func(int32) int32{double, increment}
    var f__4 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 0)
    var g__5 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 1)
    var t36 int32 = f__4(10)
    var t37 int32 = g__5(t36)
    println__T_int32(t37)
    var chosen__6 func(int32) int32 = chooser(true)
    var applied__7 int32 = chosen__6(5)
    var t38 func(int32) int32 = chooser(false)
    var direct__8 int32 = t38(5)
    var printer__9 func(string) struct{} = println__T_string
    var t39 string = _goml_m_inherent_i_int32_i_int32_i_to__string(applied__7)
    printer__9(t39)
    var t40 string = _goml_m_inherent_i_int32_i_int32_i_to__string(direct__8)
    printer__9(t40)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t42 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t42)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t45 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t45)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv48 string
    var t49 string = _goml_runtime_core_int32_to_string(self__2)
    retv48 = t49
    return retv48
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv51 string
    var t52 string = _goml_runtime_core_int32_to_string(self__13)
    retv51 = t52
    return retv51
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv54 string
    retv54 = self__9
    return retv54
}

func main() {
    main0()
}
