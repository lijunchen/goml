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
    var retv8 int32
    var t9 int32 = x__0 * 2
    retv8 = t9
    return retv8
}

func increment(x__1 int32) int32 {
    var retv11 int32
    var t12 int32 = x__1 + 1
    retv11 = t12
    return retv11
}

func chooser(flag__2 bool) func(int32) int32 {
    var retv14 func(int32) int32
    var jp16 func(int32) int32
    if flag__2 {
        jp16 = double
    } else {
        jp16 = increment
    }
    retv14 = jp16
    return retv14
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32 = [2]func(int32) int32{double, increment}
    var f__4 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 0)
    var g__5 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 1)
    var t18 int32 = f__4(10)
    var t19 int32 = g__5(t18)
    println__T_int32(t19)
    var chosen__6 func(int32) int32 = chooser(true)
    var applied__7 int32 = chosen__6(5)
    var t20 func(int32) int32 = chooser(false)
    var direct__8 int32 = t20(5)
    var printer__9 func(string) struct{} = println__T_string
    var t21 string = _goml_m_inherent_i_int32_i_int32_i_to__string(applied__7)
    printer__9(t21)
    var t22 string = _goml_m_inherent_i_int32_i_int32_i_to__string(direct__8)
    printer__9(t22)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t24 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t24)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t27 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t27)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv30 string
    var t31 string = _goml_runtime_core_int32_to_string(self__2)
    retv30 = t31
    return retv30
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv33 string
    var t34 string = _goml_runtime_core_int32_to_string(self__13)
    retv33 = t34
    return retv33
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv36 string
    retv36 = self__9
    return retv36
}

func main() {
    main0()
}
