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
    var retv11 int32
    var t12 int32 = x__0 * 2
    retv11 = t12
    return retv11
}

func increment(x__1 int32) int32 {
    var retv14 int32
    var t15 int32 = x__1 + 1
    retv14 = t15
    return retv14
}

func chooser(flag__2 bool) func(int32) int32 {
    var retv17 func(int32) int32
    var jp19 func(int32) int32
    if flag__2 {
        jp19 = double
    } else {
        jp19 = increment
    }
    retv17 = jp19
    return retv17
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32 = [2]func(int32) int32{double, increment}
    var f__4 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 0)
    var g__5 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 1)
    var t21 int32 = f__4(10)
    var t22 int32 = g__5(t21)
    println__T_int32(t22)
    var chosen__6 func(int32) int32 = chooser(true)
    var applied__7 int32 = chosen__6(5)
    var t23 func(int32) int32 = chooser(false)
    var direct__8 int32 = t23(5)
    var printer__9 func(string) struct{} = println__T_string
    var t24 string = _goml_m_inherent_i_int32_i_int32_i_to__string(applied__7)
    printer__9(t24)
    var t25 string = _goml_m_inherent_i_int32_i_int32_i_to__string(direct__8)
    printer__9(t25)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t27 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t27)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t30 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t30)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv33 string
    var t34 string = _goml_runtime_core_int32_to_string(self__2)
    retv33 = t34
    return retv33
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv36 string
    var t37 string = _goml_runtime_core_int32_to_string(self__13)
    retv36 = t37
    return retv36
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv39 string
    retv39 = self__9
    return retv39
}

func main() {
    main0()
}
