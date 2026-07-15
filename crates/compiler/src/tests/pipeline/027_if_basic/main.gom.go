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

func choose(flag__0 bool, x__1 int32, y__2 int32) int32 {
    var retv25 int32
    var jp27 int32
    if flag__0 {
        jp27 = x__1
    } else {
        jp27 = y__2
    }
    retv25 = jp27
    return retv25
}

func main0() struct{} {
    var yes__3 int32 = choose(true, 10, 99)
    var no__4 int32 = choose(false, 10, 99)
    var t29 string = _goml_m_inherent_i_int32_i_int32_i_to__string(yes__3)
    var t30 string = "yes=" + t29
    println__T_string(t30)
    var t31 string = _goml_m_inherent_i_int32_i_int32_i_to__string(no__4)
    var t32 string = "no=" + t31
    println__T_string(t32)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t34 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t34)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv37 string
    var t38 string = _goml_runtime_core_int32_to_string(self__2)
    retv37 = t38
    return retv37
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv40 string
    retv40 = self__9
    return retv40
}

func main() {
    main0()
}
