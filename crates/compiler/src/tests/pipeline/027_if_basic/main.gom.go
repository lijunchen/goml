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
    var retv10 int32
    var jp12 int32
    if flag__0 {
        jp12 = x__1
    } else {
        jp12 = y__2
    }
    retv10 = jp12
    return retv10
}

func main0() struct{} {
    var yes__3 int32 = choose(true, 10, 99)
    var no__4 int32 = choose(false, 10, 99)
    var t14 string = _goml_m_inherent_i_int32_i_int32_i_to__string(yes__3)
    var t15 string = "yes=" + t14
    println__T_string(t15)
    var t16 string = _goml_m_inherent_i_int32_i_int32_i_to__string(no__4)
    var t17 string = "no=" + t16
    println__T_string(t17)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t19 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t19)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv22 string
    var t23 string = _goml_runtime_core_int32_to_string(self__2)
    retv22 = t23
    return retv22
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv25 string
    retv25 = self__9
    return retv25
}

func main() {
    main0()
}
