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
    var retv155 int32
    var jp157 int32
    if flag__0 {
        jp157 = x__1
    } else {
        jp157 = y__2
    }
    retv155 = jp157
    return retv155
}

func main0() struct{} {
    var yes__3 int32 = choose(true, 10, 99)
    var no__4 int32 = choose(false, 10, 99)
    var t159 string = _goml_m_inherent_i_int32_i_int32_i_to__string(yes__3)
    var t160 string = "yes=" + t159
    println__T_string(t160)
    var t161 string = _goml_m_inherent_i_int32_i_int32_i_to__string(no__4)
    var t162 string = "no=" + t161
    println__T_string(t162)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t164 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t164)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv167 string
    var t168 string = _goml_runtime_core_int32_to_string(self__6)
    retv167 = t168
    return retv167
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv170 string
    retv170 = self__38
    return retv170
}

func main() {
    main0()
}
