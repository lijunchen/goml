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
    var retv111 int32
    var jp113 int32
    if flag__0 {
        jp113 = x__1
    } else {
        jp113 = y__2
    }
    retv111 = jp113
    return retv111
}

func main0() struct{} {
    var yes__3 int32 = choose(true, 10, 99)
    var no__4 int32 = choose(false, 10, 99)
    var t115 string = _goml_m_inherent_i_int32_i_int32_i_to__string(yes__3)
    var t116 string = "yes=" + t115
    println__T_string(t116)
    var t117 string = _goml_m_inherent_i_int32_i_int32_i_to__string(no__4)
    var t118 string = "no=" + t117
    println__T_string(t118)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t120 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t120)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv123 string
    var t124 string = _goml_runtime_core_int32_to_string(self__6)
    retv123 = t124
    return retv123
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv126 string
    retv126 = self__38
    return retv126
}

func main() {
    main0()
}
