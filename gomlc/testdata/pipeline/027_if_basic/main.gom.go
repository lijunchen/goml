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
    var retv71 int32
    var jp73 int32
    if flag__0 {
        jp73 = x__1
    } else {
        jp73 = y__2
    }
    retv71 = jp73
    return retv71
}

func main0() struct{} {
    var yes__3 int32 = choose(true, 10, 99)
    var no__4 int32 = choose(false, 10, 99)
    var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(yes__3)
    var t76 string = "yes=" + t75
    println__T_string(t76)
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(no__4)
    var t78 string = "no=" + t77
    println__T_string(t78)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t80 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t80)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv83 string
    var t84 string = _goml_runtime_core_int32_to_string(self__6)
    retv83 = t84
    return retv83
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv86 string
    retv86 = self__38
    return retv86
}

func main() {
    main0()
}
