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
    var retv64 int32
    var jp66 int32
    if flag__0 {
        jp66 = x__1
    } else {
        jp66 = y__2
    }
    retv64 = jp66
    return retv64
}

func main0() struct{} {
    var yes__3 int32 = choose(true, 10, 99)
    var no__4 int32 = choose(false, 10, 99)
    var t68 string = _goml_m_inherent_i_int32_i_int32_i_to__string(yes__3)
    var t69 string = "yes=" + t68
    println__T_string(t69)
    var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(no__4)
    var t71 string = "no=" + t70
    println__T_string(t71)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t73 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t73)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv76 string
    var t77 string = _goml_runtime_core_int32_to_string(self__5)
    retv76 = t77
    return retv76
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv79 string
    retv79 = self__37
    return retv79
}

func main() {
    main0()
}
