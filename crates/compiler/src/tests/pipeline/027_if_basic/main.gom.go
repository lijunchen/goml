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
    var retv61 int32
    var jp63 int32
    if flag__0 {
        jp63 = x__1
    } else {
        jp63 = y__2
    }
    retv61 = jp63
    return retv61
}

func main0() struct{} {
    var yes__3 int32 = choose(true, 10, 99)
    var no__4 int32 = choose(false, 10, 99)
    var t65 string = _goml_m_inherent_i_int32_i_int32_i_to__string(yes__3)
    var t66 string = "yes=" + t65
    println__T_string(t66)
    var t67 string = _goml_m_inherent_i_int32_i_int32_i_to__string(no__4)
    var t68 string = "no=" + t67
    println__T_string(t68)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t70 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t70)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv73 string
    var t74 string = _goml_runtime_core_int32_to_string(self__2)
    retv73 = t74
    return retv73
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv76 string
    retv76 = self__34
    return retv76
}

func main() {
    main0()
}
