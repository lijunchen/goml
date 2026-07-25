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
    var retv67 int32
    var jp69 int32
    if flag__0 {
        jp69 = x__1
    } else {
        jp69 = y__2
    }
    retv67 = jp69
    return retv67
}

func main0() struct{} {
    var yes__3 int32 = choose(true, 10, 99)
    var no__4 int32 = choose(false, 10, 99)
    var t71 string = _goml_m_inherent_i_int32_i_int32_i_to__string(yes__3)
    var t72 string = "yes=" + t71
    println__T_string(t72)
    var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(no__4)
    var t74 string = "no=" + t73
    println__T_string(t74)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t76 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t76)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv79 string
    var t80 string = _goml_runtime_core_int32_to_string(self__6)
    retv79 = t80
    return retv79
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv82 string
    retv82 = self__38
    return retv82
}

func main() {
    main0()
}
