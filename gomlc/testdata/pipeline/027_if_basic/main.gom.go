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
    var retv158 int32
    var jp160 int32
    if flag__0 {
        jp160 = x__1
    } else {
        jp160 = y__2
    }
    retv158 = jp160
    return retv158
}

func main0() struct{} {
    var yes__3 int32 = choose(true, 10, 99)
    var no__4 int32 = choose(false, 10, 99)
    var t162 string = _goml_m_inherent_i_int32_i_int32_i_to__string(yes__3)
    var t163 string = "yes=" + t162
    println__T_string(t163)
    var t164 string = _goml_m_inherent_i_int32_i_int32_i_to__string(no__4)
    var t165 string = "no=" + t164
    println__T_string(t165)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t167 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t167)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv170 string
    var t171 string = _goml_runtime_core_int32_to_string(self__6)
    retv170 = t171
    return retv170
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv173 string
    retv173 = self__38
    return retv173
}

func main() {
    main0()
}
