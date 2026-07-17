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

func main0() struct{} {
    var x__0 int32 = 1
    var t64 int32 = x__0 + 1
    x__0 = t64
    var t65 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__0)
    println__T_string(t65)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t67 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t67)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv70 string
    var t71 string = _goml_runtime_core_int32_to_string(self__5)
    retv70 = t71
    return retv70
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv73 string
    retv73 = self__37
    return retv73
}

func main() {
    main0()
}
