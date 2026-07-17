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
    var t61 int32 = x__0 + 1
    x__0 = t61
    var t62 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__0)
    println__T_string(t62)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t64 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t64)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv67 string
    var t68 string = _goml_runtime_core_int32_to_string(self__2)
    retv67 = t68
    return retv67
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv70 string
    retv70 = self__34
    return retv70
}

func main() {
    main0()
}
