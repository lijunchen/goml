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
    var t7 int32 = x__0 + 1
    x__0 = t7
    var t8 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__0)
    println__T_string(t8)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t10 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t10)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv13 string
    var t14 string = _goml_runtime_core_int32_to_string(self__2)
    retv13 = t14
    return retv13
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv16 string
    retv16 = self__9
    return retv16
}

func main() {
    main0()
}
