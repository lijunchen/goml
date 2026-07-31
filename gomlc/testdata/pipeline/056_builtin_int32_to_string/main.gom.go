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
    var value__0 int32 = 42
    var text__1 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__0)
    println__T_string(text__1)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv155 string
    var t156 string = _goml_runtime_core_int32_to_string(self__6)
    retv155 = t156
    return retv155
}

func println__T_string(value__1 string) struct{} {
    var t158 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t158)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv161 string
    retv161 = self__38
    return retv161
}

func main() {
    main0()
}
