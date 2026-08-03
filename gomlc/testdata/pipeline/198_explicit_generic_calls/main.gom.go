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
    var t138 string
    var inline165 string = "direct"
    t138 = inline165
    var inline162 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t138)
    _goml_runtime_core_string_println(inline162)
    var t139 int32
    var inline160 int32 = 42
    t139 = inline160
    var t140 string
    var inline158 string = _goml_runtime_core_int32_to_string(t139)
    t140 = inline158
    var inline155 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t140)
    _goml_runtime_core_string_println(inline155)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
