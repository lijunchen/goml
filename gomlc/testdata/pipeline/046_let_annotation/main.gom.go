package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var x__0 int32 = 1
    var y__1 int8 = 1
    var inline210 string = "int32: "
    var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline210)
    _goml_runtime_core_string_print(inline211)
    var inline207 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x__0)
    _goml_runtime_core_string_println(inline207)
    var inline203 string = "int8: "
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline203)
    _goml_runtime_core_string_print(inline204)
    var inline200 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(y__1)
    _goml_runtime_core_string_println(inline200)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t195 string = _goml_runtime_core_int32_to_string(self__72)
    return t195
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__70 int8) string {
    var t198 string = _goml_runtime_core_int8_to_string(self__70)
    return t198
}

func main() {
    main0()
}
