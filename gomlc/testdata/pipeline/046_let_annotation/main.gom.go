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
    var inline215 string = "int32: "
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline215)
    _goml_runtime_core_string_print(inline216)
    var inline212 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x__0)
    _goml_runtime_core_string_println(inline212)
    var inline208 string = "int8: "
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline208)
    _goml_runtime_core_string_print(inline209)
    var inline205 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(y__1)
    _goml_runtime_core_string_println(inline205)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t200 string = _goml_runtime_core_int32_to_string(self__70)
    return t200
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__68 int8) string {
    var t203 string = _goml_runtime_core_int8_to_string(self__68)
    return t203
}

func main() {
    main0()
}
