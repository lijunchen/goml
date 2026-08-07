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
    var inline205 string = "int32: "
    var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline205)
    _goml_runtime_core_string_print(inline206)
    var inline202 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x__0)
    _goml_runtime_core_string_println(inline202)
    var inline198 string = "int8: "
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline198)
    _goml_runtime_core_string_print(inline199)
    var inline195 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(y__1)
    _goml_runtime_core_string_println(inline195)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t190 string = _goml_runtime_core_int32_to_string(self__72)
    return t190
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__70 int8) string {
    var t193 string = _goml_runtime_core_int8_to_string(self__70)
    return t193
}

func main() {
    main0()
}
