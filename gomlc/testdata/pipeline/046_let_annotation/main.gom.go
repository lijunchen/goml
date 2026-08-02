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
    var inline188 string = "int32: "
    var inline189 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline188)
    _goml_runtime_core_string_print(inline189)
    var inline185 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x__0)
    _goml_runtime_core_string_println(inline185)
    var inline181 string = "int8: "
    var inline182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline181)
    _goml_runtime_core_string_print(inline182)
    var inline178 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(y__1)
    _goml_runtime_core_string_println(inline178)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t173 string = _goml_runtime_core_int32_to_string(self__43)
    return t173
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__41 int8) string {
    var t176 string = _goml_runtime_core_int8_to_string(self__41)
    return t176
}

func main() {
    main0()
}
