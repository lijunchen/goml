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
    var inline220 string = "int32: "
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline220)
    _goml_runtime_core_string_print(inline221)
    var inline217 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x__0)
    _goml_runtime_core_string_println(inline217)
    var inline213 string = "int8: "
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline213)
    _goml_runtime_core_string_print(inline214)
    var inline210 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(y__1)
    _goml_runtime_core_string_println(inline210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t205 string = _goml_runtime_core_int32_to_string(self__70)
    return t205
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__68 int8) string {
    var t208 string = _goml_runtime_core_int8_to_string(self__68)
    return t208
}

func main() {
    main0()
}
