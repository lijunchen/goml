package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int16_to_string(x int16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var a__0 int8 = -128
    var t185 string
    var inline207 string = _goml_runtime_core_int8_to_string(a__0)
    t185 = inline207
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t185)
    _goml_runtime_core_string_println(inline204)
    var b__1 int16 = -32768
    var t186 string
    var inline202 string = _goml_runtime_core_int16_to_string(b__1)
    t186 = inline202
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline199)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
