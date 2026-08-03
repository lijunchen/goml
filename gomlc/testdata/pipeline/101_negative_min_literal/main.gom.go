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
    var t180 string
    var inline202 string = _goml_runtime_core_int8_to_string(a__0)
    t180 = inline202
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t180)
    _goml_runtime_core_string_println(inline199)
    var b__1 int16 = -32768
    var t181 string
    var inline197 string = _goml_runtime_core_int16_to_string(b__1)
    t181 = inline197
    var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t181)
    _goml_runtime_core_string_println(inline194)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
