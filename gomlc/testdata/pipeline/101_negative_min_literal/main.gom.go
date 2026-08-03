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
    var t139 string
    var inline161 string = _goml_runtime_core_int8_to_string(a__0)
    t139 = inline161
    var inline158 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t139)
    _goml_runtime_core_string_println(inline158)
    var b__1 int16 = -32768
    var t140 string
    var inline156 string = _goml_runtime_core_int16_to_string(b__1)
    t140 = inline156
    var inline153 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t140)
    _goml_runtime_core_string_println(inline153)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
