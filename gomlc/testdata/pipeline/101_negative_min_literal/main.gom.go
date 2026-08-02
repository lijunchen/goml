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
    var t158 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(a__0)
    println__T_string(t158)
    var b__1 int16 = -32768
    var t159 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(b__1)
    println__T_string(t159)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t161 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t161)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__41 int8) string {
    var t165 string = _goml_runtime_core_int8_to_string(self__41)
    return t165
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__42 int16) string {
    var t168 string = _goml_runtime_core_int16_to_string(self__42)
    return t168
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
