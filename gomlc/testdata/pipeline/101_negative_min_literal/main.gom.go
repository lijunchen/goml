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
    var t67 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(a__0)
    println__T_string(t67)
    var b__1 int16 = -32768
    var t68 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(b__1)
    println__T_string(t68)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t70 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t70)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__41 int8) string {
    var retv73 string
    var t74 string = _goml_runtime_core_int8_to_string(self__41)
    retv73 = t74
    return retv73
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__42 int16) string {
    var retv76 string
    var t77 string = _goml_runtime_core_int16_to_string(self__42)
    retv76 = t77
    return retv76
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv79 string
    retv79 = self__38
    return retv79
}

func main() {
    main0()
}
