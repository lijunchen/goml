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
    var t61 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(a__0)
    println__T_string(t61)
    var b__1 int16 = -32768
    var t62 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(b__1)
    println__T_string(t62)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t64 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t64)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__36 int8) string {
    var retv67 string
    var t68 string = _goml_runtime_core_int8_to_string(self__36)
    retv67 = t68
    return retv67
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__37 int16) string {
    var retv70 string
    var t71 string = _goml_runtime_core_int16_to_string(self__37)
    retv70 = t71
    return retv70
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv73 string
    retv73 = self__34
    return retv73
}

func main() {
    main0()
}
