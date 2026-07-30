package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Value struct {}

func _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Value_i_convert(self__0 Value) string {
    var retv111 string
    retv111 = "int"
    return retv111
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Value_i_convert(self__1 Value) int32 {
    var retv113 int32
    retv113 = 7
    return retv113
}

func main0() struct{} {
    var t115 Value = Value{}
    var text__2 string = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Value_i_convert(t115)
    var t116 Value = Value{}
    var number__3 int32 = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Value_i_convert(t116)
    println__T_string(text__2)
    println__T_int32(number__3)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t118 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t118)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t121 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t121)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv124 string
    retv124 = self__38
    return retv124
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv126 string
    var t127 string = _goml_runtime_core_int32_to_string(self__43)
    retv126 = t127
    return retv126
}

func main() {
    main0()
}
