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
    var retv25 string
    retv25 = "int"
    return retv25
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Value_i_convert(self__1 Value) int32 {
    var retv27 int32
    retv27 = 7
    return retv27
}

func main0() struct{} {
    var t29 Value = Value{}
    var text__2 string = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Value_i_convert(t29)
    var t30 Value = Value{}
    var number__3 int32 = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Value_i_convert(t30)
    println__T_string(text__2)
    println__T_int32(number__3)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t32 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t32)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t35 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t35)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv38 string
    retv38 = self__9
    return retv38
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv40 string
    var t41 string = _goml_runtime_core_int32_to_string(self__13)
    retv40 = t41
    return retv40
}

func main() {
    main0()
}
