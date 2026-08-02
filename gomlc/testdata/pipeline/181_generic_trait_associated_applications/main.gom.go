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
    return "int"
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Value_i_convert(self__1 Value) int32 {
    return 7
}

func main0() struct{} {
    var t162 Value = Value{}
    var text__2 string = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Value_i_convert(t162)
    var t163 Value = Value{}
    var number__3 int32 = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Value_i_convert(t163)
    println__T_string(text__2)
    println__T_int32(number__3)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t165)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t168 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t168)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t174 string = _goml_runtime_core_int32_to_string(self__43)
    return t174
}

func main() {
    main0()
}
