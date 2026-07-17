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
    var retv64 string
    retv64 = "int"
    return retv64
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Value_i_convert(self__1 Value) int32 {
    var retv66 int32
    retv66 = 7
    return retv66
}

func main0() struct{} {
    var t68 Value = Value{}
    var text__2 string = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Value_i_convert(t68)
    var t69 Value = Value{}
    var number__3 int32 = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Value_i_convert(t69)
    println__T_string(text__2)
    println__T_int32(number__3)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t71 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t71)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t74 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t74)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv77 string
    retv77 = self__37
    return retv77
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv79 string
    var t80 string = _goml_runtime_core_int32_to_string(self__41)
    retv79 = t80
    return retv79
}

func main() {
    main0()
}
