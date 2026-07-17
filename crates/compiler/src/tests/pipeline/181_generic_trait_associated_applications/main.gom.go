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
    var retv61 string
    retv61 = "int"
    return retv61
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Value_i_convert(self__1 Value) int32 {
    var retv63 int32
    retv63 = 7
    return retv63
}

func main0() struct{} {
    var t65 Value = Value{}
    var text__2 string = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Value_i_convert(t65)
    var t66 Value = Value{}
    var number__3 int32 = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Value_i_convert(t66)
    println__T_string(text__2)
    println__T_int32(number__3)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t68 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t68)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t71 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t71)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv74 string
    retv74 = self__34
    return retv74
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv76 string
    var t77 string = _goml_runtime_core_int32_to_string(self__38)
    retv76 = t77
    return retv76
}

func main() {
    main0()
}
