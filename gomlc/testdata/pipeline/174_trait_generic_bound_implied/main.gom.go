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

type NumberBox struct {
    value int32
}

func _goml_m_trait__impl_i_Mark_i_int32_i_mark(self__0 int32) string {
    var retv110 string
    var t111 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    var t112 string = "marked:" + t111
    retv110 = t112
    return retv110
}

func _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(self__1 NumberBox) int32 {
    var retv114 int32
    var t115 int32 = self__1.value
    retv114 = t115
    return retv114
}

func main0() struct{} {
    var t117 NumberBox = NumberBox{
        value: 7,
    }
    var t118 string = describe__C_NumberBox__T_int32(t117)
    println__T_string(t118)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv120 string
    var t121 string = _goml_runtime_core_int32_to_string(self__6)
    retv120 = t121
    return retv120
}

func println__T_string(value__1 string) struct{} {
    var t123 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t123)
    return struct{}{}
}

func describe__C_NumberBox__T_int32(container__2 NumberBox) string {
    var retv126 string
    var t127 int32 = _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(container__2)
    var t128 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(t127)
    retv126 = t128
    return retv126
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv130 string
    retv130 = self__38
    return retv130
}

func main() {
    main0()
}
