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
    var retv154 string
    var t155 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    var t156 string = "marked:" + t155
    retv154 = t156
    return retv154
}

func _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(self__1 NumberBox) int32 {
    var retv158 int32
    var t159 int32 = self__1.value
    retv158 = t159
    return retv158
}

func main0() struct{} {
    var t161 NumberBox = NumberBox{
        value: 7,
    }
    var t162 string = describe__C_NumberBox__T_int32(t161)
    println__T_string(t162)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv164 string
    var t165 string = _goml_runtime_core_int32_to_string(self__6)
    retv164 = t165
    return retv164
}

func println__T_string(value__1 string) struct{} {
    var t167 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t167)
    return struct{}{}
}

func describe__C_NumberBox__T_int32(container__2 NumberBox) string {
    var retv170 string
    var t171 int32 = _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(container__2)
    var t172 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(t171)
    retv170 = t172
    return retv170
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv174 string
    retv174 = self__38
    return retv174
}

func main() {
    main0()
}
