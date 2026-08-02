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
    var retv157 string
    var t158 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    var t159 string = "marked:" + t158
    retv157 = t159
    return retv157
}

func _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(self__1 NumberBox) int32 {
    var retv161 int32
    var t162 int32 = self__1.value
    retv161 = t162
    return retv161
}

func main0() struct{} {
    var t164 NumberBox = NumberBox{
        value: 7,
    }
    var t165 string = describe__C_NumberBox__T_int32(t164)
    println__T_string(t165)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv167 string
    var t168 string = _goml_runtime_core_int32_to_string(self__6)
    retv167 = t168
    return retv167
}

func println__T_string(value__1 string) struct{} {
    var t170 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t170)
    return struct{}{}
}

func describe__C_NumberBox__T_int32(container__2 NumberBox) string {
    var retv173 string
    var t174 int32 = _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(container__2)
    var t175 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(t174)
    retv173 = t175
    return retv173
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv177 string
    retv177 = self__38
    return retv177
}

func main() {
    main0()
}
