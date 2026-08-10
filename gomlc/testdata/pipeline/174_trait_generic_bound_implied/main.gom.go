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
    var t175 string
    var inline196 string = _goml_runtime_core_int32_to_string(self__0)
    t175 = inline196
    var t176 string = "marked:" + t175
    return t176
}

func _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(self__1 NumberBox) int32 {
    var t179 int32 = self__1.value
    return t179
}

func main0() struct{} {
    var t181 NumberBox = NumberBox{
        value: 7,
    }
    var t182 string
    var inline201 int32 = _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(t181)
    var inline202 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(inline201)
    t182 = inline202
    var inline198 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline198)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
