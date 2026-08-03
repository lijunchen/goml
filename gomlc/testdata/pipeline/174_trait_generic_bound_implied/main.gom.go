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
    var t139 string
    var inline160 string = _goml_runtime_core_int32_to_string(self__0)
    t139 = inline160
    var t140 string = "marked:" + t139
    return t140
}

func _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(self__1 NumberBox) int32 {
    var t143 int32 = self__1.value
    return t143
}

func main0() struct{} {
    var t145 NumberBox = NumberBox{
        value: 7,
    }
    var t146 string
    var inline165 int32 = _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(t145)
    var inline166 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(inline165)
    t146 = inline166
    var inline162 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t146)
    _goml_runtime_core_string_println(inline162)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
