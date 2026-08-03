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
    var t180 string
    var inline201 string = _goml_runtime_core_int32_to_string(self__0)
    t180 = inline201
    var t181 string = "marked:" + t180
    return t181
}

func _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(self__1 NumberBox) int32 {
    var t184 int32 = self__1.value
    return t184
}

func main0() struct{} {
    var t186 NumberBox = NumberBox{
        value: 7,
    }
    var t187 string
    var inline206 int32 = _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(t186)
    var inline207 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(inline206)
    t187 = inline207
    var inline203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline203)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
