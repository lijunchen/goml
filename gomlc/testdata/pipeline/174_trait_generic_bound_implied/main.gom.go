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
    var t158 string
    var inline179 string = _goml_runtime_core_int32_to_string(self__0)
    t158 = inline179
    var t159 string = "marked:" + t158
    return t159
}

func _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(self__1 NumberBox) int32 {
    var t162 int32 = self__1.value
    return t162
}

func main0() struct{} {
    var t164 NumberBox = NumberBox{
        value: 7,
    }
    var t165 string
    var inline184 int32 = _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(t164)
    var inline185 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(inline184)
    t165 = inline185
    var inline181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t165)
    _goml_runtime_core_string_println(inline181)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
