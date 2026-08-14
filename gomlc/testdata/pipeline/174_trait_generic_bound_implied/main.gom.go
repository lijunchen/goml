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
    var t185 string
    var inline206 string = _goml_runtime_core_int32_to_string(self__0)
    t185 = inline206
    var t186 string = "marked:" + t185
    return t186
}

func _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(self__1 NumberBox) int32 {
    var t189 int32 = self__1.value
    return t189
}

func main0() struct{} {
    var t191 NumberBox = NumberBox{
        value: 7,
    }
    var t192 string
    var inline211 int32 = _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(t191)
    var inline212 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(inline211)
    t192 = inline212
    var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline208)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
